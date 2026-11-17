package org.skyve.impl.cache;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.stream.Stream;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.api.Timeout.ThreadMode;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;
import org.skyve.cache.CSRFTokenCacheConfig;
import org.skyve.cache.ConversationCacheConfig;
import org.skyve.cache.GeoIPCacheConfig;
import org.skyve.cache.SessionCacheConfig;
import org.skyve.impl.util.UtilImpl;

import jakarta.servlet.http.HttpSession;

/**
 * Proves that {@link StateUtil}'s per-user session sets and per-session CSRF token sets stay exact
 * when many threads change one entry at the same moment, against the real Ehcache caches.
 *
 * <p>Each round parks every worker on a {@link CountDownLatch} and releases them together, so
 * the read-modify-write window of each call overlaps the others'. Every round uses a fresh key,
 * and each test runs {@link #ROUNDS} rounds, so a lost update in any round fails the test.
 * Both cache topologies Skyve configures are covered: heap only, and heap with off-heap and
 * disk tiers (skyve.json's default for the session and token caches).
 * <p>
 * A set changed by racing threads can be corrupted so that a worker loops forever while it holds a cache lock,
 * and then every later cache call blocks. So each test and the clean-up have a timeout on a separate thread, and
 * once a worker is known to be stuck the later tests fail at once instead of blocking on the same lock.
 */
@Timeout(value = 60, unit = TimeUnit.SECONDS, threadMode = ThreadMode.SEPARATE_THREAD)
class StateUtilConcurrencyTest {
	private static final int THREADS = 8;
	private static final int ROUNDS = 200;
	/**
	 * Changes per session count writer: on the old code 10 000 showed a negative count in 30 of 30 runs (about
	 * 0.1 s each), 5 000 in 16 of 20.
	 */
	private static final int SESSION_COUNT_CHANGES = 10_000;
	/** Set when a worker did not finish: it may hold a cache lock for good. */
	private static volatile boolean stuckWorker = false;

	/**
	 * The cache topologies Skyve builds in {@link DefaultCaching#createEHCache}.
	 */
	enum Topology {
		/** Heap tier only, with a non-persistent cache manager (values held by reference). */
		HEAP,
		/** Heap, off-heap and disk tiers with a persistent cache manager (values serialised below the heap). */
		TIERED
	}

	private boolean originalForceNonPersistentCaching;
	private String originalCacheDirectory;
	private SessionCacheConfig originalSessionCache;
	private CSRFTokenCacheConfig originalCsrfTokenCache;
	private ConversationCacheConfig originalConversationCache;
	private GeoIPCacheConfig originalGeoIPCache;
	private Path cacheDirectory;
	private ExecutorService executor;

	@BeforeEach
	void before() throws Exception {
		originalForceNonPersistentCaching = UtilImpl.FORCE_NON_PERSISTENT_CACHING;
		originalCacheDirectory = UtilImpl.CACHE_DIRECTORY;
		originalSessionCache = UtilImpl.SESSION_CACHE;
		originalCsrfTokenCache = UtilImpl.CSRF_TOKEN_CACHE;
		originalConversationCache = UtilImpl.CONVERSATION_CACHE;
		originalGeoIPCache = UtilImpl.GEO_IP_CACHE;
		cacheDirectory = Files.createTempDirectory("skyve-stateutil-concurrency");
		// enough threads for the largest round (3 tasks per thread index); daemon, so a worker spinning in a
		// TreeSet corrupted by a racing update cannot keep the test JVM alive
		executor = Executors.newFixedThreadPool(THREADS * 3, r -> {
			Thread result = new Thread(r, "state-util-concurrency");
			result.setDaemon(true);
			return result;
		});
	}

	@AfterEach
	@Timeout(value = 20, unit = TimeUnit.SECONDS, threadMode = ThreadMode.SEPARATE_THREAD)
	void after() throws Exception {
		executor.shutdownNow();
		if (! stuckWorker) {
			DefaultCaching.get().shutdown();
		}

		UtilImpl.FORCE_NON_PERSISTENT_CACHING = originalForceNonPersistentCaching;
		UtilImpl.CACHE_DIRECTORY = originalCacheDirectory;
		UtilImpl.SESSION_CACHE = originalSessionCache;
		UtilImpl.CSRF_TOKEN_CACHE = originalCsrfTokenCache;
		UtilImpl.CONVERSATION_CACHE = originalConversationCache;
		UtilImpl.GEO_IP_CACHE = originalGeoIPCache;

		try (Stream<Path> paths = Files.walk(cacheDirectory)) {
			paths.sorted((a, b) -> b.compareTo(a)).map(Path::toFile).forEach(File::delete);
		}
	}

	@ParameterizedTest
	@EnumSource(Topology.class)
	void concurrentSignInsOfOneUserKeepEverySessionId(Topology topology) throws Exception {
		start(topology);
		List<String> failures = new ArrayList<>();
		for (int round = 0; round < ROUNDS; round++) {
			String userId = "user-" + round;
			List<HttpSession> sessions = sessions("r" + round + "-s", THREADS);
			List<Runnable> tasks = new ArrayList<>();
			for (HttpSession session : sessions) {
				tasks.add(() -> StateUtil.addSession(userId, session));
			}
			runTogether(failures, round, tasks);
			checkSessions(failures, round, userId, ids(sessions));
		}
		assertTrue(failures.isEmpty(), () -> describe(failures));
	}

	@ParameterizedTest
	@EnumSource(Topology.class)
	void concurrentSignInsAndSignOutsOfOneUserLoseNothing(Topology topology) throws Exception {
		start(topology);
		List<String> failures = new ArrayList<>();
		for (int round = 0; round < ROUNDS; round++) {
			String userId = "user-" + round;
			List<HttpSession> ending = sessions("r" + round + "-old", THREADS);
			List<HttpSession> starting = sessions("r" + round + "-new", THREADS);
			for (HttpSession session : ending) {
				StateUtil.addSession(userId, session);
			}
			List<Runnable> tasks = new ArrayList<>();
			for (int i = 0; i < THREADS; i++) {
				HttpSession end = ending.get(i);
				HttpSession begin = starting.get(i);
				tasks.add(() -> StateUtil.removeSession(userId, end));
				tasks.add(() -> StateUtil.addSession(userId, begin));
			}
			runTogether(failures, round, tasks);
			checkSessions(failures, round, userId, ids(starting));
		}
		assertTrue(failures.isEmpty(), () -> describe(failures));
	}

	@ParameterizedTest
	@EnumSource(Topology.class)
	void concurrentShortSessionsOfOneUserLeaveNoSessionBehind(Topology topology) throws Exception {
		start(topology);
		List<String> failures = new ArrayList<>();
		for (int round = 0; round < ROUNDS; round++) {
			String userId = "user-" + round;
			List<Runnable> tasks = new ArrayList<>();
			for (HttpSession session : sessions("r" + round + "-s", THREADS)) {
				tasks.add(() -> {
					StateUtil.addSession(userId, session);
					StateUtil.removeSession(userId, session);
				});
			}
			runTogether(failures, round, tasks);
			checkSessions(failures, round, userId, new TreeSet<>());
		}
		assertTrue(failures.isEmpty(), () -> describe(failures));
	}

	/**
	 * Revoking every session (as a password change does) while the user's sessions start and end: no revoked
	 * session id may come back, which would leave a session valid after its revocation. Half of the revoked
	 * sessions also end on their own at the same moment; the other half can only go through the revocation.
	 */
	@ParameterizedTest
	@EnumSource(Topology.class)
	void concurrentRevocationOfAUsersSessionsLeavesNoRevokedSession(Topology topology) throws Exception {
		start(topology);
		List<String> failures = new ArrayList<>();
		for (int round = 0; round < ROUNDS; round++) {
			String userId = "user-" + round;
			List<HttpSession> revoked = sessions("r" + round + "-old", THREADS * 2);
			List<HttpSession> starting = sessions("r" + round + "-new", THREADS);
			for (HttpSession session : revoked) {
				StateUtil.addSession(userId, session);
			}
			List<Runnable> tasks = new ArrayList<>();
			tasks.add(() -> StateUtil.removeSessions(userId));
			for (int i = 0; i < THREADS; i++) {
				HttpSession end = revoked.get(i);
				HttpSession begin = starting.get(i);
				tasks.add(() -> StateUtil.removeSession(userId, end));
				tasks.add(() -> StateUtil.addSession(userId, begin));
			}
			runTogether(failures, round, tasks);
			checkNoRevokedSession(failures, round, userId, ids(starting));
		}
		assertTrue(failures.isEmpty(), () -> describe(failures));
	}

	/**
	 * Session count decrements at zero racing increments and reads: the count is never seen below zero. A
	 * decrement that went to -1 and was then reset to 0 showed -1 to readers, and lost any increment made
	 * in between; that window is what the readers catch.
	 * <p>
	 * It touches no cache, so it has its own bounds and never sets {@link #stuckWorker}: a slow machine fails
	 * this test alone. Each reader keeps its own minimum and returns it at the end.
	 */
	@Test
	void concurrentSessionCountChangesNeverShowANegativeCount() throws Exception {
		resetSessionCount();
		int writers = THREADS;
		int readers = THREADS / 2;
		CountDownLatch ready = new CountDownLatch(writers + readers);
		CountDownLatch go = new CountDownLatch(1);
		AtomicBoolean done = new AtomicBoolean();
		List<Future<?>> writing = new ArrayList<>();
		for (int i = 0; i < writers; i++) {
			// mostly decrements, so the count sits at zero, where the old decrement went below it
			boolean decrement = (i % 4 != 0);
			writing.add(executor.submit(() -> {
				ready.countDown();
				go.await();
				for (int j = 0; j < SESSION_COUNT_CHANGES; j++) {
					if (decrement) {
						StateUtil.decrementSessionCount();
					}
					else {
						StateUtil.incrementSessionCount();
					}
				}
				return null;
			}));
		}
		List<Future<Integer>> reading = new ArrayList<>();
		for (int i = 0; i < readers; i++) {
			reading.add(executor.submit(() -> {
				ready.countDown();
				go.await();
				int lowest = 0;
				while (! done.get()) {
					lowest = Math.min(lowest, StateUtil.getSessionCount());
				}
				return Integer.valueOf(lowest);
			}));
		}
		try {
			// every worker is parked on the go latch before it opens
			assertTrue(ready.await(30, TimeUnit.SECONDS), "workers did not start");
			go.countDown();
			for (Future<?> future : writing) {
				future.get(30, TimeUnit.SECONDS);
			}
		}
		finally {
			done.set(true);
		}
		int lowest = 0;
		for (Future<Integer> future : reading) {
			lowest = Math.min(lowest, future.get(30, TimeUnit.SECONDS).intValue());
		}
		resetSessionCount();
		int seen = lowest;
		assertTrue(seen >= 0, () -> "the session count was seen at " + seen);
	}

	private static void resetSessionCount() {
		while (StateUtil.getSessionCount() > 0) {
			StateUtil.decrementSessionCount();
		}
	}

	@ParameterizedTest
	@EnumSource(Topology.class)
	void concurrentTokenChangesInOneSessionKeepEveryToken(Topology topology) throws Exception {
		start(topology);
		List<String> failures = new ArrayList<>();
		for (int round = 0; round < ROUNDS; round++) {
			String sessionId = "session-" + round;
			int base = round * THREADS * 4;
			// tokens base .. base + 4T - 1: replaced (old -> new), removed, and added from nothing
			List<Runnable> tasks = new ArrayList<>();
			TreeSet<Integer> expected = new TreeSet<>();
			for (int i = 0; i < THREADS; i++) {
				Integer replaced = Integer.valueOf(base + i);
				Integer replacement = Integer.valueOf(base + THREADS + i);
				Integer removed = Integer.valueOf(base + (2 * THREADS) + i);
				Integer added = Integer.valueOf(base + (3 * THREADS) + i);
				StateUtil.replaceToken(sessionId, null, replaced);
				StateUtil.replaceToken(sessionId, null, removed);
				tasks.add(() -> StateUtil.replaceToken(sessionId, replaced, replacement));
				tasks.add(() -> StateUtil.removeToken(sessionId, removed));
				tasks.add(() -> StateUtil.replaceToken(sessionId, null, added));
				expected.add(replacement);
				expected.add(added);
			}
			runTogether(failures, round, tasks);
			checkTokens(failures, round, sessionId, expected);
		}
		assertTrue(failures.isEmpty(), () -> describe(failures));
	}

	/**
	 * Starts the caches in the given topology; the off-heap and disk sizes are small, but the tiers are real.
	 */
	private void start(Topology topology) {
		assertFalse(stuckWorker, "an earlier test left a worker stuck, maybe holding a cache lock");
		UtilImpl.CACHE_DIRECTORY = cacheDirectory.toString() + File.separator;
		if (topology == Topology.HEAP) {
			UtilImpl.FORCE_NON_PERSISTENT_CACHING = true;
			UtilImpl.SESSION_CACHE = new SessionCacheConfig(1000, 10);
			UtilImpl.CSRF_TOKEN_CACHE = new CSRFTokenCacheConfig(1000, 10);
		}
		else {
			UtilImpl.FORCE_NON_PERSISTENT_CACHING = false;
			// a heap smaller than the rounds' keys, so entries move to the lower tiers during the test
			UtilImpl.SESSION_CACHE = new SessionCacheConfig(50, 1, 10, 10);
			UtilImpl.CSRF_TOKEN_CACHE = new CSRFTokenCacheConfig(50, 1, 10, 10);
		}
		UtilImpl.CONVERSATION_CACHE = new ConversationCacheConfig(10, 10);
		UtilImpl.GEO_IP_CACHE = new GeoIPCacheConfig(10, 10);
		DefaultCaching.get().shutdown();
		DefaultCaching.get().startup();
	}

	/**
	 * Runs the tasks on their own threads, all released at the same moment, and waits for them all.
	 * A task's exception (a corrupted set or a failed cache call) is added to the round's failures;
	 * a task still running after 10 seconds (a corrupted set can loop forever) fails the test at once.
	 */
	private void runTogether(List<String> failures, int round, List<Runnable> tasks) throws Exception {
		CountDownLatch ready = new CountDownLatch(tasks.size());
		CountDownLatch go = new CountDownLatch(1);
		List<Future<?>> futures = new ArrayList<>();
		for (Runnable task : tasks) {
			futures.add(executor.submit(() -> {
				ready.countDown();
				go.await();
				task.run();
				return null;
			}));
		}
		// every worker is parked on the go latch before it opens
		assertTrue(ready.await(30, TimeUnit.SECONDS), "workers did not start");
		go.countDown();
		for (Future<?> future : futures) {
			try {
				future.get(10, TimeUnit.SECONDS);
			}
			catch (ExecutionException e) {
				failures.add("round " + round + ": a task threw " + e.getCause());
			}
			catch (TimeoutException e) {
				stuckWorker = true;
				failures.add("round " + round + ": a task did not finish in 10 seconds");
				fail(describe(failures), e);
			}
		}
	}

	private static void checkSessions(List<String> failures, int round, String userId, Set<String> expected) {
		try {
			Set<String> actual = new TreeSet<>(StateUtil.getSessions(userId));
			if (! expected.equals(actual)) {
				failures.add("round " + round + ": expected " + expected + " but was " + actual);
			}
			boolean registered = sessionsCache().containsKey(userId);
			if (registered == expected.isEmpty()) {
				failures.add("round " + round + ": user entry " + (registered ? "left behind" : "missing"));
			}
		}
		catch (RuntimeException e) { // a set corrupted by racing updates cannot be read
			failures.add("round " + round + ": reading the sessions threw " + e);
		}
	}

	/**
	 * Records a failure if the user has any session but the new ones; a new session may or may not
	 * survive, depending on whether it started before the revocation.
	 */
	private static void checkNoRevokedSession(List<String> failures, int round, String userId, Set<String> started) {
		try {
			Set<String> left = new TreeSet<>(StateUtil.getSessions(userId));
			Set<String> unexpected = new TreeSet<>(left);
			unexpected.removeAll(started);
			if (! unexpected.isEmpty()) {
				failures.add("round " + round + ": revoked sessions " + unexpected + " came back; left " + left);
			}
		}
		catch (RuntimeException e) { // a set corrupted by racing updates cannot be read
			failures.add("round " + round + ": reading the sessions threw " + e);
		}
	}

	private static void checkTokens(List<String> failures, int round, String sessionId, Set<Integer> expected) {
		try {
			Set<Integer> actual = tokensCache().get(sessionId);
			if (! expected.equals(actual)) {
				failures.add("round " + round + ": expected " + expected + " but was " + actual);
			}
		}
		catch (RuntimeException e) { // a set corrupted by racing updates cannot be read
			failures.add("round " + round + ": reading the tokens threw " + e);
		}
	}

	private static String describe(List<String> failures) {
		long rounds = failures.stream().map(f -> f.substring(0, f.indexOf(':'))).distinct().count();
		return rounds + " of " + ROUNDS + " rounds failed; first: "
				+ String.join("; ", failures.subList(0, Math.min(3, failures.size())));
	}

	@SuppressWarnings("rawtypes")
	private static org.ehcache.Cache<String, TreeSet> sessionsCache() {
		return DefaultCaching.get().getEHCache(UtilImpl.SESSION_CACHE.getName(), String.class, TreeSet.class);
	}

	@SuppressWarnings("rawtypes")
	private static org.ehcache.Cache<String, TreeSet> tokensCache() {
		return DefaultCaching.get().getEHCache(UtilImpl.CSRF_TOKEN_CACHE.getName(), String.class, TreeSet.class);
	}

	private static List<HttpSession> sessions(String prefix, int count) {
		List<HttpSession> result = new ArrayList<>();
		for (int i = 0; i < count; i++) {
			result.add(session(prefix + i));
		}
		return result;
	}

	private static Set<String> ids(List<HttpSession> sessions) {
		Set<String> result = new TreeSet<>();
		for (HttpSession session : sessions) {
			result.add(session.getId());
		}
		return result;
	}

	private static HttpSession session(String id) {
		HttpSession session = mock(HttpSession.class);
		when(session.getId()).thenReturn(id);
		return session;
	}
}
