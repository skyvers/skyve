package org.skyve.impl.sail.execution;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;

import org.junit.jupiter.api.Test;

/**
 * Tests that run tokens are unique in the JVM, so that runs started at once (a load test's virtual users) never share
 * {@code {run}} data.
 */
@SuppressWarnings("static-method")
class RunStateTest {
	private static long time(RunState run) {
		return Long.parseLong(run.getRunToken(), Character.MAX_RADIX);
	}

	@Test
	void aRunInTheSameMillisecondAsTheLastTakesTheNextMillisecond() {
		long last = time(new RunState());

		RunState same = new RunState(last);
		RunState sameAgain = new RunState(last);
		RunState later = new RunState(last + 100L);

		assertAll(() -> assertEquals(last + 1L, time(same)),
					() -> assertEquals(last + 2L, time(sameAgain)),
					() -> assertEquals(last + 100L, time(later), "a later time is its own token"),
					() -> assertEquals(Long.toString(last + 100L, Character.MAX_RADIX), later.getRunToken()));
	}

	@Test
	void anEarlierTimeThanTheLastTokenStillGetsANewToken() {
		long last = time(new RunState());

		RunState earlier = new RunState(last - 1_000L);

		assertEquals(last + 1L, time(earlier));
	}

	@Test
	void runsStartedTogetherOnManyThreadsGetDifferentTokens() throws Exception {
		int threads = 8;
		int perThread = 500;
		Set<String> tokens = ConcurrentHashMap.newKeySet();
		CountDownLatch start = new CountDownLatch(1);
		ExecutorService executor = Executors.newFixedThreadPool(threads);
		try {
			List<Future<?>> futures = new ArrayList<>(threads);
			for (int t = 0; t < threads; t++) {
				futures.add(executor.submit(() -> {
					start.await(); // every thread starts making runs at once
					for (int i = 0; i < perThread; i++) {
						tokens.add(new RunState().getRunToken());
					}
					return null;
				}));
			}
			start.countDown();
			for (Future<?> future : futures) {
				future.get();
			}
		}
		finally {
			executor.shutdownNow();
		}

		assertEquals(threads * perThread, tokens.size(), "every run has its own token");
		assertTrue(tokens.stream().allMatch(token -> token.matches("[0-9a-z]{8,}")), "tokens are lower-case base 36");
	}
}
