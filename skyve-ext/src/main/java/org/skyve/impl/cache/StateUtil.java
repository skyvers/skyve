package org.skyve.impl.cache;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.Serializable;
import java.nio.charset.StandardCharsets;
import java.security.SecureRandom;
import java.util.Collections;
import java.util.Iterator;
import java.util.Locale;
import java.util.Set;
import java.util.TreeSet;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Predicate;
import java.util.zip.GZIPInputStream;
import java.util.zip.GZIPOutputStream;

import org.apache.commons.codec.binary.Base64;
import org.ehcache.Cache;
import org.ehcache.Cache.Entry;
import org.ehcache.core.statistics.CacheStatistics;
import org.ehcache.core.statistics.TierStatistics;
import org.hibernate.internal.util.SerializationHelper;
import org.skyve.EXT;
import org.skyve.cache.CacheTier;
import org.skyve.cache.Caching;
import org.skyve.domain.messages.ConversationEndedException;
import org.skyve.domain.messages.SessionEndedException;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.AbstractWebContext;
import org.skyve.util.IPGeolocation;
import org.skyve.util.logging.Category;
import org.skyve.web.WebContext;
import org.slf4j.Logger;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpSession;

/**
 * Manages serialised conversation (page-scope) state in the HTTP session,
 * providing store and retrieve operations for Faces view state and navigation data,
 * and keeps the per-user session id sets and per-session CSRF token sets.
 *
 * <p>Thread-safe: all public methods may be called concurrently without external synchronisation.
 * The session id and CSRF token sets are changed with Ehcache's atomic conditional operations
 * ({@code putIfAbsent}, {@code replace(key, old, new)}, {@code remove(key, old)}) on a new copy of the
 * set, so simultaneous changes to one user's sessions or one session's tokens are never lost, and a
 * set read from the cache is never changed afterwards.
 */
public class StateUtil {

    private static final Logger FACES_LOGGER = Category.FACES.logger();

	private StateUtil() {
		// Disallow instantiation.
	}

	private static @Nonnull Cache<String, byte[]> getConversations() {
		return EXT.getCaching().getEHCache(UtilImpl.CONVERSATION_CACHE.getName(), String.class, byte[].class);
	}

	/**
	 * Commit an active conversation transaction if it exists without closing its persistence
	 * context and then cache the conversation.
	 *
	 * <p>This low-level operation does not begin a replacement transaction. Public
	 * continuation APIs such as {@link WebContext#cacheConversationAndCycleTransaction()}
	 * do that after the serialised checkpoint has been written.
	 *
	 * @param webContext the conversation to checkpoint and cache
	 */
	public static void commitAndCacheConversation(@Nonnull WebContext webContext) {
		if (webContext instanceof AbstractWebContext context) {
			AbstractPersistence persistence = context.getConversation();
			if (persistence != null) {
				persistence.commit(false);
			}
		}

		// Note that EHCache puts are thread-safe
		getConversations().put(webContext.getKey(), SerializationHelper.serialize(webContext));
	}
	
	@SuppressWarnings("java:S3776") // Complexity OK
	public static @Nullable AbstractWebContext getCachedConversation(@Nullable String webId,
																		@Nullable HttpServletRequest request) {
		AbstractWebContext result = null;

        // Context key here is a UUID with a bizId smashed together
        // The first 1 is the web context ID, the second 1 is the bizId of the context bean to use
		// NB - Can't check for 72 char webId as bizIds could be non UUIDs for legacy data stores...
		// So check that they are > 36 (UUID length + something at least)
		if ((webId != null) && (webId.length() > 36)) {
			String conversationKey = webId.substring(0, 36);
			String currentBeanId = webId.substring(36);
			byte[] value = getConversations().get(conversationKey);
			if (value == null) {
				throw new ConversationEndedException((request == null) ? Locale.ENGLISH : request.getLocale());
			}
	
			result = (AbstractWebContext) SerializationHelper.deserialize(value);
			
			// Check conversation belongs to the web session if defined
			if (request != null) {
				HttpSession session = request.getSession(false);
				if (session == null) {
					throw new SessionEndedException(request.getLocale());
				}
				String sessionId = result.getSessionId();
				if (sessionId == null) {
					throw new ConversationEndedException(request.getLocale());
				}
				if (! sessionId.equals(session.getId())) {
					throw new ConversationEndedException(request.getLocale());
				}
			}
			
	        result.setKey(conversationKey);
	        result.setCurrentBean(result.getBean(currentBeanId));
		}
		
		return result;
	}
	
	private static final AtomicInteger SESSION_COUNT = new AtomicInteger(0);

	/**
	 * Returns the sessionCount.
	 */
	public static int getSessionCount() {
		return SESSION_COUNT.get();
	}
	
	/**
	 * Performs incrementSessionCount.
	 */
	public static void incrementSessionCount() {
		SESSION_COUNT.incrementAndGet();
	}
	
	/**
	 * Decrements the JVM-wide session count, never below zero.
	 *
	 * <p>Threading: atomic; a decrement at zero does nothing, and a concurrent increment is never lost
	 * nor a negative count seen.
	 */
	public static void decrementSessionCount() {
		SESSION_COUNT.updateAndGet(count -> Math.max(0, count - 1));
	}

	@SuppressWarnings("rawtypes")
	private static @Nonnull Cache<String, TreeSet> getSessions() {
		return EXT.getCaching().getEHCache(UtilImpl.SESSION_CACHE.getName(), String.class, TreeSet.class);
	}

	/**
	 * Atomically applies a change to the set cached under a key, retrying until no other thread changed
	 * that entry in between.
	 *
	 * <p>Each attempt copies the current set (an empty set when the key is absent), applies the change to
	 * the copy, and writes the copy only if the entry still holds the set that was read: with
	 * {@code putIfAbsent} when the key was absent, {@code remove(key, old)} when the copy is empty and
	 * {@code removeWhenEmpty} is set, else {@code replace(key, old, new)}. The cached set itself is never
	 * changed, so a reader holding it sees a consistent snapshot. When the change reports no change,
	 * nothing is written.
	 *
	 * <p>Threading: safe to call concurrently for the same key, because the cache's conditional operations
	 * are atomic; it has no lock objects of its own. An attempt fails only when another thread changed the
	 * entry (or it expired) after it was read, and then it retries.
	 *
	 * @param cache	the cache holding the sets
	 * @param key	the entry's key
	 * @param removeWhenEmpty	{@code true} to remove the entry when the change leaves its set empty,
	 * 							{@code false} to keep an empty set
	 * @param change	applies the change to the copy it is given, returning {@code true} if the copy changed;
	 * 					it may run more than once, so it must have no other side effects
	 * @param <E>	the set's element type
	 */
	@SuppressWarnings("rawtypes")
	private static <E> void updateSet(@Nonnull Cache<String, TreeSet> cache,
										@Nonnull String key,
										boolean removeWhenEmpty,
										@Nonnull Predicate<TreeSet<E>> change) {
		boolean written = false;
		while (! written) {
			TreeSet<E> current = cache.get(key);
			TreeSet<E> next = (current == null) ? new TreeSet<>() : new TreeSet<>(current);
			if (! change.test(next)) {
				return;
			}
			if (current == null) {
				written = (cache.putIfAbsent(key, next) == null);
			}
			else if (removeWhenEmpty && next.isEmpty()) {
				written = cache.remove(key, current);
			}
			else {
				written = cache.replace(key, current, next);
			}
		}
	}

	/**
	 * Registers a session id against a user, keeping the user's other session ids.
	 *
	 * <p>Side effects: writes the user's entry in the session cache (unless the id is already there).
	 *
	 * <p>Threading: atomic; simultaneous sign-ins of one user each keep their session id.
	 *
	 * @param userId	the user's id
	 * @param session	the session to register
	 */
	public static void addSession(@Nonnull String userId, @Nonnull HttpSession session) {
		String sessionId = session.getId();
		updateSet(getSessions(), userId, true, (TreeSet<String> sessionIds) -> sessionIds.add(sessionId));
	}

	/**
	 * Unregisters a session id from a user, removing the user's entry when it was the last one.
	 *
	 * <p>Side effects: writes or removes the user's entry in the session cache (unless the id is not there).
	 *
	 * <p>Threading: atomic; simultaneous sign-ins and sign-outs of one user change only their own session id.
	 *
	 * @param userId	the user's id
	 * @param session	the session to unregister
	 */
	public static void removeSession(@Nonnull String userId, @Nonnull HttpSession session) {
		String sessionId = session.getId();
		updateSet(getSessions(), userId, true, (TreeSet<String> sessionIds) -> sessionIds.remove(sessionId));
	}
	
	/**
	 * Performs checkSession.
	 */
	@SuppressWarnings("rawtypes")
	public static boolean checkSession(@Nonnull String userId, @Nonnull HttpSession session) {
		Cache<String, TreeSet> sessions = getSessions();
		TreeSet sessionIds = sessions.get(userId);
		if (sessionIds != null) {
			return sessionIds.contains(session.getId());
		}
		return false;
	}

	/**
	 * Determines whether the user has at least one different registered session ID.
	 *
	 * @param userId The user identifier.
	 * @param session The current session.
	 * @return {@code true} if at least one other session ID is already registered for the user.
	 */
	@SuppressWarnings("rawtypes")
	public static boolean hasOtherSession(@Nonnull String userId, @Nonnull HttpSession session) {
		Cache<String, TreeSet> sessions = getSessions();
		TreeSet sessionIds = sessions.get(userId);
		if (sessionIds == null || sessionIds.isEmpty()) {
			return false;
		}
		if (sessionIds.size() > 1) {
			return true;
		}
		return ! sessionIds.contains(session.getId());
	}

	/**
	 * Returns the number of currently registered session IDs for a user.
	 *
	 * @param userId The user identifier.
	 * @return The count of session IDs currently tracked for this user.
	 */
	@SuppressWarnings("rawtypes")
	public static int getSessionCount(@Nonnull String userId) {
		Cache<String, TreeSet> sessions = getSessions();
		TreeSet sessionIds = sessions.get(userId);
		return (sessionIds == null) ? 0 : sessionIds.size();
	}
	
	/**
	 * Removes the sessions.
	 */
	public static void removeSessions(@Nonnull String userId) {
		getSessions().remove(userId);
	}
	
	/**
	 * Ends all of a user's sessions except one, which stays signed in.
	 *
	 * <p>A session ends when it is no longer registered against its user: the next request it makes fails
	 * {@link #checkSession(String, HttpSession)}, is invalidated and goes to the expired page. This unregisters
	 * every session id of the user but {@code keep}'s. If {@code keep} is not registered it is not added,
	 * so all the user's sessions end.
	 *
	 * <p>Side effects: writes or removes the user's entry in the session cache (unless nothing else is there).
	 *
	 * <p>Threading: atomic, as {@link #removeSession(String, HttpSession)}; a session registered by a sign in
	 * that races with this call is either kept or ended whole, never lost from the cache.
	 *
	 * @param userId	the user's id
	 * @param keep	the session to keep, normally the current request's
	 * @return	the number of sessions ended
	 */
	public static int removeOtherSessions(@Nonnull String userId, @Nonnull HttpSession keep) {
		String keepId = keep.getId();
		int[] removed = new int[1];
		updateSet(getSessions(), userId, true, (TreeSet<String> sessionIds) -> {
			// NB the change can run again after a lost race, so count afresh each time
			int before = sessionIds.size();
			boolean keepRegistered = sessionIds.contains(keepId);
			sessionIds.clear();
			if (keepRegistered) {
				sessionIds.add(keepId);
			}
			removed[0] = before - sessionIds.size();
			return removed[0] > 0;
		});
		return removed[0];
	}

	/**
	 * Returns the session ids registered against a user.
	 *
	 * @param userId	the user's id
	 * @return	a read-only snapshot, in session id order, that later sign-ins and sign-outs do not change;
	 * 			empty when the user has no sessions
	 */
	public static @Nonnull Set<String> getSessions(@Nonnull String userId) {
		@SuppressWarnings("rawtypes")
		Cache<String, TreeSet> sessions = getSessions();
		Set<String> result = sessions.get(userId);
		if (result == null) {
			return Collections.emptySet();
		}
		return Collections.unmodifiableSet(result);
	}

	public static @Nonnull Cache<String, IPGeolocation> getGeoIPs() {
		return EXT.getCaching().getEHCache(UtilImpl.GEO_IP_CACHE.getName(), String.class, IPGeolocation.class);
	}
	
	@SuppressWarnings("rawtypes")
	private static @Nonnull Cache<String, TreeSet> getTokens() {
		return EXT.getCaching().getEHCache(UtilImpl.CSRF_TOKEN_CACHE.getName(), String.class, TreeSet.class);
	}

	/**
	 * Performs clearTokens.
	 */
	public static void clearTokens(@Nonnull HttpSession session) {
		clearTokens(session.getId());
	}
	
	/**
	 * Invalidates every CSRF token of a session.
	 *
	 * <p>Side effects: replaces a known session's token set with an empty one in the CSRF token cache;
	 * does nothing for an unknown session.
	 *
	 * <p>Threading: atomic.
	 *
	 * @param sessionId	the session's id
	 */
	public static void clearTokens(@Nonnull String sessionId) {
		getTokens().replace(sessionId, new TreeSet<>());
	}
	
	/**
	 * Performs checkToken.
	 */
	public static boolean checkToken(@Nonnull HttpSession session, @Nullable Integer token) {
		return checkToken(session.getId(), token);
	}
	
	/**
	 * Performs checkToken.
	 */
	@SuppressWarnings("rawtypes")
	public static boolean checkToken(@Nonnull String sessionId, @Nullable Integer token) {
		if (token == null) {
			return false;
		}
		Cache<String, TreeSet> tokens = getTokens();
		TreeSet values = tokens.get(sessionId);
		return (values != null) && values.contains(token);
	}

	/**
	 * Removes one token from a session's tokens, leaving its other tokens valid.
	 *
	 * <p>Side effects: puts the session's token set back in the CSRF token cache.
	 *
	 * @param session	the session
	 * @param token	the token to remove, or {@code null} to do nothing
	 */
	public static void removeToken(@Nonnull HttpSession session, @Nullable Integer token) {
		removeToken(session.getId(), token);
	}

	/**
	 * Removes one token from a session's tokens, leaving its other tokens valid.
	 *
	 * <p>Side effects: writes the session's token set in the CSRF token cache when it held the token;
	 * a set left empty stays in the cache.
	 *
	 * <p>Threading: atomic; simultaneous token changes in one session are all kept.
	 *
	 * @param sessionId	the session's id
	 * @param token	the token to remove, or {@code null} to do nothing
	 */
	public static void removeToken(@Nonnull String sessionId, @Nullable Integer token) {
		if (token == null) {
			return;
		}
		updateSet(getTokens(), sessionId, false, (TreeSet<Integer> values) -> values.remove(token));
	}

	public static void replaceToken(@Nonnull HttpSession session,
										@Nullable Integer oldToken,
										@Nonnull Integer newToken) {
		replaceToken(session.getId(), oldToken, newToken);
	}

	/**
	 * Replaces one of a session's CSRF tokens with a new one, leaving its other tokens valid.
	 *
	 * <p>Side effects: writes the session's token set in the CSRF token cache, creating it for a new
	 * session; does nothing when both tokens are the same.
	 *
	 * <p>Threading: atomic; simultaneous token changes in one session are all kept.
	 *
	 * @param sessionId	the session's id
	 * @param oldToken	the token to invalidate, or {@code null} to only add {@code newToken}
	 * @param newToken	the token to make valid
	 */
	public static void replaceToken(@Nonnull String sessionId,
										@Nullable Integer oldToken,
										@Nonnull Integer newToken) {
		if (newToken.equals(oldToken)) {
			return;
		}
		
		updateSet(getTokens(), sessionId, false, (TreeSet<Integer> values) -> {
			boolean removed = (oldToken != null) && values.remove(oldToken);
			return values.add(newToken) || removed;
		});
	}
	
	// This is thread-safe
	private static final SecureRandom RANDOM = new SecureRandom();
    // Get 128 random bytes - move past first seed sequence
	static {
	    byte[] randomBytes = new byte[128];
	    RANDOM.nextBytes(randomBytes);
	}
	
	public static @Nonnull Integer createToken() {
		return Integer.valueOf(RANDOM.nextInt());
	}
	
	/**
	 * Logs the stateStats.
	 */
	public static void logStateStats() {
		logCacheStats(UtilImpl.CONVERSATION_CACHE.getName(), "Conversation");
		logCacheStats(UtilImpl.CSRF_TOKEN_CACHE.getName(), "CSRF Token");
		logCacheStats(UtilImpl.GEO_IP_CACHE.getName(), "Geo IP");
		logCacheStats(UtilImpl.SESSION_CACHE.getName(), "User Session");
		FACES_LOGGER.info("Session count = {}", SESSION_COUNT);
		FACES_LOGGER.info("********************************************************************************");
	}
	
	@SuppressWarnings("java:S2629") // logging with .toString() is good here
	private static void logCacheStats(@Nonnull String cacheName, @Nonnull String cacheDescription) {
		Caching caching = EXT.getCaching();
		CacheStatistics statistics = caching.getEHCacheStatistics(cacheName);
		StringBuilder log = new StringBuilder(64);
		TierStatistics tier = caching.getEHTierStatistics(statistics, CacheTier.OnHeap);
		if (tier != null) {
			log.append(cacheDescription).append(" Count in heap memory = ").append(tier.getMappings());
			FACES_LOGGER.info(log.toString());
			log.setLength(0);
		}
		tier = caching.getEHTierStatistics(statistics, CacheTier.OffHeap);
		if (tier != null) {
			log.append(cacheDescription).append(" Count/MB in off-heap memory = ").append(tier.getMappings());
			log.append('/').append((long) (tier.getOccupiedByteSize() / 1024.0 / 1024.0 * 10.0) / 10.0);
			FACES_LOGGER.info(log.toString());
			log.setLength(0);
		}
		tier = caching.getEHTierStatistics(statistics, CacheTier.Disk);
		if (tier != null) {
			log.append(cacheDescription).append(" Count/MB on disk = ").append(tier.getMappings());
			log.append('/').append((long) (tier.getOccupiedByteSize() / 1024.0 / 1024.0 * 10.0) / 10.0);
			FACES_LOGGER.info(log.toString());
		}
	}
	
	/**
	 * Performs evictExpiredConversations.
	 */
	public static void evictExpiredConversations() {
		Cache<String, byte[]> conversations = getConversations();
		Iterator<Entry<String, byte[]>> i = conversations.iterator();
		while (i.hasNext()) {
			// accessing an entry is meant to evict the entry if it has expired.
			// iterating isn't enough but containsKey() does the trick
			Entry<String, byte[]> entry = i.next();
			if (entry != null) {
				conversations.containsKey(entry.getKey());
			}
		}
	}
	
	/**
	 * Performs evictExpiredSessionTokens.
	 */
	@SuppressWarnings("rawtypes")
	public static void evictExpiredSessionTokens() {
		Cache<String, TreeSet> tokens = getTokens();
		Iterator<Entry<String, TreeSet>> i = tokens.iterator();
		while (i.hasNext()) {
			// accessing an entry is meant to evict the entry if it has expired.
			// iterating isn't enough but containsKey() does the trick
			Entry<String, TreeSet> entry = i.next();
			if (entry != null) {
				tokens.containsKey(entry.getKey());
			}
		}
	}
	
	public static @Nonnull String encode64(@Nonnull Serializable obj) 
	throws IOException {
		byte[] result = null;
		
		try (ByteArrayOutputStream baos = new ByteArrayOutputStream()) {
			try (OutputStream zos = new GZIPOutputStream(baos)) {
				// Don't use standard Java serialization as it has issues with hibernate
				SerializationHelper.serialize(obj, zos);
			}
			baos.flush();
			result = baos.toByteArray();
		}
		Base64 base64Codec = new Base64();

		return new String(base64Codec.encode(result), StandardCharsets.ISO_8859_1);
	}

	@SuppressWarnings("unchecked")
	public static @Nonnull <T extends Serializable> T decode64(@Nonnull String encoding)
	throws IOException {
		T result = null;

		Base64 base64Codec = new Base64();
		byte[] gzippedoos = base64Codec.decode(encoding.getBytes(StandardCharsets.ISO_8859_1));
		try (ByteArrayInputStream bais = new ByteArrayInputStream(gzippedoos)) {
			try (InputStream zis = new GZIPInputStream(bais)) {
				// Don't use standard Java serialization as it has issues with hibernate
				result = (T) SerializationHelper.deserialize(zis);
			}
		}

		return result;
	}
}
