package org.skyve.impl.cache;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.Serializable;
import java.security.SecureRandom;
import java.util.Iterator;
import java.util.Locale;
import java.util.TreeSet;
import java.util.concurrent.atomic.AtomicInteger;
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
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.AbstractWebContext;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.servlet.http.HttpSession;

public class StateUtil {
	private StateUtil() {
		// Disallow instantiation.
	}

	private static Cache<String, byte[]> getConversations() {
		return EXT.getCaching().getEHCache(UtilImpl.CONVERSATION_CACHE.getName(), String.class, byte[].class);
	}

	public static void cacheConversation(AbstractWebContext webContext)
	throws Exception {
		if (webContext != null) {
			// Note that EHCache puts are thread-safe
			getConversations().put(webContext.getKey(), SerializationHelper.serialize(webContext));
		}
	}
	
	public static AbstractWebContext getCachedConversation(String webId,
															HttpServletRequest request,
															HttpServletResponse response)
	throws Exception {
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
			result.setHttpServletRequest(request);
            result.setHttpServletResponse(response);
            result.setKey(conversationKey);
            result.setCurrentBean(result.getBean(currentBeanId));
		}
		
		return result;
	}
	
	private static final AtomicInteger SESSION_COUNT = new AtomicInteger(0);

	public static int getSessionCount() {
		return SESSION_COUNT.get();
	}
	
	public static void incrementSessionCount() {
		SESSION_COUNT.incrementAndGet();
	}
	
	public static void decrementSessionCount() {
		int count = SESSION_COUNT.decrementAndGet();
		if (count < 0) {
			SESSION_COUNT.set(0);
		}
	}

	@SuppressWarnings("rawtypes")
	private static Cache<String, TreeSet> getTokens() {
		return EXT.getCaching().getEHCache(UtilImpl.CSRF_TOKEN_CACHE.getName(), String.class, TreeSet.class);
	}

	public static void clearTokens(HttpSession session) {
		clearTokens(session.getId());
	}
	
	@SuppressWarnings("rawtypes")
	public static void clearTokens(String sessionId) {
		Cache<String, TreeSet> tokens = getTokens();
		TreeSet values = tokens.get(sessionId);
		if (values != null) {
			values.clear();
			// Note that EHCache puts are thread-safe
			tokens.put(sessionId, values);
		}
	}
	
	public static boolean checkToken(HttpSession session, Integer token) {
		return checkToken(session.getId(), token);
	}
	
	@SuppressWarnings("rawtypes")
	public static boolean checkToken(String sessionId, Integer token) {
		if (token == null) {
			return false;
		}
		Cache<String, TreeSet> tokens = getTokens();
		TreeSet values = tokens.get(sessionId);
		return (values != null) && values.contains(token);
	}

	public static void replaceToken(HttpSession session, Integer oldToken, Integer newToken) {
		replaceToken(session.getId(), oldToken, newToken);
	}

	@SuppressWarnings({"unchecked", "rawtypes"})
	public static void replaceToken(String sessionId, Integer oldToken, Integer newToken) {
//System.out.println("replace token o=" + oldToken + ":n=" + newToken);
		if (newToken.equals(oldToken)) {
			return;
		}
		
		Cache<String, TreeSet> tokens = getTokens();
		TreeSet values = tokens.get(sessionId);
		if (values == null) {
			values = new TreeSet();
		}
		else {
			if (oldToken != null) {
				values.remove(oldToken);
			}
		}
		values.add(newToken);
//System.out.println("tokens size =" + values.size());
		// Note that EHCache puts are thread-safe
		tokens.put(sessionId, values);
	}
	
	public static Integer createToken() {
	    // Get 128 random bytes - move past first seed sequence
		SecureRandom random = new SecureRandom();
	    byte[] randomBytes = new byte[128];
	    random.nextBytes(randomBytes);
	    
		return Integer.valueOf(random.nextInt());
	}
	
	public static void logStateStats() {
		logCacheStats(UtilImpl.CONVERSATION_CACHE.getName(), "Conversation");
		logCacheStats(UtilImpl.CSRF_TOKEN_CACHE.getName(), "CSRF Session");
		UtilImpl.LOGGER.info("Session count = " + SESSION_COUNT.get());
		UtilImpl.LOGGER.info("********************************************************************************");
	}
	
	private static void logCacheStats(String cacheName, String cacheDescription) {
		Caching caching = EXT.getCaching();
		CacheStatistics statistics = caching.getEHCacheStatistics(cacheName);
		if (statistics != null) {
			StringBuilder log = new StringBuilder(64);
			TierStatistics tier = caching.getEHTierStatistics(statistics, CacheTier.OnHeap);
			if (tier != null) {
				log.append(cacheDescription).append(" Count in heap memory = ").append(tier.getMappings());
				UtilImpl.LOGGER.info(log.toString());
				log.setLength(0);
			}
			tier = caching.getEHTierStatistics(statistics, CacheTier.OffHeap);
			if (tier != null) {
				log.append(cacheDescription).append(" Count/MB in off-heap memory = ").append(tier.getMappings());
				log.append('/').append((long) (tier.getOccupiedByteSize() / 1024.0 / 1024.0 * 10.0) / 10.0);
				UtilImpl.LOGGER.info(log.toString());
				log.setLength(0);
			}
			tier = caching.getEHTierStatistics(statistics, CacheTier.Disk);
			if (tier != null) {
				log.append(cacheDescription).append(" Count/MB on disk = ").append(tier.getMappings());
				log.append('/').append((long) (tier.getOccupiedByteSize() / 1024.0 / 1024.0 * 10.0) / 10.0);
				UtilImpl.LOGGER.info(log.toString());
			}
		}
	}
	
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
	
	private static final String ZIP_CHARSET = "ISO-8859-1";

	public static String encode64(Serializable obj) 
	throws IOException {
		byte[] result = null;
		
		try (ByteArrayOutputStream baos = new ByteArrayOutputStream()) {
			try (OutputStream zos = new GZIPOutputStream(baos)) {
//				ObjectOutputStream oos = new ObjectOutputStream(zos);
//				oos.writeObject(obj);
//				oos.close();
				SerializationHelper.serialize(obj, zos);
			}
			baos.flush();
			result = baos.toByteArray();
		}
		Base64 base64Codec = new Base64();

		return new String(base64Codec.encode(result), ZIP_CHARSET);
	}

	@SuppressWarnings("unchecked")
	public static <T extends Serializable> T decode64(String encoding)
	throws IOException {
		T result = null;

		Base64 base64Codec = new Base64();
		byte[] gzippedoos = base64Codec.decode(encoding.getBytes(ZIP_CHARSET));
		try (ByteArrayInputStream bais = new ByteArrayInputStream(gzippedoos)) {
			try (InputStream zis = new GZIPInputStream(bais)) {
//    		ObjectInputStream ois = new ObjectInputStream(zis);
//    		result = ois.readObject();
//    		ois.close();
				result = (T) SerializationHelper.deserialize(zis);
			}
		}

		return result;
	}
}
