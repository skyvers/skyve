package org.skyve.cache;

import java.util.concurrent.atomic.AtomicInteger;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.ehcache.Cache;
import org.ehcache.core.statistics.CacheStatistics;
import org.ehcache.core.statistics.TierStatistics;
import org.hibernate.internal.util.SerializationHelper;
import org.skyve.domain.messages.ConversationEndedException;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.AbstractWebContext;

public class ConversationUtil {
	private ConversationUtil() {
		// Disallow instantiation.
	}

	private static Cache<String, byte[]> getConversations() {
		return CacheUtil.getEHCache(UtilImpl.CONVERSATION_CACHE.getName(), String.class, byte[].class);
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
				throw new ConversationEndedException();
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
	
	public static void logSessionAndConversationsStats() {
		CacheStatistics statistics = CacheUtil.getEHCacheStatistics(UtilImpl.CONVERSATION_CACHE.getName());
		if (statistics != null) {
			TierStatistics tier = CacheUtil.getEHTierStatistics(statistics, CacheTier.OnHeap);
			if (tier != null) {
				UtilImpl.LOGGER.info("Count in heap memory = " + tier.getMappings());
			}
			tier = CacheUtil.getEHTierStatistics(statistics, CacheTier.OffHeap);
			if (tier != null) {
				UtilImpl.LOGGER.info("Count in off-heap memory = " + tier.getMappings());
				UtilImpl.LOGGER.info("MB in off-heap memory = " + ((int) (tier.getOccupiedByteSize() / 1024F / 1024F * 10F) / 10F));
			}
			tier = CacheUtil.getEHTierStatistics(statistics, CacheTier.Disk);
			if (tier != null) {
				UtilImpl.LOGGER.info("Count on disk = " + tier.getMappings());
				UtilImpl.LOGGER.info("MB on disk = " + ((int) (tier.getOccupiedByteSize() / 1024F / 1024F * 10F) / 10F));
			}
		}
		UtilImpl.LOGGER.info("Session count = " + SESSION_COUNT.get());
		UtilImpl.LOGGER.info("**************************************************************");
	}
}
