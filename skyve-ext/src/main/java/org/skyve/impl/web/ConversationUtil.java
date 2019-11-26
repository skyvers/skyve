package org.skyve.impl.web;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.ehcache.Cache;
import org.ehcache.core.statistics.CacheStatistics;
import org.hibernate.internal.util.SerializationHelper;
import org.skyve.domain.messages.ConversationEndedException;
import org.skyve.impl.util.CacheUtil;
import org.skyve.impl.util.CacheUtil.CacheTier;
import org.skyve.impl.util.UtilImpl;

public class ConversationUtil {
	private static final String CONVERSATIONS_CACHE_NAME = "conversations";
	
	private ConversationUtil() {
		// Disallow instantiation.
	}

	public static void initConversationsCache() {
		CacheUtil.createEHCache(CONVERSATIONS_CACHE_NAME,
									UtilImpl.CONVERSATION_HEAP_MAX_ENTRIES,
									UtilImpl.CONVERSATION_OFF_HEAP_MAX_SIZE_MB,
									UtilImpl.CONVERSATION_DISK_MAX_SIZE_GB * 1024,
									UtilImpl.CONVERSATION_EVICTION_TIME_MINUTES,
									String.class,
									byte[].class);
	}

	private static Cache<String, byte[]> getConversations() {
		return CacheUtil.getEHCache(CONVERSATIONS_CACHE_NAME, String.class, byte[].class);
	}

	public static void destroyConversationsCache() {
		CacheUtil.destroyEHCache(CONVERSATIONS_CACHE_NAME);
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
	
	public static void logConversationsStats() {
		CacheStatistics statistics = CacheUtil.getEHCacheStatistics(CONVERSATIONS_CACHE_NAME);
		if (statistics != null) {
			UtilImpl.LOGGER.info("Count in heap memory = " + CacheUtil.getEHTierStatistics(statistics, CacheTier.OnHeap).getMappings());
			UtilImpl.LOGGER.info("Count in off-heap memory = " + CacheUtil.getEHTierStatistics(statistics, CacheTier.OffHeap).getMappings());
			UtilImpl.LOGGER.info("MB in off-heap memory = " + ((int) (CacheUtil.getEHTierStatistics(statistics, CacheTier.OffHeap).getOccupiedByteSize() / 1024F / 1024F * 10) / 10F));
			UtilImpl.LOGGER.info("Count on disk = " + CacheUtil.getEHTierStatistics(statistics, CacheTier.Disk).getMappings());
			UtilImpl.LOGGER.info("MB on disk = " + ((int) (CacheUtil.getEHTierStatistics(statistics, CacheTier.Disk).getOccupiedByteSize() / 1024F / 1024F * 10) / 10F));
		}
		UtilImpl.LOGGER.info("**************************************************************");
	}
}
