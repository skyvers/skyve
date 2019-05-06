package org.skyve.impl.web;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.hibernate.internal.util.SerializationHelper;
import org.skyve.domain.messages.ConversationEndedException;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.AbstractWebContext;

import net.sf.ehcache.Cache;
import net.sf.ehcache.CacheManager;
import net.sf.ehcache.Element;
import net.sf.ehcache.statistics.StatisticsGateway;

public class ConversationUtil {
	private static final String CONVERSATIONS_CACHE_NAME = "conversations";
	
	private ConversationUtil() {
		// Disallow instantiation.
	}

	public static void initConversationsCache() {
		CacheManager singletonManager = CacheManager.getInstance();
		Cache conversations = new Cache(CONVERSATIONS_CACHE_NAME, 
											UtilImpl.MAX_CONVERSATIONS_IN_MEMORY, 
											true, 
											false, 
											0, 
											UtilImpl.CONVERSATION_EVICTION_TIME_MINUTES * 60);
		singletonManager.addCache(conversations);
	}

	private static Cache getConversations() {
		return CacheManager.getInstance().getCache(CONVERSATIONS_CACHE_NAME);
	}

	public static void destroyConversationsCache() {
		CacheManager.getInstance().shutdown();
	}
	
	public static void cacheConversation(AbstractWebContext webContext)
	throws Exception {
		if (webContext != null) {
			// Note that EHCache puts are thread-safe
			getConversations().put(new Element(webContext.getKey(), SerializationHelper.serialize(webContext)));
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
			Element element = getConversations().get(conversationKey);
			if (element == null) {
				throw new ConversationEndedException();
			}

			result = (AbstractWebContext) SerializationHelper.deserialize((byte[]) element.getObjectValue());
			result.setHttpServletRequest(request);
            result.setHttpServletResponse(response);
            result.setKey(conversationKey);
            result.setCurrentBean(result.getBean(currentBeanId));
		}
		
		return result;
	}
	
	public static void logConversationsStats() {
		Cache conversations = ConversationUtil.getConversations();
		UtilImpl.LOGGER.info("Count = " + conversations.getSize());
		StatisticsGateway statistics = conversations.getStatistics();
		if (statistics != null) {
			UtilImpl.LOGGER.info("Count in memory = " + statistics.getLocalHeapSize());
			UtilImpl.LOGGER.info("Count on disk = " + statistics.getLocalDiskSize());
			// NB - This method takes a long time for large object graphs, so don't use it on prod systems.
			//UtilImpl.LOGGER.info("In-Memory (MB) = " + (statistics.getLocalHeapSizeInBytes() / 1048576.0));
		}
		UtilImpl.LOGGER.info("**************************************************************");
	}
}
