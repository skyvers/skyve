package org.skyve.cache;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.ehcache.Cache;
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
}
