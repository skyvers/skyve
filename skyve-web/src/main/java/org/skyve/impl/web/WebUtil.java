package org.skyve.impl.web;

import java.util.List;

import javax.servlet.http.Cookie;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.skyve.domain.Bean;
import org.skyve.domain.messages.ConversationEndedException;
import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.ValidationException;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.domain.messages.SecurityException;
import org.skyve.impl.metadata.repository.AbstractRepository;
import org.skyve.impl.metadata.user.UserImpl;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.util.SQLMetaDataUtil;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.WebStatsUtil;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.persistence.Persistence;
import org.skyve.util.StateUtil;
import org.skyve.web.WebContext;

import net.sf.ehcache.Cache;
import net.sf.ehcache.CacheManager;
import net.sf.ehcache.Element;

public class WebUtil {
	private static final String CONVERSATIONS_CACHE_NAME = "conversations";

	private WebUtil() {
		// Disallow instantiation.
	}

	public static void initConversationsCache() {
		CacheManager singletonManager = CacheManager.create();
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
	
	public static void putConversationInCache(AbstractWebContext webContext)
	throws Exception {
		if (webContext != null) {
			getConversations().put(new Element(webContext.getKey(), StateUtil.encode64(webContext)));
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

        	result = StateUtil.decode64((String) element.getValue());
    		result.setHttpServletRequest(request);
            result.setHttpServletResponse(response);
            result.setKey(conversationKey);
            result.setCurrentBean(result.getBean(currentBeanId));
		}
		
		return result;
	}
	
	public static void logConversationsStats() {
		Cache conversations = WebUtil.getConversations();
		UtilImpl.LOGGER.info("Count = " + conversations.getSize());
		UtilImpl.LOGGER.info("Count in memory = " + conversations.getMemoryStoreSize());
		UtilImpl.LOGGER.info("Count on disk = " + conversations.getDiskStoreSize());
		UtilImpl.LOGGER.info("In-Memory (MB) = " + (conversations.calculateInMemorySize() / 1048576.0));
		UtilImpl.LOGGER.info("**************************************************************");
	}
	
	public static User processUserPrincipalForRequest(HttpServletRequest request,
														String userPrincipal,
														boolean useSession)
	throws Exception {
		UserImpl user = null;
		if (useSession) {
			user = (UserImpl) request.getSession().getAttribute(WebContext.USER_SESSION_ATTRIBUTE_NAME);
		}
		
		// If the user in the session is not the same as the security's user principal
		// then the session user needs to be reset.
		if ((user != null) && (userPrincipal != null)) {
			UserImpl principalUser = AbstractRepository.setCustomerAndUserFromPrincipal(userPrincipal);
			if (! (user.getCustomerName().equals(principalUser.getCustomerName()) &&
					user.getName().equals(principalUser.getName()))) {
				user = null;
			}
		}
		
		if (user == null) {
			// This can happen using SSO when the session expires as the servlets are not protected by normal Java EE security
			if (userPrincipal != null) {
				user = AbstractRepository.get().retrieveUser(userPrincipal);
				if (user == null) {
					throw new IllegalStateException("WebUtil: Cannot get the user " + userPrincipal);
				}
				if (useSession) {
					request.getSession(true).setAttribute(WebContext.USER_SESSION_ATTRIBUTE_NAME, user);
				}
				AbstractPersistence.get().setUser(user);
				WebStatsUtil.recordLogin(user);
			}
		}
		else {
			AbstractPersistence.get().setUser(user);
		}
		if (user != null) {
			user.setWebLocale(request.getLocale());
		}
		
		return user;
	}
	
	public static Bean getConversationBeanFromRequest(HttpServletRequest request,
														HttpServletResponse response)
	throws Exception {
		// Find the context bean
		// Note - if there is no form in the view then there is no web context
		Bean result = null;
		String contextKey = request.getParameter(AbstractWebContext.CONTEXT_NAME);
        if (contextKey != null) {
    		AbstractWebContext webContext = WebUtil.getCachedConversation(contextKey, request, response);
    		if (webContext != null) {
	    		result = webContext.getCurrentBean();
	
				String bizId = request.getParameter(Bean.DOCUMENT_ID); 
		    	String formBinding = request.getParameter(AbstractWebContext.BINDING_NAME);
	    		if (formBinding != null) { // sub-form
	        		// find the process bean
	        		Object referenceValue = BindUtil.get(result, formBinding);
	        		if (referenceValue instanceof List<?>) {
	        			result = BindUtil.getElementInCollection(result, formBinding, bizId); 
	        		}
	        		else {
	        			result = (Bean) referenceValue;
	        		}
	        	}
    		}
        }
        
        return result;
	}
	
	public static String determineCustomerWithoutSession(HttpServletRequest request) {
		String result = UtilImpl.CUSTOMER;

		if (result == null) { // no-one is logged in and its not set by configuration
			// See if the customer name is supplied as a cookie
			Cookie[] cookies = request.getCookies();
			if (cookies != null) {
				for (int i = 0, l = cookies.length; i < l; i++) {
					Cookie cookie = cookies[i];
					if (AbstractWebContext.CUSTOMER_COOKIE_NAME.equals(cookie.getName())) {
						result = cookie.getValue();
						break;
					}
				}
			}
		}
		
		return result;
	}
	
	/**
	 * Called from the changePassword.jsp.
	 * 
	 * @param user
	 * @param newPassword
	 * @return
	 * @throws Exception
	 */
	public static String makePasswordChange(User user, String newPassword) 
	throws Exception {
		String errorMessage = null;
		
		Customer c = user.getCustomer();
		Module admin = c.getModule(SQLMetaDataUtil.ADMIN_MODULE_NAME);
		Document changePassword = admin.getDocument(c, SQLMetaDataUtil.CHANGE_PASSWORD_DOCUMENT_NAME);
		Bean bean = changePassword.newInstance(user);
		BindUtil.set(bean, SQLMetaDataUtil.NEW_PASSWORD_PROPERTY_NAME, newPassword);
		BindUtil.set(bean, SQLMetaDataUtil.CONFIRM_PASSWORD_PROPERTY_NAME, newPassword);
		AbstractRepository r = AbstractRepository.get();

		AbstractPersistence persistence = AbstractPersistence.get();
		persistence.setUser(user); // user has not been set as this is called directly from changePassword.jsp
		persistence.begin();
		try {
			r.getServerSideAction(c, changePassword, SQLMetaDataUtil.MAKE_PASSWORD_CHANGE_ACTION_NAME, true).execute(bean, null);
		}
		catch (ValidationException e) {
			persistence.rollback();
			List<Message> messages = e.getMessages();
			if (messages.isEmpty()) {
				errorMessage = e.getLocalizedMessage();
			}
			else {
				errorMessage = messages.get(0).getErrorMessage();
			}
		}
		catch (Exception e) {
			persistence.rollback();
		}
		finally {
			persistence.commit(true);
		}
		
		return errorMessage;
	}
	
	// find the existing bean with retrieve
	public static Bean findReferencedBean(Document referenceDocument, 
											String bizId, 
											Persistence persistence) {
		Bean result = persistence.retrieve(referenceDocument, bizId, false);
		if (result == null) {
			throw new ValidationException(new Message(String.format("Failed to retrieve this %s as it has been deleted.", 
																		referenceDocument.getSingularAlias())));
		}
		User user = persistence.getUser();
		if (! user.canReadBean(bizId, 
								result.getBizModule(), 
								result.getBizDocument(), 
								result.getBizCustomer(), 
								result.getBizDataGroupId(), 
								result.getBizUserId())) {
			throw new SecurityException("read this data", user.getName());
		}
		
		return result;
	}
}
