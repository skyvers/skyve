package org.skyve.impl.web;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.util.List;
import java.util.UUID;
import java.util.logging.Level;

import javax.servlet.ServletException;
import javax.servlet.http.Cookie;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.domain.Bean;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.ValidationException;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.domain.messages.SecurityException;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.impl.metadata.user.SuperUser;
import org.skyve.impl.metadata.user.UserImpl;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.util.SQLMetaDataUtil;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.WebStatsUtil;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.repository.Repository;
import org.skyve.metadata.user.User;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.Persistence;
import org.skyve.util.Binder;
import org.skyve.util.Mail;
import org.skyve.util.Util;
import org.skyve.web.WebContext;

public class WebUtil {
	private WebUtil() {
		// Disallow instantiation.
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
			UserImpl principalUser = ProvidedRepositoryFactory.setCustomerAndUserFromPrincipal(userPrincipal);
			if (! (user.getCustomerName().equals(principalUser.getCustomerName()) &&
					user.getName().equals(principalUser.getName()))) {
				user = null;
			}
		}
		
		if (user == null) {
			// This can happen using SSO when the session expires as the servlets are not protected by normal Java EE security
			if (userPrincipal != null) {
				user = ProvidedRepositoryFactory.get().retrieveUser(userPrincipal);
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
	
	public static Bean getConversationBeanFromRequest(AbstractWebContext webContext,
														HttpServletRequest request)
	throws Exception {
		// Find the context bean
		// Note - if there is no form in the view then there is no web context
		Bean result = null;
		if (webContext != null) {
    		result = webContext.getCurrentBean();

			String bizId = request.getParameter(Bean.DOCUMENT_ID); 
	    	String formBinding = request.getParameter(AbstractWebContext.BINDING_NAME);
    		if (formBinding != null) { // sub-form
    			formBinding = BindUtil.unsanitiseBinding(formBinding);
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
	 * Delete all cookies (except the customer cookie which could be useful next time) or selected cookies by name.
	 * If names varargs isn't used all cookies bar the customer cookie are deleted.
	 * If names varargs is used, only those cookies are deleted.
	 */
	public static void deleteCookies(HttpServletRequest request, HttpServletResponse response, String... names) {
		// remove all cookies too
		Cookie[] cookies = request.getCookies();
		if (cookies != null && cookies.length > 0) {
			for (Cookie cookie : cookies) {
				String name = cookie.getName();
				boolean delete = (names.length == 0) && 
									(! AbstractWebContext.CUSTOMER_COOKIE_NAME.equals(name)) &&
									// Don't include "ecuador_expandeditems", "ultima_expandeditems" as the menus don't respond well.
									// Ecuador Menu - accordions do not open at all when expanded on server menu model and no cookie set
									// Ultima Menu - Selecting a different menu item within the same module makes the module accordion collapse
									(! name.startsWith("ecuador_")) &&
									(! name.startsWith("ultima_"));
				if (! delete) {
					for (String n : names) {
						if (n.equals(name)) {
							delete = true;
							break;
						}
					}
				}
				if (delete) {
					cookie.setValue("-");
					cookie.setMaxAge(0);
					cookie.setPath("/");
					cookie.setHttpOnly(true);
					cookie.setSecure(Util.isSecureUrl());
					response.addCookie(cookie);
				}
			}
		}
	}
	
	/**
	 * Delete the menu state cookies for all PF themes.
	 */
	public static void deleteMenuCookies(HttpServletRequest request, HttpServletResponse response) {
		// Don't include "ecuador_expandeditems", "ultima_expandeditems" as the menus don't respond well.
		// Ecuador Menu - accordions do not open at all when expanded on server menu model and no cookie set
		// Ultima Menu - Selecting a different menu item within the same module makes the module accordion collapse
		deleteCookies(request, response, "panelMenu-leftMenu");
	}
	
	/**
	 * Really logout of the app - logout, invalidate session and remove all cookies.
	 */
	public static void logout(HttpServletRequest request, HttpServletResponse response) 
	throws ServletException {
		request.logout();

		// NB invalidate the session after logging out otherwise WebLogic 12c NPEs
		HttpSession s = request.getSession(false);
		if (s != null) {
			s.invalidate();
		}
		
		deleteCookies(request, response);
	}
	
	/**
	 * Called from the changePassword.jsp.
	 * 
	 * @param user
	 * @param newPassword
	 * @return
	 * @throws Exception
	 */
	public static String makePasswordChange(User user, String oldPassword, String newPassword, String confirmPassword) 
	throws Exception {
		String errorMessage = null;
		
		Customer c = user.getCustomer();
		Module admin = c.getModule(SQLMetaDataUtil.ADMIN_MODULE_NAME);
		Document changePassword = admin.getDocument(c, SQLMetaDataUtil.CHANGE_PASSWORD_DOCUMENT_NAME);
		Bean bean = changePassword.newInstance(user);
		BindUtil.set(bean, SQLMetaDataUtil.OLD_PASSWORD_PROPERTY_NAME, oldPassword);
		BindUtil.set(bean, SQLMetaDataUtil.NEW_PASSWORD_PROPERTY_NAME, newPassword);
		BindUtil.set(bean, SQLMetaDataUtil.CONFIRM_PASSWORD_PROPERTY_NAME, confirmPassword);

		AbstractPersistence persistence = AbstractPersistence.get();
		persistence.setUser(user); // user has not been set as this is called directly from changePassword.jsp
		persistence.begin();
		try {
			changePassword.getServerSideAction(c, SQLMetaDataUtil.MAKE_PASSWORD_CHANGE_ACTION_NAME, true).execute(bean, null);
		}
		catch (ValidationException e) {
			persistence.rollback();
			List<Message> messages = e.getMessages();
			if (messages.isEmpty()) {
				errorMessage = e.getLocalizedMessage();
			}
			else {
				errorMessage = messages.get(0).getText();
			}
		}
		catch (@SuppressWarnings("unused") Exception e) {
			persistence.rollback();
		}
		finally {
			persistence.commit(true);
		}
		
		return errorMessage;
	}
	
	/**
	 * Called from the requestPasswordReset.jsp.
	 * 
	 * @param userName
	 */
	public static void requestPasswordReset(String customer, String email) throws Exception {
		SuperUser u = new SuperUser();
		u.setCustomerName(customer);
		Customer c = u.getCustomer();
		AbstractPersistence p = AbstractPersistence.get();
		try {
			p.begin();
			p.setUser(u);
			DocumentQuery q = p.newDocumentQuery(SQLMetaDataUtil.ADMIN_MODULE_NAME, SQLMetaDataUtil.USER_DOCUMENT_NAME);
			q.getFilter().addEquals(Binder.createCompoundBinding(SQLMetaDataUtil.CONTACT_PROPERTY_NAME, SQLMetaDataUtil.EMAIL1_PROPERTY_NAME), email);
			
			// set reset password token for all users with the same email address across all customers
			List<PersistentBean> users = q.beanResults();
			if(! users.isEmpty()) {
				PersistentBean firstUser = null;
				String passwordResetToken = generatePasswordResetToken();
				for(PersistentBean user: users) {
					Binder.set(user, SQLMetaDataUtil.PASSWORD_RESET_TOKEN_PROPERTY_NAME, passwordResetToken);
					p.upsertBeanTuple(user);
					if(firstUser==null) {
						firstUser = user;
					}
				}

				// send a single email to the user's email address 
				// (if multiple user with the same email, only 1 email should be sent)
				Module m = c.getModule(SQLMetaDataUtil.ADMIN_MODULE_NAME);
				Document d = m.getDocument(c, SQLMetaDataUtil.CONFIGURATION_DOCUMENT_NAME);
				Bean configuration = d.newInstance(u);
				String subject = (String) Binder.get(configuration, SQLMetaDataUtil.PASSWORD_RESET_EMAIL_SUBJECT_PROPERTY_NAME);
				subject = Binder.formatMessage(subject, firstUser);
				String body = (String) Binder.get(configuration, SQLMetaDataUtil.PASSWORD_RESET_EMAIL_BODY_PROPERTY_NAME); 
				
				body = body.replace("{#resetPasswordUrl}", Util.getResetPasswordUrl());
				// keeping this for backwards compatibility
				body = body.replace("{url}", Util.getSkyveContextUrl());
				
				body = Binder.formatMessage(body, firstUser);
				String fromEmail = (String) Binder.get(configuration, SQLMetaDataUtil.FROM_EMAIL_PROPERTY_NAME);
				EXT.sendMail(new Mail().addTo(email).from(fromEmail).subject(subject).body(body));
			}
		}
		catch (Exception t) {
			p.rollback();
			throw t;
		}
		finally {
			p.commit(true);
		}
	}

	public static String generatePasswordResetToken() {
		return UUID.randomUUID().toString() + Long.toString(System.currentTimeMillis());
	}

	/**
	 * /download?_n=<action>&_doc=<module.document>&_c=<webId>&_ctim=<millis> and optionally &_b=<view binding>
	 */
	public static String getDownloadActionUrl(String downloadActionName,
												String targetModuleName,
												String targetDocumentName,
												String webId,
												String viewBinding,
												String dataWidgetBinding,
												String elementBizId) {
		StringBuilder result = new StringBuilder(128);
		result.append(Util.getSkyveContextUrl()).append("/download?");
		result.append(AbstractWebContext.RESOURCE_FILE_NAME).append('=').append(downloadActionName);
		result.append('&').append(AbstractWebContext.DOCUMENT_NAME).append('=');
		result.append(targetModuleName).append('.').append(targetDocumentName);
		result.append('&').append(AbstractWebContext.CONTEXT_NAME).append('=').append(webId);
		
		String binding = null;
		if (viewBinding != null) {
			if (dataWidgetBinding != null) {
				binding = BindUtil.createCompoundBinding(viewBinding, dataWidgetBinding);
			}
			else {
				binding = viewBinding;
			}
		}
		else if (dataWidgetBinding != null) {
			binding = dataWidgetBinding;
		}
		if (binding != null) {
			result.append('&').append(AbstractWebContext.BINDING_NAME).append('=').append(binding);
		}
		if (dataWidgetBinding != null) {
			result.append('&').append(Bean.DOCUMENT_ID).append('=').append(elementBizId);
		}

		result.append('&').append(AbstractWebContext.CURRENT_TIME_IN_MILLIS).append('=').append(System.currentTimeMillis());

		return result.toString();
	}
	
	/**
	 * Called from the resetPassword.jsp.
	 * 
	 * @param passwordResetToken
	 * @param newPassword
	 */
	public static String resetPassword(String passwordResetToken, String newPassword, String confirmPassword)
	throws Exception {
		String customerName = null;
		String userName = null;
		String errorMsg = null;
		try (Connection c = EXT.getDataStoreConnection()) {
			try (PreparedStatement s = c.prepareStatement(String.format("select %s, %s from ADM_SecurityUser where %s = ?",
																			Bean.CUSTOMER_NAME,
																			SQLMetaDataUtil.USER_NAME_PROPERTY_NAME,
																			SQLMetaDataUtil.PASSWORD_RESET_TOKEN_PROPERTY_NAME))) {
				s.setString(1, passwordResetToken);
				try (ResultSet rs = s.executeQuery()) {
					if (!rs.isBeforeFirst() ) {
						return "Reset link used is invalid";
					}
					while (rs.next()) {
						customerName = rs.getString(1);
						userName = rs.getString(2);

						Repository r = CORE.getRepository();
						org.skyve.metadata.user.User u = r.retrieveUser(String.format("%s/%s", customerName, userName));
						errorMsg = makePasswordChange(u, null, newPassword, confirmPassword);
					}
				}
			}
		}

		return errorMsg;
	}
	
	// find the existing bean with retrieve
	public static Bean findReferencedBean(Document referenceDocument, 
											String bizId, 
											Persistence persistence,
											Bean conversationBean,
											WebContext webContext) {
		Bean result = null;
		
		User user = persistence.getUser();
		Customer customer = user.getCustomer();
		Bizlet<Bean> bizlet = referenceDocument.getBizlet(customer);
		if (bizlet != null) {
			try {
				result = bizlet.resolve(bizId, conversationBean, webContext);
			}
			catch (ValidationException e) {
				throw e;
			}
			catch (Exception e) {
				UtilImpl.LOGGER.log(Level.SEVERE, 
										String.format("Failed to resolve document %s.%s with bizId %s",
														referenceDocument.getOwningModuleName(),
														referenceDocument.getName(),
														bizId),
										e);
				throw new MetaDataException(String.format("Failed to resolve this %s.", 
															referenceDocument.getLocalisedSingularAlias()),
												e);
				
			}
		}
		if (result == null) {
			result = persistence.retrieve(referenceDocument, bizId);
		}
		if (result == null) {
			throw new ValidationException(new Message(String.format("Failed to retrieve this %s as it has been deleted.", 
																		referenceDocument.getLocalisedSingularAlias())));
		}
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
	
	public static String getRefererHeader(HttpServletRequest request) {
		String result = Util.processStringValue(request.getHeader("referer"));
		if (result != null) {
			if (! result.startsWith(Util.getSkyveContextUrl())) {
				Util.LOGGER.warning("referer header " + result +
										" looks tampered with because it does not start with " + Util.getSkyveContextUrl() + 
										". This looks like a doctored request because Referrer-Policy should be same-origin!");
				result = null;
			}
		}
		return result;
	}

	/**
	 * Sends a registration email with an activation code to the specified email address.
	 * 
	 * @param userBizId The email address to send the email to
	 * @throws Exception
	 */
	public static void sendRegistrationEmail(final String userBizId) throws Exception {
		Customer cust = CORE.getCustomer();
		Module admin = cust.getModule(SQLMetaDataUtil.ADMIN_MODULE_NAME);
		Document selfRegistration = admin.getDocument(cust, SQLMetaDataUtil.SELF_REGISTRATION_DOCUMENT_NAME);
		Bean bean = selfRegistration.newInstance(CORE.getUser());
		BindUtil.set(bean, Binder.createCompoundBinding(SQLMetaDataUtil.USER_PROPERTY_NAME, Bean.DOCUMENT_ID), userBizId);

		AbstractPersistence persistence = AbstractPersistence.get();
		persistence.setUser(CORE.getUser()); // user has not been set as this is called directly from changePassword.jsp
		persistence.begin();
		try {
			selfRegistration.getServerSideAction(cust, SQLMetaDataUtil.RESEND_ACTIVATION_ACTION_NAME, true).execute(bean, null);
		}
		catch (Exception e) {
			persistence.rollback();
			e.printStackTrace();
		}
		finally {
			persistence.commit(true);
		}
	}
}
