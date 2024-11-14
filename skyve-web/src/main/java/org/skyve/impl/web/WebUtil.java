package org.skyve.impl.web;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.net.URL;
import java.net.URLConnection;
import java.net.URLEncoder;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.Timestamp;
import java.time.Instant;
import java.util.Base64;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.logging.Level;

import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.domain.Bean;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.app.AppConstants;
import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.NoResultsException;
import org.skyve.domain.messages.SecurityException;
import org.skyve.domain.messages.SessionEndedException;
import org.skyve.domain.messages.ValidationException;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.cache.StateUtil;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.impl.metadata.user.SuperUser;
import org.skyve.impl.metadata.user.UserImpl;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.util.TimeUtil;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.WebStatsUtil;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.repository.Repository;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.TextOutput.Sanitisation;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.Persistence;
import org.skyve.util.Binder;
import org.skyve.util.GeoIPService;
import org.skyve.util.IPGeolocation;
import org.skyve.util.JSON;
import org.skyve.util.Mail;
import org.skyve.util.OWASP;
import org.skyve.util.SecurityUtil;
import org.skyve.util.Util;
import org.skyve.web.WebContext;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.Cookie;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.servlet.http.HttpSession;

public class WebUtil {
	private WebUtil() {
		// Disallow instantiation.
	}
	
	public static User processUserPrincipalForRequest(@Nonnull HttpServletRequest request,
														@Nullable String userPrincipal)
	throws Exception {
		HttpSession session = request.getSession(false);
		UserImpl user = null;
		if (session != null) {
			user = (UserImpl) session.getAttribute(WebContext.USER_SESSION_ATTRIBUTE_NAME);
			// Check the user knows about the session
			if ((user != null) && (! StateUtil.checkSession(user.getId(), session))) {
				// If not invalidate, and throw to forward to /pages/expired.jsp
				session.invalidate();
				// throw SessionEndedException here to bug out of everything either to expired.jsp
				throw new SessionEndedException(request.getLocale());
			}
		}
		
		// If the user in the session is not the same as the security's user principal
		// then the session user needs to be reset.
		if ((user != null) && (userPrincipal != null)) {
			UserImpl principalUser = ProvidedRepositoryFactory.setCustomerAndUserFromPrincipal(userPrincipal);
			if (principalUser == null) {
				user = null;
			}
			else if (! (user.getCustomerName().equals(principalUser.getCustomerName()) &&
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
				// ensure there is a session established
				if (session == null) {
					session = request.getSession(true);
				}
				setSessionId(user, request);
				session.setAttribute(WebContext.USER_SESSION_ATTRIBUTE_NAME, user);
				StateUtil.addSession(user.getId(), session);
				AbstractPersistence.get().setUser(user);
				
				// Get IP address of user to record in UserLoginRecord
				String userIPAddress = SecurityUtil.getSourceIpAddress(request);
				WebStatsUtil.recordLogin(user, userIPAddress);
				Customer customer = user.getCustomer();
				if (customer instanceof CustomerImpl) {
					((CustomerImpl) customer).notifyLogin(user, session);
				}
			}
			// TODO hack!
			else { // check basic auth
				final String authorization = request.getHeader("Authorization");
				if ((authorization != null) && authorization.startsWith("Basic")) {
					// Authorization: Basic base64credentials
					final String base64Credentials = authorization.substring("Basic".length()).trim();
					String credentials = new String(Base64.getMimeDecoder().decode(base64Credentials), Util.UTF8);
	
					// credentials = username:password or customer/username:password
					final String[] values = credentials.split(":", 2);
					final String username = UtilImpl.processStringValue(values[0]);
//					final String password = UtilImpl.processStringValue(values[1]);
					// TODO check password...
					user = ProvidedRepositoryFactory.get().retrieveUser(username);
					if (user != null) {
						setSessionId(user, request);
					}
					AbstractPersistence.get().setUser(user);
				}				
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
	
	/**]
	 * Set the session ID if it is established.
	 */
	public static void setSessionId(@Nonnull UserImpl user, @Nonnull HttpServletRequest request) {
		HttpSession session = request.getSession(false);
		if (session != null) {
			user.setSessionId(session.getId());
		}
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
		Module admin = c.getModule(AppConstants.ADMIN_MODULE_NAME);
		Document changePassword = admin.getDocument(c, AppConstants.CHANGE_PASSWORD_DOCUMENT_NAME);
		Bean bean = changePassword.newInstance(user);
		BindUtil.set(bean, AppConstants.OLD_PASSWORD_ATTRIBUTE_NAME, oldPassword);
		BindUtil.set(bean, AppConstants.NEW_PASSWORD_ATTRIBUTE_NAME, newPassword);
		BindUtil.set(bean, AppConstants.CONFIRM_PASSWORD_ATTRIBUTE_NAME, confirmPassword);

		AbstractPersistence persistence = AbstractPersistence.get();
		persistence.setUser(user); // user has not been set as this is called directly from changePassword.jsp
		persistence.begin();
		try {
			changePassword.getServerSideAction(c, AppConstants.MAKE_PASSWORD_CHANGE_ACTION_NAME, true).execute(bean, null);
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
			DocumentQuery q = p.newDocumentQuery(AppConstants.ADMIN_MODULE_NAME, AppConstants.USER_DOCUMENT_NAME);
			q.getFilter().addEquals(Binder.createCompoundBinding(AppConstants.CONTACT_ATTRIBUTE_NAME, AppConstants.EMAIL1_ATTRIBUTE_NAME), email);
			
			// set reset password token for all users with the same email address across all customers
			List<PersistentBean> users = q.beanResults();
			if (! users.isEmpty()) {
				GeoIPService geoip = EXT.getGeoIPService();
				if (geoip.isBlocking()) {
					HttpServletRequest request = EXT.getHttpServletRequest();
					String clientIPAddress = SecurityUtil.getSourceIpAddress(request);
					IPGeolocation geolocation = geoip.geolocate(clientIPAddress);
					if (geolocation.isBlocked()) {
						String message = "Password reset request failed because country " + geolocation.countryCode() +
											(geoip.isWhitelist() ?  " is not on the whitelist" : " is on the blacklist") + 
											". Suspected bot submission for user with email " + email;
						Util.LOGGER.warning(message);
						for (PersistentBean user : users) {
							// Record security event for this user
							String userName = (String) Binder.get(user, AppConstants.USER_NAME_ATTRIBUTE_NAME);
							User metaUser = CORE.getRepository().retrieveUser(userName);
							if (metaUser == null) {
								Util.LOGGER.warning("Failed to retrieve user with username " + userName + ", and therefore cannot create security log entry.");
							}
							else {
								SecurityUtil.log("GEO IP Block", message, metaUser);
							}
						}
						return; // Pass silently
					}
				}
				
				PersistentBean firstUser = null;
				String passwordResetToken = generatePasswordResetToken();
				org.skyve.domain.types.Timestamp now = new org.skyve.domain.types.Timestamp();
				for (PersistentBean user: users) {
					Binder.set(user, AppConstants.PASSWORD_RESET_TOKEN_ATTRIBUTE_NAME, passwordResetToken);
					Binder.set(user, AppConstants.PASSWORD_RESET_TOKEN_CREATION_TIMESTAMP_ATTRIBUTE_NAME, now);
					p.upsertBeanTuple(user);
					if (firstUser == null) {
						firstUser = user;
					}
				}

				// send a single email to the user's email address 
				// (if multiple user with the same email, only 1 email should be sent)
				Module m = c.getModule(AppConstants.ADMIN_MODULE_NAME);
				Document d = m.getDocument(c, AppConstants.CONFIGURATION_DOCUMENT_NAME);
				Bean configuration = d.newInstance(u);
				String subject = (String) Binder.get(configuration, AppConstants.PASSWORD_RESET_EMAIL_SUBJECT_ATTRIBUTE_NAME);
				if (subject == null) { // should never happen
					throw new IllegalStateException(AppConstants.ADMIN_MODULE_NAME + '.' + AppConstants.CONFIGURATION_DOCUMENT_NAME + '.' + AppConstants.PASSWORD_RESET_EMAIL_SUBJECT_ATTRIBUTE_NAME + " is null.");
				}
				subject = Binder.formatMessage(subject, firstUser);
				String body = (String) Binder.get(configuration, AppConstants.PASSWORD_RESET_EMAIL_BODY_ATTRIBUTE_NAME); 
				if (body == null) { // should never happen
					throw new IllegalStateException(AppConstants.ADMIN_MODULE_NAME + '.' + AppConstants.CONFIGURATION_DOCUMENT_NAME + '.' + AppConstants.PASSWORD_RESET_EMAIL_BODY_ATTRIBUTE_NAME + " is null.");
				}
				
				body = body.replace("{#resetPasswordUrl}", Util.getResetPasswordUrl());
				// keeping this for backwards compatibility
				body = body.replace("{url}", Util.getSkyveContextUrl());
				
				body = Binder.formatMessage(body, firstUser);
				String fromEmail = (String) Binder.get(configuration, AppConstants.FROM_EMAIL_ATTRIBUTE_NAME);
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
		Timestamp tokenCreated = null;
		String errorMsg = null;

		try (Connection c = EXT.getDataStoreConnection()) {
			// Get password reset token expiry minutes
			try (PreparedStatement s2 = c.prepareStatement(String.format("select %s from ADM_Configuration",
					AppConstants.PASSWORD_RESET_TOKEN_EXPIRY_MINUTES_ATTRIBUTE_NAME))) {
				try (ResultSet rs2 = s2.executeQuery()) {
					Integer expiryMinutes = null;
					if (rs2.next()) {
						int value = rs2.getInt(1);
						if (!rs2.wasNull()) {
							expiryMinutes = Integer.valueOf(value);
						}
					}

					// Get user by token
					try (PreparedStatement s = c
							.prepareStatement(String.format("select %s, %s, %s from ADM_SecurityUser where %s = ?",
									Bean.CUSTOMER_NAME,
									AppConstants.USER_NAME_ATTRIBUTE_NAME,
									AppConstants.PASSWORD_RESET_TOKEN_CREATION_TIMESTAMP_ATTRIBUTE_NAME,
									AppConstants.PASSWORD_RESET_TOKEN_ATTRIBUTE_NAME))) {
						s.setString(1, passwordResetToken);

						try (ResultSet rs = s.executeQuery()) {
							if (!rs.isBeforeFirst()) {
								return Util.i18n("exception.passwordResetLinkInvalid");
							}
							while (rs.next()) {
								customerName = rs.getString(1);
								userName = rs.getString(2);
								tokenCreated = rs.getTimestamp(3);
								
								// Check for expiry of token
								boolean expired = false;
								if (expiryMinutes != null) {
									Timestamp now = Timestamp.from(Instant.now());
									if (tokenCreated != null) {
										TimeUtil.addMinutes(tokenCreated, expiryMinutes.intValue());
										if (now.after(tokenCreated)) {
											expired = true;
										}
									}
								}
								if (!expired) {
									Repository r = CORE.getRepository();
									org.skyve.metadata.user.User u = r.retrieveUser(String.format("%s/%s", customerName, userName));
									errorMsg = makePasswordChange(u, null, newPassword, confirmPassword);
								} else {
									return Util.i18n("exception.passwordResetTokenExpired");
								}
							}
						}
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
											WebContext webContext)
	throws NoResultsException, SecurityException {
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
		if ((result == null) && referenceDocument.isPersistable()) {
			result = persistence.retrieve(referenceDocument, bizId);
		}
		if (result == null) {
			throw new NoResultsException();
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
			else {
				result = OWASP.sanitise(Sanitisation.text, result); // protect reflected XSS in referer header
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
		Module admin = cust.getModule(AppConstants.ADMIN_MODULE_NAME);
		Document selfRegistration = admin.getDocument(cust, AppConstants.SELF_REGISTRATION_DOCUMENT_NAME);
		Bean bean = selfRegistration.newInstance(CORE.getUser());
		BindUtil.set(bean, Binder.createCompoundBinding(AppConstants.USER_ATTRIBUTE_NAME, Bean.DOCUMENT_ID), userBizId);

		AbstractPersistence persistence = AbstractPersistence.get();
		persistence.setUser(CORE.getUser()); // user has not been set as this is called directly from changePassword.jsp
		persistence.begin();
		try {
			selfRegistration.getServerSideAction(cust, AppConstants.RESEND_ACTIVATION_ACTION_NAME, true).execute(bean, null);
		}
		catch (Exception e) {
			persistence.rollback();
			e.printStackTrace();
		}
		finally {
			persistence.commit(true);
		}
	}

	/**
	 * Validate the recaptcha response with google.
	 * @param response	The response from the recaptcha control.
	 * @return true if valid, otherwise false.
	 */
	public static boolean validateRecaptcha(String response) {
		boolean valid = true;
		String recaptchaSecretKey = null;
		
		// Use either google recaptcha or cloudflare turnstile secret key
		if (UtilImpl.GOOGLE_RECAPTCHA_SECRET_KEY != null) {
			recaptchaSecretKey = UtilImpl.GOOGLE_RECAPTCHA_SECRET_KEY;
		}
		else if (UtilImpl.CLOUDFLARE_TURNSTILE_SECRET_KEY != null) {
			recaptchaSecretKey = UtilImpl.CLOUDFLARE_TURNSTILE_SECRET_KEY;
			// Because turnstile secret key is necessary set valid to false in case it's null		
			valid = false;
		}
		
		if (recaptchaSecretKey != null) { // we can validate the response
			valid = false;
			
			if (response != null) { // we have a response to validate
				try {
					URL url = null;
					if (recaptchaSecretKey == UtilImpl.GOOGLE_RECAPTCHA_SECRET_KEY) {
						url = new URL("https://www.google.com/recaptcha/api/siteverify");
					}
					else if (recaptchaSecretKey == UtilImpl.CLOUDFLARE_TURNSTILE_SECRET_KEY) {
						url = new URL("https://challenges.cloudflare.com/turnstile/v0/siteverify");
					}
					if(url != null) {
						URLConnection connection = url.openConnection();
						connection.setDoInput(true);
						connection.setDoOutput(true);
						connection.setUseCaches(false);
						connection.setRequestProperty("Content-Type", "application/x-www-form-urlencoded");
			
						// Create the post body with the required parameters
						StringBuilder postBody = new StringBuilder();
						postBody.append("secret=").append(URLEncoder.encode(recaptchaSecretKey, Util.UTF8));
						postBody.append("&response=").append(URLEncoder.encode(response, Util.UTF8));
			
						try (OutputStream out = connection.getOutputStream()) {
							out.write(postBody.toString().getBytes());
							out.flush();
						}
			
						try (BufferedReader rd = new BufferedReader(new InputStreamReader(connection.getInputStream()))) {
							StringBuilder result = new StringBuilder();
							String line;
							while ((line = rd.readLine()) != null) {
								result.append(line);
							}
			
							@SuppressWarnings("unchecked")
							Map<String, Object> json = (Map<String, Object>) JSON.unmarshall(result.toString());
							valid = Boolean.TRUE.equals(json.get("success"));
						}
					}
				}
				catch (Exception e) {
					// NB valid is already false here
					e.printStackTrace();
				}
			}
		}
		
		return valid;
	}
}
