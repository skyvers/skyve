package org.skyve.impl.web;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.net.URL;
import java.net.URLConnection;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.security.Principal;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.Timestamp;
import java.time.Instant;
import java.util.Base64;
import java.util.Enumeration;
import java.util.List;
import java.util.Map;
import java.util.UUID;

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
import org.skyve.metadata.repository.ProvidedRepository;
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
import org.skyve.util.logging.SkyveLoggerFactory;
import org.skyve.web.WebContext;
import org.slf4j.Logger;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.Cookie;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.servlet.http.HttpSession;

/**
 * Provides utility methods shared by Skyve web rendering and request handling paths.
 */
public class WebUtil {
    private static final Logger LOGGER = SkyveLoggerFactory.getLogger(WebUtil.class);
	private static final String CONCURRENT_SESSION_EVENT_TYPE = "Concurrent Session";

	private WebUtil() {
		// Disallow instantiation.
	}
	
	/**
	 * Resolves and binds the active user for a request from session, principal, or basic-auth fallback.
	 *
	 * @param request active HTTP request
	 * @param userPrincipal optional authenticated principal name
	 * @return resolved user, or {@code null} when no authenticated user is available
	 */
	@SuppressWarnings({"java:S3776", "java:S1871"}) // complexity OK
	public static User processUserPrincipalForRequest(@Nonnull HttpServletRequest request,
														@Nullable String userPrincipal) {
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
				addSessionAndAuditConcurrentSessionWarning(user, request, session);
				AbstractPersistence.get().setUser(user);
				
				// Get IP address of user to record in UserLoginRecord
				String userIPAddress = SecurityUtil.getSourceIpAddress(request);
				WebStatsUtil.recordLogin(user, userIPAddress);
				Customer customer = user.getCustomer();
				if (customer instanceof CustomerImpl c) {
					c.notifyLogin(user, session);
				}
			}
			// TODO hack!
			else { // check basic auth
				final String authorization = request.getHeader("Authorization");
				if ((authorization != null) && authorization.startsWith("Basic")) {
					// Authorization: Basic base64credentials
					final String base64Credentials = authorization.substring("Basic".length()).trim();
					String credentials = new String(Base64.getMimeDecoder().decode(base64Credentials), StandardCharsets.UTF_8);
	
					// credentials = username:password or customer/username:password
					final String[] values = credentials.split(":", 2);
					final String username = UtilImpl.processStringValue(values[0]);
//					final String password = UtilImpl.processStringValue(values[1]);
					// TODO check password...
					user = ProvidedRepositoryFactory.get().retrieveUser(username);
					if (user != null) {
						setSessionId(user, request);
						AbstractPersistence.get().setUser(user);
					}
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
	
	/**
	 * Resolves the conversation bean targeted by the request binding and optional row identifier.
	 *
	 * @param webContext current web context, or {@code null}
	 * @param request active HTTP request
	 * @return resolved bean for the request context, or {@code null}
	 */
	public static @Nullable Bean getConversationBeanFromRequest(@Nullable AbstractWebContext webContext,
																	@Nonnull HttpServletRequest request) {
		// Find the context bean
		// Note - if there is no form in the view then there is no web context
		Bean result = null;
		if (webContext != null) {
    		result = webContext.getNullableCurrentBean();

			String bizId = request.getParameter(Bean.DOCUMENT_ID); 
	    	String formBinding = request.getParameter(AbstractWebContext.BINDING_NAME);
    		if (formBinding != null) { // sub-form
    			@SuppressWarnings("null") // if formBindinding not null, then unsantised is not null
				@Nonnull String nonNullFormBinding = BindUtil.unsanitiseBinding(formBinding);
    			// find the process bean
    			if (result != null) {
	    			Object referenceValue = BindUtil.get(result, nonNullFormBinding);
	        		if (referenceValue instanceof List<?>) {
	        			result = BindUtil.getElementInCollection(result, nonNullFormBinding, bizId); 
	        		}
	        		else {
	        			result = (Bean) referenceValue;
	        		}
    			}
        	}
		}
        
        return result;
	}
	
	/**
	 * Resolves customer context without requiring an authenticated session.
	 * Preference order is configured customer first, then sanitized customer cookie.
	 *
	 * @param request active HTTP request
	 * @return resolved customer name, or {@code null} when none can be determined
	 */
	@SuppressWarnings("javasecurity:S5146") // false positive: the customer name is not user controlled as it is either set by configuration or from a cookie that is protected against tampering by OWASP sanitisation
	public static @Nullable String determineCustomerWithoutSession(@Nonnull HttpServletRequest request) {
		String result = UtilImpl.CUSTOMER;

		if (result == null) { // no-one is logged in and its not set by configuration
			// See if the customer name is supplied as a cookie
			Cookie[] cookies = request.getCookies();
			if (cookies != null) {
				for (int i = 0, l = cookies.length; i < l; i++) {
					Cookie cookie = cookies[i];
					if (AbstractWebContext.CUSTOMER_COOKIE_NAME.equals(cookie.getName())) {
						result = OWASP.sanitise(Sanitisation.text, cookie.getValue()); // protect against cookie tampering
						break;
					}
				}
			}
		}
		
		return result;
	}
	
	/**
	 * Copies the current HTTP session identifier onto the supplied user when a session already exists.
	 *
	 * <p>Side effects: mutates {@code user} by updating its stored session identifier. This method does not
	 * create a new servlet session.
	 *
	 * @param user user to update
	 * @param request active HTTP request
	 */
	public static void setSessionId(@Nonnull UserImpl user, @Nonnull HttpServletRequest request) {
		HttpSession session = request.getSession(false);
		if (session != null) {
			user.setSessionId(session.getId());
		}
	}

	/**
	 * Registers the current session for the user and logs a concurrent-session warning when policy allows.
	 * This method is invoked only at session establishment points.
	 *
	 * @param user The authenticated or asserted user for the session.
	 * @param request The active HTTP request.
	 * @param session The HTTP session being registered.
	 */
	public static void addSessionAndAuditConcurrentSessionWarning(@Nonnull User user,
																	@Nonnull HttpServletRequest request,
																	@Nonnull HttpSession session) {
		boolean sessionAlreadyRegistered = StateUtil.checkSession(user.getId(), session);
		int existingSessionCount = StateUtil.getSessionCount(user.getId());
		boolean hasOtherSession = StateUtil.hasOtherSession(user.getId(), session);
		StateUtil.addSession(user.getId(), session);

		boolean shouldEvaluateEligibility = UtilImpl.CONCURRENT_SESSION_WARNINGS &&
											(! sessionAlreadyRegistered) &&
											hasOtherSession;
		boolean eligibleForWarning = false;
		if (shouldEvaluateEligibility) {
			try {
				eligibleForWarning = isConcurrentSessionWarningEligible(user, request);
			}
			catch (RuntimeException e) {
				LOGGER.warn("Could not determine concurrent session warning eligibility for user {}.", user.getName(), e);
			}
		}

		if (shouldLogConcurrentSessionWarning(UtilImpl.CONCURRENT_SESSION_WARNINGS,
												sessionAlreadyRegistered,
												hasOtherSession,
												eligibleForWarning)) {
			logConcurrentSessionWarning(user, existingSessionCount);
		}
	}

	private static void logConcurrentSessionWarning(@Nonnull User user, int existingSessionCount) {
		try {
			SecurityUtil.log(CONCURRENT_SESSION_EVENT_TYPE,
								buildConcurrentSessionWarningMessage(existingSessionCount),
								user,
								UtilImpl.CONCURRENT_SESSION_NOTIFICATIONS);
		}
		catch (RuntimeException e) {
			LOGGER.warn("Could not log concurrent session warning for user {}.", user.getName(), e);
		}
	}

	static boolean shouldLogConcurrentSessionWarning(boolean concurrentSessionWarningsEnabled,
														boolean sessionAlreadyRegistered,
														boolean hasOtherSession,
														boolean eligibleForWarning) {
		return concurrentSessionWarningsEnabled && (! sessionAlreadyRegistered) && hasOtherSession && eligibleForWarning;
	}

	/**
	 * Builds a human-readable concurrent-session warning message.
	 *
	 * @param existingSessionCount number of existing sessions before adding the current one
	 * @return warning message text
	 */
	static @Nonnull String buildConcurrentSessionWarningMessage(int existingSessionCount) {
		return "User logged in while another active session already existed. Existing session count: " + existingSessionCount + '.';
	}

	/**
	 * Determines whether a session is eligible for concurrent-session warning auditing.
	 * Public-form sessions and unauthenticated requests are excluded.
	 *
	 * @param user The current user.
	 * @param request The active HTTP request.
	 * @return {@code true} when the session should be considered for concurrent-session warnings.
	 */
	public static boolean isConcurrentSessionWarningEligible(@Nonnull User user, @Nonnull HttpServletRequest request) {
		return isConcurrentSessionWarningEligible(user, request.getUserPrincipal(), ProvidedRepositoryFactory.get());
	}

	/**
	 * Determines whether a session is eligible for concurrent-session warning auditing.
	 *
	 * @param user current user
	 * @param principal authenticated request principal, or {@code null}
	 * @param repository repository used to resolve customer public-user identity
	 * @return {@code true} when warnings are eligible for this user/principal combination
	 */
	static boolean isConcurrentSessionWarningEligible(@Nonnull User user,
														@Nullable Principal principal,
														@Nonnull ProvidedRepository repository) {
		return (principal != null) && (! isPublicUser(user, repository));
	}

	/**
	 * Determines whether the provided user is the configured public user for the customer.
	 *
	 * @param user current user
	 * @param repository repository used to resolve public-user name
	 * @return {@code true} when the user matches the customer's public-user account
	 */
	static boolean isPublicUser(@Nonnull User user, @Nonnull ProvidedRepository repository) {
		String customerName = user.getCustomerName();
		if (customerName == null) {
			return false;
		}

		String publicUserName = repository.retrievePublicUserName(customerName);
		return (publicUserName != null) && publicUserName.equals(user.getName());
	}
	
	/**
	 * Delete all cookies (except the customer cookie which could be useful next time) or selected cookies by name.
	 * If names varargs isn't used all cookies bar the customer cookie are deleted.
	 * If names varargs is used, only those cookies are deleted.
	 *
	 * @param request active HTTP request
	 * @param response active HTTP response
	 * @param names optional explicit cookie names to delete
	 */
	@SuppressWarnings("java:S3776") // complexity OK
	public static void deleteCookies(@Nonnull HttpServletRequest request,
										@Nonnull HttpServletResponse response,
										@Nonnull String... names) {
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
	 *
	 * @param request active HTTP request
	 * @param response active HTTP response
	 */
	public static void deleteMenuCookies(@Nonnull HttpServletRequest request,
											@Nonnull HttpServletResponse response) {
		// Don't include "ecuador_expandeditems", "ultima_expandeditems" as the menus don't respond well.
		// Ecuador Menu - accordions do not open at all when expanded on server menu model and no cookie set
		// Ultima Menu - Selecting a different menu item within the same module makes the module accordion collapse
		deleteCookies(request, response, "panelMenu-leftMenu");
	}
	
	/**
	 * Really logout of the app - logout, invalidate session and remove all cookies.
	 *
	 * @param request active HTTP request
	 * @param response active HTTP response
	 * @throws ServletException when container logout fails
	 */
	public static void logout(@Nonnull HttpServletRequest request,
								@Nonnull HttpServletResponse response) 
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
	 * Executes the change-password server-side action for the supplied user.
	 *
	 * <p>The action is run inside a fresh persistence transaction because this helper is called directly from
	 * the JSP flow rather than from a fully prepared web action context.
	 *
	 * @return the first validation error message when the password change is rejected, or {@code null} when
	 * the password change succeeds
	 * @param user target user
	 * @param oldPassword existing password, or {@code null}
	 * @param newPassword new password value
	 * @param confirmPassword confirmation password value
	 * @throws Exception when the underlying server-side action fails unexpectedly
	 */
	public static @Nullable String makePasswordChange(@Nonnull User user,
														@Nullable String oldPassword,
														@Nonnull String newPassword,
														@Nonnull String confirmPassword) 
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
				errorMessage = "The password change could not be completed.";
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
	 * Issues password-reset tokens for all users matching the supplied customer/email pair and sends one reset email.
	 *
	 * <p>Side effects: writes reset-token state to matching security-user records, may record GeoIP security
	 * events for blocked submissions, and sends a single outbound email when at least one eligible user exists.
	 * The method is intentionally silent when no user matches the email address.
	 *
	 * @param customer customer name
	 * @param email target email address
	 * @throws Exception when token generation, persistence, or mail delivery fails unexpectedly
	 */
	@SuppressWarnings("java:S3776") // complexity OK
	public static void requestPasswordReset(@Nonnull String customer, @Nonnull String email)
	throws Exception {
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
											". Suspected bot submission for user with email " + OWASP.sanitiseLog(email);
						LOGGER.warn(message);
						for (PersistentBean user : users) {
							// Record security event for this user
							String userName = (String) Binder.get(user, AppConstants.USER_NAME_ATTRIBUTE_NAME);
							User metaUser = (userName == null) ? null : CORE.getRepository().retrieveUser(userName);
							if (metaUser == null) {
								LOGGER.warn("Failed to retrieve user with username {}, and therefore cannot create security log entry.", userName);
							}
							else {
								SecurityUtil.log("GEO IP Block", message, metaUser, UtilImpl.GEO_IP_BLOCK_NOTIFICATIONS);
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
				if (firstUser == null) { // should never happen
					throw new IllegalStateException("No users found with email " + email);
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
				EXT.getMailService().sendMail(new Mail().addTo(email).from(fromEmail).subject(subject).body(body));
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

	/**
	 * Generates a password-reset token string combining random UUID and current timestamp.
	 *
	 * @return generated reset token
	 */
	public static @Nonnull String generatePasswordResetToken() {
		return UUID.randomUUID().toString() + Long.toString(System.currentTimeMillis());
	}

	/**
	 * Builds a download-action URL for the supplied target document and optional binding context.
	 *
	 * <p>The generated URL includes the download action name, target module/document, web context identifier,
	 * an optional compound binding, optional element bizId for data-widget rows, and a cache-busting timestamp.
	 *
	 * @param downloadActionName download action name
	 * @param targetModuleName target module name
	 * @param targetDocumentName target document name
	 * @param webId web context id
	 * @param viewBinding optional view binding
	 * @param dataWidgetBinding optional data-widget binding
	 * @param elementBizId optional element business id
	 * @return generated download URL
	 */
	public static @Nonnull String getDownloadActionUrl(@Nonnull String downloadActionName,
														@Nonnull String targetModuleName,
														@Nonnull String targetDocumentName,
														@Nonnull String webId,
														@Nullable String viewBinding,
														@Nullable String dataWidgetBinding,
														@Nullable String elementBizId) {
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
	 * Validates a password-reset token and, when still valid, delegates to the password-change workflow.
	 *
	 * <p>The token is resolved directly from the datastore so the method can enforce token expiry without
	 * leaking whether a specific credential exists. Invalid or expired tokens return a localized user-facing
	 * error message instead of throwing.
	 *
	 * @param passwordResetToken password reset token
	 * @param newPassword new password value
	 * @param confirmPassword confirmation password value
	 * @return a localized validation or expiry message when the reset cannot be completed, or {@code null}
	 * when the password reset succeeds
	 * @throws Exception when persistence or password-change execution fails unexpectedly
	 */
	@SuppressWarnings("java:S3776") // complexity OK
	public static @Nullable String resetPassword(@Nonnull String passwordResetToken,
													@Nonnull String newPassword,
													@Nonnull String confirmPassword)
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
								return Util.nullSafeI18n("exception.passwordResetLinkInvalid");
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
								if (! expired) {
									Repository r = CORE.getRepository();
									org.skyve.metadata.user.User u = r.retrieveUser(String.format("%s/%s", customerName, userName));
									// // don't allow credential existence to leak
									if (u == null) {
										return Util.nullSafeI18n("exception.passwordResetTokenExpired");
									}
									errorMsg = makePasswordChange(u, null, newPassword, confirmPassword);
								}
								else {
									return Util.nullSafeI18n("exception.passwordResetTokenExpired");
								}
							}
						}
					}
				}
			}
		}

		return errorMsg;
	}
	
	/**
	 * Resolves an existing referenced bean via bizlet resolution and fallback persistence retrieval.
	 *
	 * @param referenceDocument referenced document metadata
	 * @param bizId business id of the referenced bean
	 * @param persistence active persistence context
	 * @param conversationBean current conversation bean
	 * @param webContext current web context
	 * @return resolved referenced bean
	 * @throws NoResultsException when no referenced bean can be resolved
	 * @throws SecurityException when the current user cannot read the resolved bean
	 */
	// find the existing bean with retrieve
	public static @Nonnull Bean findReferencedBean(@Nonnull Document referenceDocument, 
													@Nonnull String bizId, 
													@Nonnull Persistence persistence,
													@Nullable Bean conversationBean,
													@Nonnull WebContext webContext)
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
                LOGGER.error("Failed to resolve document {}.{} with bizId {}",
                				referenceDocument.getOwningModuleName(), referenceDocument.getName(), bizId);
				throw new MetaDataException("Failed to resolve this " + referenceDocument.getLocalisedSingularAlias(), e);
				
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
	
	/**
	 * Returns a sanitized same-origin Referer header value, or {@code null} when absent/invalid.
	 *
	 * @param request active HTTP request
	 * @return sanitized referer header value, or {@code null}
	 */
	@SuppressWarnings("javasecurity:S5131") // OWASP sanitisation is applied to the referer header value before it is used, so this is not a reflected XSS vulnerability
	public static @Nullable String getRefererHeader(@Nonnull HttpServletRequest request) {
		String result = Util.processStringValue(request.getHeader("referer"));
		if (result != null) {
			if (! result.startsWith(Util.getSkyveContextUrl())) {
				LOGGER.warn("referer header {} looks tampered with because it does not start with {}. This looks like a doctored request because Referrer-Policy should be same-origin!",
								result,
								Util.getSkyveContextUrl());
				result = null;
			}
			else {
				result = OWASP.sanitise(Sanitisation.text, result); // protect reflected XSS in referer header
			}
		}
		return result;
	}

	/**
	 * Appends each supplied value as a UTF-8 encoded query parameter.
	 *
	 * @param result URL under construction
	 * @param name query parameter name
	 * @param values query parameter values, or {@code null} to append nothing
	 */
	public static void appendQueryParameter(@Nonnull StringBuilder result,
											@Nonnull String name,
											@Nullable String[] values) {
		if (values != null) {
			for (String value : values) {
				result.append(result.indexOf("?") < 0 ? '?' : '&');
				result.append(URLEncoder.encode(name, StandardCharsets.UTF_8));
				result.append('=');
				result.append(URLEncoder.encode((value == null) ? "" : value, StandardCharsets.UTF_8));
			}
		}
	}

	/**
	 * Appends all request parameters except the named exclusion to a URL under construction.
	 * Parameter names and values are UTF-8 encoded, repeated values retain their request order,
	 * and {@code null} values are represented by an empty value.
	 *
	 * @param result URL under construction
	 * @param request request supplying the parameters
	 * @param excludedParameterName parameter name to omit
	 */
	public static void appendRequestParameters(@Nonnull StringBuilder result,
											@Nonnull HttpServletRequest request,
											@Nonnull String excludedParameterName) {
		Enumeration<String> parameterNames = request.getParameterNames();
		while (parameterNames.hasMoreElements()) {
			String parameterName = parameterNames.nextElement();
			if (! excludedParameterName.equals(parameterName)) {
				appendQueryParameter(result, parameterName, request.getParameterValues(parameterName));
			}
		}
	}

	/**
	 * Sends a registration email with an activation code to the specified email address.
	 * 
	 * @param userBizId business id of the self-registration user record
	 * @throws Exception when registration email dispatch fails unexpectedly
	 */
	public static void sendRegistrationEmail(final @Nonnull String userBizId) throws Exception {
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
			LOGGER.error("Send registration email failed for userBizId {}", userBizId, e);
		}
		finally {
			persistence.commit(true);
		}
	}

	/**
	 * Validates a Google reCAPTCHA or Cloudflare Turnstile response against the configured verification endpoint.
	 *
	 * <p>When no secret key is configured, the method returns the default configured behavior for the active
	 * provider path. When a secret key is configured, a missing or rejected client response evaluates to
	 * {@code false}.
	 *
	 * @param response the client verification token submitted by the captcha control
	 * @return {@code true} when the remote verification service reports success, otherwise {@code false}
	 */
	@SuppressWarnings("java:S3776") // complexity OK
	public static boolean validateRecaptcha(@Nullable String response) {
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
					if (recaptchaSecretKey.equals(UtilImpl.GOOGLE_RECAPTCHA_SECRET_KEY)) {
						url = new URL("https://www.google.com/recaptcha/api/siteverify");
					}
					else if (recaptchaSecretKey.equals(UtilImpl.CLOUDFLARE_TURNSTILE_SECRET_KEY)) {
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
						postBody.append("secret=").append(URLEncoder.encode(recaptchaSecretKey, StandardCharsets.UTF_8));
						postBody.append("&response=").append(URLEncoder.encode(response, StandardCharsets.UTF_8));
			
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
					LOGGER.warn("Recaptcha validation failed.", e);
				}
			}
		}
		
		return valid;
	}
}
