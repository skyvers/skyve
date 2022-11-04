package org.skyve.impl.web.spring;

import java.io.IOException;
import java.util.UUID;

import javax.servlet.FilterChain;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.skyve.EXT;
import org.skyve.domain.types.DateTime;
import org.skyve.impl.util.UtilImpl;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.web.authentication.UsernamePasswordAuthenticationFilter;
import org.springframework.security.web.util.matcher.AntPathRequestMatcher;

import com.google.common.base.Strings;

public abstract class TwoFactorAuthPushFilter extends UsernamePasswordAuthenticationFilter {
	
	
	protected class UserDB {
		protected String username;
		protected String password;
		protected String twoFactorCode; // hashed
		protected DateTime twoFactorCodeGeneratedDateTime;
		protected String twoFactorToken;
		protected String bizCustomer;
		
		protected boolean tfaExpected() {
			return 
			 !Strings.isNullOrEmpty(twoFactorCode) &&
			 !Strings.isNullOrEmpty(twoFactorToken) &&
			 twoFactorCodeGeneratedDateTime != null;
		}
	}

	// potentially need to do one for smartclient as well
	private static final AntPathRequestMatcher DEFAULT_ANT_PATH_REQUEST_MATCHER = new AntPathRequestMatcher("/loginAttempt",
			"POST");

	public static final String SKYVE_SECURITY_FORM_CUSTOMER_KEY = "customer";
	
//	private String TWOFAParameter = "tfaCode";
	public static String TWO_FACTOR_TOKEN_ATTRIBUTE = "tfaToken";
	public static String USER_ATTRIBUTE = "user";
	public static String AUTH_ERROR_ATTRIBUTE = "error";
	
	
	// probably dont need this, login.jsp can just use if TWO_FACTOR_ID_ATTRIBUTE exists/null
	public static String IS_TWO_FACTOR_ATTRIBUTE = "2FA";
	
	

	public TwoFactorAuthPushFilter(AuthenticationManager authenticationManager) {
		super(authenticationManager);
		this.setRequiresAuthenticationRequestMatcher(DEFAULT_ANT_PATH_REQUEST_MATCHER);
		this.setUsernameParameter("user");
	}
	
	
	@Override
	public void doFilter(ServletRequest request, ServletResponse response, FilterChain chain)
			throws IOException, ServletException {
		
		doFilter((HttpServletRequest) request, (HttpServletResponse) response, chain);
	}

	private void doFilter(HttpServletRequest request, HttpServletResponse response, FilterChain chain)
			throws IOException, ServletException {
		
		if (!DEFAULT_ANT_PATH_REQUEST_MATCHER.matches(request)) {
			chain.doFilter(request, response);
			return;
		}
		
		String username = obtainUsername(request);
		String customer = obtainCustomer(request);
		String password = obtainPassword(request);
		UtilImpl.LOGGER.info("ELTRACEDEV params customer|user|password "  + customer + "|" + username + "|" + password); 
		
		
		String twoFactorToken = request.getParameter(TWO_FACTOR_TOKEN_ATTRIBUTE);
		if (twoFactorToken != null) {
			doTFACodeCheckProcess(request,response);
			return;
		}
		
		
		// if it gets to here, there is no two factor token.
		// take the opportunity in this method to clear the old TFA details if they exist;
		if (customerRequiresTFAPushNotification() ) {
			doPushNotificationProcess(request, response);
			return;
		}

		UtilImpl.LOGGER.info("ELTRACEDEV pass onto normal authentication"); 
		chain.doFilter(request, response);

	}
	
	/**
	 * This gets called when:
	 * - a push notification needs to be sent.
	 * - a user is not performing a 2FA code entry.
	 * 
	 * At the end of this method, the 2FA details will either be updated with the new details or cleared.
	 * 
	 * @param request
	 * @param response
	 * @throws IOException
	 * @throws ServletException
	 */
	private void doPushNotificationProcess(HttpServletRequest request, HttpServletResponse response) 
			throws IOException, ServletException {
		
		String username = obtainUsername(request);
		String customer = obtainCustomer(request);
		
		UserDB user = getUserDB(customer, username);
		
		// if cannot authenticate, let the security chain continue
		// don't send the push notification
		// let whoever handles incorrect credentials take over
		// i.e fall out of this filter without doing anything (call chain.doFilter(...))
		if (canAuthenticateWithPassword(request, user)) {
			UtilImpl.LOGGER.info("ELTRACEDEV send 2fa code push notification"); 
			
			
			String twoFactorCodeClearText = generateTFACode();
			
			DateTime generatedDateTime = new DateTime();
			user.twoFactorCodeGeneratedDateTime = generatedDateTime;
			user.twoFactorCode = EXT.hashPassword(twoFactorCodeClearText);
			user.twoFactorToken = generateTFAPushId(generatedDateTime);
			
			updateUserDB(user);
			
			pushNotifcation(twoFactorCodeClearText);

			// redirect to 2FA code entry page
			TwoFactorAuthenticationForwardHandler handler = new TwoFactorAuthenticationForwardHandler("/login");
//			this should be on the page/cookie?
//			request.setAttribute("customer", "demo");
			request.setAttribute(TWO_FACTOR_TOKEN_ATTRIBUTE,  user.twoFactorToken);
			request.setAttribute(IS_TWO_FACTOR_ATTRIBUTE, "1");
			request.setAttribute(USER_ATTRIBUTE, super.obtainUsername(request));
			handler.onAuthenticationFailure(request, response, new TwoFactorAuthRequiredException("OTP sent", false));
			return;
		} else {
			clearTFADetails(user);
		}
	}
	
	/**
	 * if everything checks out and its just the code which is incorrect, then let the user enter the code again.
	 * 
	 * 
	 * @param request
	 * @param response
	 * @throws IOException
	 * @throws ServletException
	 */
	private void doTFACodeCheckProcess(HttpServletRequest request, HttpServletResponse response) 
			throws IOException, ServletException {
		
		String twoFactorToken = request.getParameter(TWO_FACTOR_TOKEN_ATTRIBUTE);
		
		if (Strings.isNullOrEmpty(twoFactorToken)) {
			// should not have entered this method
			return;
		}
		
		String username = obtainUsername(request);
		String customer = obtainCustomer(request);
		String tfaCode = obtainPassword(request);
		UserDB user = getUserDB(customer, username);
		
		if (!user.tfaExpected() || !twoFactorToken.equals(user.twoFactorToken) || tfaCodeExpired(user.twoFactorCodeGeneratedDateTime)) {
			// fall straight through and do default auth failure
			
			// maybe we can do something nice if the code has expired
			return;
		}
		
		
		// have the right token, check the code, if the code is wrong, show a nice error message and allow them to retry
		if (!EXT.checkPassword(tfaCode, user.password)) {
			// check if this keeps the old attributes (if not set them again)
			TwoFactorAuthenticationForwardHandler handler = new TwoFactorAuthenticationForwardHandler("/login");
			request.setAttribute(AUTH_ERROR_ATTRIBUTE, "1");
			handler.onAuthenticationFailure(request, response, new TwoFactorAuthRequiredException("OTP sent", true));
			return;
		}
	}
	
	private void clearTFADetails(UserDB user) {
		
		if (user.twoFactorCodeGeneratedDateTime != null) {
			user.twoFactorCodeGeneratedDateTime = null;
			user.twoFactorCode = null;
			user.twoFactorToken = null;
			
			updateUserDB(user);
		}
		
		
	}
	
	private String obtainCustomer(HttpServletRequest request) {
		return request.getParameter(SKYVE_SECURITY_FORM_CUSTOMER_KEY);
	}
	
	/**
	 * send the push notification
	 */
	protected abstract void pushNotifcation(String code);
	
	/**
	 * Get the user details required for this filter
	 * 
	 * @param customerName
	 * @param username
	 * @return
	 */
	protected abstract UserDB getUserDB(String customerName, String username);
	
	protected abstract void updateUserDB(UserDB user);
	
	protected abstract boolean customerRequiresTFAPushNotification() ;
	
	protected String generateTFAPushId(DateTime generatedDateTime) {
		return UUID.randomUUID().toString() + "-" + Long.toString(generatedDateTime.getTime());
	}
	
	protected String generateTFACode() {
		return UUID.randomUUID().toString();
	}
	
	/**
	 * note: return true here does not log the user in.
	 * Checks the credentials are correct before sending the push notification
	 * 
	 * @param request
	 * @return
	 */
	protected boolean canAuthenticateWithPassword(HttpServletRequest request, UserDB user) {
		String password = obtainPassword(request);
		password = (password != null) ? password : "";
		
		return EXT.checkPassword(password, user.password);
	}
	
	protected boolean tfaCodeExpired(DateTime generatedDateTime) {
		// 5min worth of milliseconds
		long expiryMillis = 300000l;
		
		long generatedTime = generatedDateTime.getTime();
		long currentTime = new DateTime().getTime();
		
		return currentTime > generatedTime + expiryMillis;
	}
}
