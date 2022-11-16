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
import org.skyve.domain.types.Timestamp;
import org.skyve.impl.util.UtilImpl;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.provisioning.JdbcUserDetailsManager;
import org.springframework.security.web.authentication.UsernamePasswordAuthenticationFilter;
import org.springframework.security.web.util.matcher.AntPathRequestMatcher;

public abstract class TwoFactorAuthPushFilter extends UsernamePasswordAuthenticationFilter {
	private static final AntPathRequestMatcher DEFAULT_ANT_PATH_REQUEST_MATCHER = new AntPathRequestMatcher("/loginAttempt", "POST");

	public static final String SKYVE_SECURITY_FORM_CUSTOMER_KEY = "customer";
	
	public static String TWO_FACTOR_TOKEN_ATTRIBUTE = "tfaToken";
	public static String USER_ATTRIBUTE = "user"; 
	public static String CUSTOMER_ATTRIBUTE = "customer";
	
	public static String REMEMBER_ATTRIBUTE = "remember";
	
	// this is from the SpringSecurityConfig.remember me.
	public static String REMEMBER_PARAMETER = "remember";
	
	private JdbcUserDetailsManager userDetailsService;

	public TwoFactorAuthPushFilter(AuthenticationManager authenticationManager, UserDetailsService userDetailsService) {
		super(authenticationManager);
		this.setRequiresAuthenticationRequestMatcher(DEFAULT_ANT_PATH_REQUEST_MATCHER);
		
		if (userDetailsService instanceof JdbcUserDetailsManager) {
			this.userDetailsService = (JdbcUserDetailsManager) userDetailsService;
		} else {
			UtilImpl.LOGGER.warning("UserDetailsService is the wrong type for TFA");
		}
	}
	
	@Override
	public void doFilter(ServletRequest request, ServletResponse response, FilterChain chain)
	throws IOException, ServletException {
		doFilter((HttpServletRequest) request, (HttpServletResponse) response, chain);
	}

	private void doFilter(HttpServletRequest request, HttpServletResponse response, FilterChain chain) 
	throws IOException, ServletException{
		if (! DEFAULT_ANT_PATH_REQUEST_MATCHER.matches(request)) {
			chain.doFilter(request, response);
			return;
		}
		
		String twoFactorToken = UtilImpl.processStringValue(request.getParameter(TWO_FACTOR_TOKEN_ATTRIBUTE));
		if (twoFactorToken != null) {
			boolean stopSecFilterChain = doTFACodeCheckProcess(request,response);
			
			if (! stopSecFilterChain) {
				chain.doFilter(request, response);
			}
			return;
		}
		
		// if it gets to here, there is no two factor token.
		// take the opportunity in this method to clear the old TFA details if they exist;
		if (customerRequiresTFAPushNotification() ) {
			boolean stopSecFilterChain = doPushNotificationProcess(request, response);
			
			if (! stopSecFilterChain) {
				chain.doFilter(request, response);
			}
			return;
		}

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
	 * @return true: halt at this filter. false: continue to next security filter.
	 * @throws IOException
	 * @throws ServletException
	 */
	private boolean doPushNotificationProcess(HttpServletRequest request, HttpServletResponse response) 
	throws IOException, ServletException {
		String username = obtainUsername(request);
		String customer = obtainCustomer(request);
		
		UserTFA user = getUserDB(username);
		
		// if cannot authenticate, let the security chain continue
		// don't send the push notification
		// let whoever handles incorrect credentials take over
		// i.e fall out of this filter without doing anything (call chain.doFilter(...))
		if (canAuthenticateWithPassword(request, user)) {
			UtilImpl.LOGGER.info("Sending 2fa code push notification "); 
			
			// save previous selected remember me token so it can be sent back
			boolean rememberMe = request.getParameter(REMEMBER_PARAMETER) != null;
			
			String twoFactorCodeClearText = generateTFACode();
			
			Timestamp generatedTS = new Timestamp();
			user.setTfaCodeGeneratedTimestamp(generatedTS);
			user.setTfaCode(EXT.hashPassword(twoFactorCodeClearText));
			user.setTfaToken(generateTFAPushId(generatedTS));
			
			updateUserTFADetails(user);
			
			pushNotification(user, twoFactorCodeClearText);

			// redirect to 2FA code entry page
			TwoFactorAuthenticationForwardHandler handler = new TwoFactorAuthenticationForwardHandler("/login");

			request.setAttribute(CUSTOMER_ATTRIBUTE, customer);
			request.setAttribute(TWO_FACTOR_TOKEN_ATTRIBUTE,  user.getTfaToken());
			request.setAttribute(USER_ATTRIBUTE, user.getUser());
			request.setAttribute(REMEMBER_ATTRIBUTE, Boolean.valueOf(rememberMe));
			handler.onAuthenticationFailure(request, response, new TwoFactorAuthRequiredException("OTP sent", false));
			return true;
		}
		
		clearTFADetails(user);
		// incorrect creds, follow thorugh for normal incorrect creds path.
		return true;
	}
	
	/**
	 * if everything checks out and its just the code which is incorrect, then let the user enter the code again.
	 * 
	 * @param request
	 * @param response
	 * @return true: halt at this filter. false: continue to next security filter.
	 * @throws IOException
	 * @throws ServletException
	 */
	private boolean doTFACodeCheckProcess(HttpServletRequest request, HttpServletResponse response) 
	throws IOException, ServletException {
		String twoFactorToken = request.getParameter(TWO_FACTOR_TOKEN_ATTRIBUTE);
		String username = obtainUsername(request);
		String tfaCode = obtainPassword(request);
		UserTFA user = getUserDB(username);
		
		if (! tfaCodesPopulated(user) || ! twoFactorToken.equals(user.getTfaToken()) || tfaCodeExpired(user.getTfaCodeGeneratedTimestamp())) {
			UtilImpl.LOGGER.info(String.format("Inconsistent TFA details. TFA Codes populated : %s, TFA Token matches: %s, TFA Code Expired: %s", 
												String.valueOf(tfaCodesPopulated(user)),
												String.valueOf(twoFactorToken.equals(user.getTfaToken())),
												String.valueOf(tfaCodeExpired(user.getTfaCodeGeneratedTimestamp()))));
			return false;
		}
		
		// have the right token, check the code, if the code is wrong, show a nice error message and allow them to retry
		if (! EXT.checkPassword(tfaCode, user.getTfaCode())) {
			UtilImpl.LOGGER.info("Provided TFA code does not match."); 
			
			// check if this keeps the old attributes (if not set them again)
			TwoFactorAuthenticationForwardHandler handler = new TwoFactorAuthenticationForwardHandler("/login");
			
			request.setAttribute(CUSTOMER_ATTRIBUTE, user.getCustomer());
			request.setAttribute(TWO_FACTOR_TOKEN_ATTRIBUTE,  user.getTfaToken());
			request.setAttribute(USER_ATTRIBUTE, user.getUser());
			handler.onAuthenticationFailure(request, response, new TwoFactorAuthRequiredException("OTP sent", true));
			return true;
		}
		
		return false;
	}
	
	private void clearTFADetails(UserTFA user) {
		boolean edited = false;
		
		if (user.getTfaCodeGeneratedTimestamp() != null) {
			user.setTfaCodeGeneratedTimestamp(null);
			edited = true;
		}
		
		if (user.getTfaCode() != null) {
			user.setTfaCode(null);
			edited = true;
		}
		
		if (user.getTfaToken() != null) {
			user.setTfaToken(null);
			edited = true;
		}

		if (edited) {
			updateUserTFADetails(user);
		}
	}
	
	private static String obtainCustomer(HttpServletRequest request) {
		return request.getParameter(SKYVE_SECURITY_FORM_CUSTOMER_KEY);
	}
	
	/**
	 * send the push notification
	 */
	protected abstract void pushNotification(UserTFA user, String code);
	
	/**
	 * Get the user details required for this filter
	 * 
	 * @param username	customer/username
	 * @return
	 * @throws Exception 
	 */
	protected UserTFA getUserDB(String username) {
		UserDetails userDetails = userDetailsService.loadUserByUsername(username);
		
		if (userDetails instanceof UserTFA) {
			return (UserTFA) userDetails;
		}
		Exception e = new Exception("Two Factor Authentication expects the user details service : skyve.jdbcUserTFADetailsService()");
		e.printStackTrace();
		return null;
	}
	
	protected void updateUserTFADetails(UserTFA user) {
		userDetailsService.updateUser(user);
	}
	
	@SuppressWarnings("static-method")
	protected boolean customerRequiresTFAPushNotification() {
		return true;
	}
	
	@SuppressWarnings("static-method")
	protected String generateTFAPushId(Timestamp generatedTS) {
		return UUID.randomUUID().toString() + "-" + Long.toString(generatedTS.getTime());
	}
	
	@SuppressWarnings("static-method")
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
	protected boolean canAuthenticateWithPassword(HttpServletRequest request, UserTFA user) {
		String password = obtainPassword(request);
		password = (password != null) ? password : "";
		
		return EXT.checkPassword(password, user.getUserPassword());
	}
	
	@SuppressWarnings("static-method")
	protected boolean tfaCodeExpired(Timestamp generatedTS) {
		// timeout worth of milliseconds
		long expiryMillis = UtilImpl.TWO_FACTOR_CODE_TIMEOUT_SECONDS * 1000;
		
		long generatedTime = generatedTS.getTime();
		long currentTime = new DateTime().getTime();
		
		return currentTime > (generatedTime + expiryMillis);
	}
	
	/**
	 * Check the user and see if they have the necessary TFA codes populated
	 * 
	 * @param user
	 * @return
	 */
	@SuppressWarnings("static-method")
	protected boolean tfaCodesPopulated(UserTFA user) {
		return ((user.getTfaCode() != null) &&
				(user.getTfaToken() != null) &&
				(user.getTfaCodeGeneratedTimestamp() != null));
	}
}
