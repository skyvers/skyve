package org.skyve.impl.web.spring;

import java.io.IOException;
import java.security.SecureRandom;
import java.text.DecimalFormat;
import java.util.Optional;
import java.util.UUID;
import java.util.regex.Pattern;

import org.skyve.EXT;
import org.skyve.domain.types.DateTime;
import org.skyve.domain.types.Timestamp;
import org.skyve.impl.util.MfaConfigurationUtil;
import org.skyve.impl.util.TwoFactorAuthConfigurationSingleton;
import org.skyve.impl.util.TwoFactorAuthCustomerConfiguration;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.view.TextOutput.Sanitisation;
import org.skyve.util.OWASP;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpMethod;
import org.springframework.security.authentication.AccountExpiredException;
import org.springframework.security.authentication.AuthenticationServiceException;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.core.AuthenticationException;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UsernameNotFoundException;
import org.springframework.security.provisioning.UserDetailsManager;
import org.springframework.security.web.authentication.SimpleUrlAuthenticationFailureHandler;
import org.springframework.security.web.authentication.UsernamePasswordAuthenticationFilter;
import org.springframework.security.web.servlet.util.matcher.PathPatternRequestMatcher;

import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.ServletRequest;
import jakarta.servlet.ServletResponse;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

public abstract class TwoFactorAuthPushFilter extends UsernamePasswordAuthenticationFilter {
	private static final PathPatternRequestMatcher DEFAULT_LOGIN_ATTEMPT_PATH_REQUEST_MATCHER = PathPatternRequestMatcher.withDefaults()
			.matcher(HttpMethod.POST, SkyveSpringSecurity.LOGIN_ATTEMPT_PATH);
	private static final Pattern SIX_DIGIT_TFA_CODE_PATTERN = Pattern.compile("\\d{6}");

    private static final Logger LOGGER = LoggerFactory.getLogger(TwoFactorAuthPushFilter.class);

	public static final String SKYVE_SECURITY_FORM_CUSTOMER_KEY = "customer";
	
	public static String TWO_FACTOR_TOKEN_ATTRIBUTE = "tfaToken";
	public static String USER_ATTRIBUTE = "user"; 
	public static String CUSTOMER_ATTRIBUTE = "customer";
	
	public static String REMEMBER_ATTRIBUTE = "remember";
	public static final String RESEND_ATTRIBUTE = "tfaResend";
	public static final String RESEND_SUCCESS_ATTRIBUTE = "tfaResendSuccess";
	public static final String RESEND_COOLDOWN_ATTRIBUTE = "tfaResendCooldown";
	
	// this is from the SpringSecurityConfig.remember me.
	public static String REMEMBER_PARAMETER = "remember";
	
	private UserDetailsManager userDetailsManager;

	public TwoFactorAuthPushFilter(UserDetailsManager userDetailsManager) {
		setRequiresAuthenticationRequestMatcher(DEFAULT_LOGIN_ATTEMPT_PATH_REQUEST_MATCHER);
		this.userDetailsManager = userDetailsManager;
	}
	
	@Override
	public void doFilter(ServletRequest iRequest, ServletResponse iResponse, FilterChain chain)
	throws IOException, ServletException {
		HttpServletRequest request = (HttpServletRequest) iRequest;
		HttpServletResponse response = (HttpServletResponse) iResponse;
		
		if (skipPushFilter(request, response)) {
			chain.doFilter(request, response);
			return;
		}
		
		doFilter(request, response, chain);
	}
	
	protected boolean skipPushFilter(HttpServletRequest request, HttpServletResponse response) {
		// No two factor customers defined
		if (UtilImpl.TWO_FACTOR_AUTH_CUSTOMERS == null) {
			return true;
		}
		
		// Not a login attempt
		if (! DEFAULT_LOGIN_ATTEMPT_PATH_REQUEST_MATCHER.matches(request)) {
			return true;
		}
		
		String customerName = obtainCustomer(request);
		
		// No customer given or set in JSON or TFA customer not in json
		if ((customerName == null) || (! UtilImpl.TWO_FACTOR_AUTH_CUSTOMERS.contains(customerName))) {
			return true;
		}
		
		// No authentication required
		if (! requiresAuthentication(request, response)) {
			return true;
		}
				
		TwoFactorAuthCustomerConfiguration config = TwoFactorAuthConfigurationSingleton.getInstance().getConfig(customerName);
		return (! isTwoFactorEnabled(config));
	}

	private void doFilter(HttpServletRequest request, HttpServletResponse response, FilterChain chain) 
	throws IOException, ServletException{
		String twoFactorToken = UtilImpl.processStringValue(request.getParameter(TWO_FACTOR_TOKEN_ATTRIBUTE));
		if (twoFactorToken != null) {
			boolean resendRequested = UtilImpl.processStringValue(request.getParameter(RESEND_ATTRIBUTE)) != null;
			boolean stopSecFilterChain = resendRequested ? doResendProcess(request, response) : doTFACodeCheckProcess(request, response);
			
			if (! stopSecFilterChain) {
				chain.doFilter(request, response);
			}
			return;
		}
		
		// if it gets to here, there is no two factor token.
		TwoFactorAuthCustomerConfiguration config = TwoFactorAuthConfigurationSingleton.getInstance().getConfig(obtainCustomer(request));
		if (supportsPushConfiguration(config)) {
			boolean stopSecFilterChain = doPushNotificationProcess(request, response);
			
			if (! stopSecFilterChain) {
				chain.doFilter(request, response);
			}
			return;
		}
		if (isTwoFactorEnabled(config)) {
			doUnsupportedPushConfigurationProcess(request, response, config);
			return;
		}

		chain.doFilter(request, response);
	}

	private void doUnsupportedPushConfigurationProcess(HttpServletRequest request,
														HttpServletResponse response,
														TwoFactorAuthCustomerConfiguration config)
	throws IOException, ServletException {
		LOGGER.warn("No MFA push filter supports the configured type [{}]", config.getTfaType());
		SimpleUrlAuthenticationFailureHandler handler = new SimpleUrlAuthenticationFailureHandler("/login?error");
		handler.onAuthenticationFailure(request, response, new AuthenticationServiceException("Unsupported MFA factor type"));
	}

	@SuppressWarnings("static-method")
	protected boolean isTwoFactorEnabled(TwoFactorAuthCustomerConfiguration config) {
		return (config != null) && (! config.isTfaOff());
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
		String userName = obtainUsername(request);
		String customerName = obtainCustomer(request);
		
		TwoFactorAuthUser user = getUserDB(userName);
		if (user == null) {
			return false;
		}

		if (! isUserMfaRequired(user, TwoFactorAuthCustomerConfiguration.TFA_TYPE_EMAIL)) {
			clearTFADetails(user);
			return false;
		}
		
		// if cannot authenticate, let the security chain continue
		// don't send the push notification
		// let whoever handles incorrect credentials take over
		// i.e fall out of this filter without doing anything (call chain.doFilter(...))
		if (canAuthenticateWithPassword(request, user)) {
			LOGGER.info("Sending 2fa code push notification "); 
			
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
			TwoFactorAuthForwardHandler handler = new TwoFactorAuthForwardHandler("/login");

			request.setAttribute(CUSTOMER_ATTRIBUTE, OWASP.sanitise(Sanitisation.text, customerName));
			request.setAttribute(TWO_FACTOR_TOKEN_ATTRIBUTE,  OWASP.sanitise(Sanitisation.text, user.getTfaToken()));
			request.setAttribute(USER_ATTRIBUTE, user.getUser());
			request.setAttribute(REMEMBER_ATTRIBUTE, Boolean.valueOf(rememberMe));
			handler.onAuthenticationFailure(request, response, new TwoFactorAuthRequiredException("OTP sent", false));
			return true;
		}
		
		clearTFADetails(user);
		return false;
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
		String twoFactorToken = UtilImpl.processStringValue(request.getParameter(TWO_FACTOR_TOKEN_ATTRIBUTE));
		String username = obtainUsername(request);
		TwoFactorAuthUser user = getUserDB(username);
		
		if (user == null) {
			return false;
		}

		if ((! tfaCodesPopulated(user)) || (! twoFactorToken.equals(user.getTfaToken())) ) {
			LOGGER.info("Inconsistent TFA details. TFA Codes populated : {} : Token populated : {}",
							Boolean.valueOf(tfaCodesPopulated(user)),
							Boolean.valueOf(twoFactorToken.equals(user.getTfaToken())));
			return false;
		}
		
		if (tfaCodeExpired(user.getCustomer(), twoFactorToken)) {
			LOGGER.info("Users TFA Code has timed out.");
			SimpleUrlAuthenticationFailureHandler handler = new SimpleUrlAuthenticationFailureHandler("/login");
			handler.onAuthenticationFailure(request, response, new AccountExpiredException("TFA timeout"));
			return true;
		}

		String twoFactorCode = UtilImpl.processStringValue(obtainPassword(request));
		if (! isValidTwoFactorCode(twoFactorCode)) {
			LOGGER.info("Provided TFA code has invalid format.");
		}
		
		try {
			attemptAuthentication(request, response);
		}
		catch (@SuppressWarnings("unused") BadCredentialsException e) {
			// throws error if authentication failed, catch so we want to handle it
			LOGGER.info("Provided TFA code does not match."); 
			return forwardToTwoFactorPage(request, response, user);
		}
		catch (AuthenticationException e) {
			LOGGER.info("TFA authentication failed with {}", e.getClass().getSimpleName());
			SimpleUrlAuthenticationFailureHandler handler = new SimpleUrlAuthenticationFailureHandler("/login?error");
			handler.onAuthenticationFailure(request, response, e);
			return true;
		}
		
		return false;
	}

	private boolean doResendProcess(HttpServletRequest request, HttpServletResponse response)
	throws IOException, ServletException {
		String username = obtainUsername(request);
		String twoFactorToken = UtilImpl.processStringValue(request.getParameter(TWO_FACTOR_TOKEN_ATTRIBUTE));
		if (username == null) {
			LOGGER.warn("Rejecting 2fa resend because the username was missing.");
			redirectToLogin(request, response);
			return true;
		}

		TwoFactorAuthUser user = getUserDB(username);
		if (user == null) {
			LOGGER.warn("Rejecting 2fa resend because user was not found: {}", username);
			redirectToLogin(request, response);
			return true;
		}

		if ((! tfaCodesPopulated(user)) || (! twoFactorToken.equals(user.getTfaToken()))) {
			LOGGER.warn("Rejecting 2fa resend for user {} due to token mismatch.", username);
			redirectToLogin(request, response);
			return true;
		}

		if (tfaCodeExpired(user.getCustomer(), twoFactorToken)) {
			LOGGER.info("Rejecting 2fa resend for user {} because the token has expired.", username);
			SimpleUrlAuthenticationFailureHandler handler = new SimpleUrlAuthenticationFailureHandler("/login");
			handler.onAuthenticationFailure(request, response, new AccountExpiredException("TFA timeout"));
			return true;
		}

		if (!isUserMfaRequired(user, TwoFactorAuthCustomerConfiguration.TFA_TYPE_EMAIL)) {
			LOGGER.info("Rejecting 2fa resend for user {} because EMAIL MFA is disabled for the user.", username);
			clearTFADetails(user);
			redirectToLogin(request, response);
			return true;
		}

		boolean rememberMe = request.getParameter(REMEMBER_PARAMETER) != null;
		if (isResendOnCooldown(user)) {
			LOGGER.info("Rejecting 2fa resend for user {} because cooldown is active.", username);
			request.setAttribute(RESEND_COOLDOWN_ATTRIBUTE, Boolean.TRUE);
			return forwardToTwoFactorPage(request, response, user, rememberMe, false);
		}

		LOGGER.info("Resending 2fa code for user {}", username);
		String twoFactorCodeClearText = generateTFACode();

		Timestamp generatedTS = new Timestamp();
		user.setTfaCodeGeneratedTimestamp(generatedTS);
		user.setTfaCode(EXT.hashPassword(twoFactorCodeClearText));
		user.setTfaToken(generateTFAPushId(generatedTS));
		updateUserTFADetails(user);
		pushNotification(user, twoFactorCodeClearText);

		request.setAttribute(RESEND_SUCCESS_ATTRIBUTE, Boolean.TRUE);
		return forwardToTwoFactorPage(request, response, user, rememberMe, false);
	}

	private void redirectToLogin(HttpServletRequest request, HttpServletResponse response)
	throws IOException, ServletException {
		SimpleUrlAuthenticationFailureHandler handler = new SimpleUrlAuthenticationFailureHandler("/login");
		handler.onAuthenticationFailure(request, response, new AuthenticationServiceException("Invalid TFA resend request"));
	}

	private boolean forwardToTwoFactorPage(HttpServletRequest request,
											HttpServletResponse response,
											TwoFactorAuthUser user)
	throws IOException, ServletException {
		return forwardToTwoFactorPage(request, response, user, false, true);
	}

	private boolean forwardToTwoFactorPage(HttpServletRequest request,
											HttpServletResponse response,
											TwoFactorAuthUser user,
											boolean rememberMe,
											boolean invalidCode)
	throws IOException, ServletException {
		TwoFactorAuthForwardHandler handler = new TwoFactorAuthForwardHandler("/login");
		request.setAttribute(CUSTOMER_ATTRIBUTE, user.getCustomer());
		request.setAttribute(TWO_FACTOR_TOKEN_ATTRIBUTE,  user.getTfaToken());
		request.setAttribute(USER_ATTRIBUTE, user.getUser());
		request.setAttribute(REMEMBER_ATTRIBUTE, Boolean.valueOf(rememberMe));
		handler.onAuthenticationFailure(request, response, new TwoFactorAuthRequiredException("OTP sent", invalidCode));
		return true;
	}
	
	private void clearTFADetails(TwoFactorAuthUser user) {
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
	
	@Override
	protected String obtainUsername(HttpServletRequest request) {
		return UtilImpl.processStringValue(super.obtainUsername(request));
	}
	
	@SuppressWarnings("static-method")
	protected String obtainCustomer(HttpServletRequest request) {
		String customerName = UtilImpl.processStringValue(request.getParameter(SKYVE_SECURITY_FORM_CUSTOMER_KEY));
		if (customerName == null) {
			customerName = UtilImpl.CUSTOMER;
		}
		
		return customerName;
	}

	protected boolean isValidTwoFactorCode(String twoFactorCode) {
		return (twoFactorCode != null) && SIX_DIGIT_TFA_CODE_PATTERN.matcher(twoFactorCode).matches();
	}
	
	/**
	 * Determine if this filter supports the configured MFA factor.
	 */
	protected abstract boolean supportsPushConfiguration(TwoFactorAuthCustomerConfiguration config);

	/**
	 * send the push notification
	 */
	protected abstract void pushNotification(TwoFactorAuthUser user, String code);
	
	/**
	 * Get the user details required for this filter
	 * 
	 * @param username	customer/username
	 * @return
	 * @throws Exception 
	 */
	protected TwoFactorAuthUser getUserDB(String username) {
		UserDetails userDetails;
		try {
			userDetails = userDetailsManager.loadUserByUsername(username);
		}
		catch (@SuppressWarnings("unused") UsernameNotFoundException e) {
			// incorrect username
			return null;
		}
		
		if (userDetails instanceof TwoFactorAuthUser twoFactorAuthUser) {
			return twoFactorAuthUser;
		}
		return null;
	}
	
	protected void updateUserTFADetails(TwoFactorAuthUser user) {
		userDetailsManager.updateUser(user);
	}
	
	@SuppressWarnings("static-method")
	protected String generateTFAPushId(Timestamp generatedTS) {
		return UUID.randomUUID().toString() + "-" + Long.toString(generatedTS.getTime());
	}

	// This is thread-safe
	private static final SecureRandom RANDOM = new SecureRandom();
    // Get 128 random bytes - move past first seed sequence
    static {
		byte[] randomBytes = new byte[128];
	    RANDOM.nextBytes(randomBytes);
    }
    
	@SuppressWarnings("static-method")
	protected String generateTFACode() {
		return new DecimalFormat("000000").format(RANDOM.nextDouble() * 1000000d);
	}
	
	/**
	 * note: return true here does not log the user in.
	 * Checks the credentials are correct before sending the push notification
	 * 
	 * @param request
	 * @return
	 */
	protected boolean canAuthenticateWithPassword(HttpServletRequest request, TwoFactorAuthUser user) {
		if ((! user.isEnabled()) ||
				(! user.isAccountNonExpired()) ||
				(! user.isCredentialsNonExpired()) ||
				(! user.isAccountNonLocked())) {
			return false;
		}

		String password = obtainPassword(request);
		password = (password != null) ? password : "";
		
		return EXT.checkPassword(password, user.getUserPassword());
	}
	
	@SuppressWarnings("static-method")
	protected boolean tfaCodeExpired(String customer, String twoFactorCode) {
		long generatedTime;
		try {
			String [] splitParts = twoFactorCode.split("-");
			String timeComponent = splitParts[splitParts.length-1];
			
			generatedTime = Long.parseLong(timeComponent);
		} 
		catch (@SuppressWarnings("unused") NumberFormatException e) {
			//code tampered with?
			return true;
		}
		
		long expiryMillis = getTwoFactorTimeoutMillis(customer);
		long currentTime = new DateTime().getTime();
		
		return currentTime > (generatedTime + expiryMillis);
	}
	
	private static long getTwoFactorTimeoutMillis(String customer) {
		TwoFactorAuthCustomerConfiguration config = TwoFactorAuthConfigurationSingleton.getInstance().getConfig(customer);
		int timeoutSeconds = config.getTfaTimeOutSeconds();
		return timeoutSeconds * 1000;
	}
	
	/**
	 * Check the user and see if they have the necessary TFA codes populated
	 * 
	 * @param user
	 * @return
	 */
	@SuppressWarnings("static-method")
	protected boolean tfaCodesPopulated(TwoFactorAuthUser user) {
		return ((user.getTfaCode() != null) &&
				(user.getTfaToken() != null) &&
				(user.getTfaCodeGeneratedTimestamp() != null));
	}

	/**
	 * Check per-user MFA override.
	 * @return true if MFA should proceed, false if the user explicitly opted out.
	 *         Returns true (fail closed) when there is no per-user config or it is invalid.
	 */
	protected boolean isUserMfaRequired(TwoFactorAuthUser user, String method) {
		Optional<Boolean> userEnabled = MfaConfigurationUtil.isMethodEnabled(user.getMfaConfiguration(), method);
		return userEnabled.orElse(Boolean.TRUE).booleanValue();
	}

	protected boolean isResendOnCooldown(TwoFactorAuthUser user) {
		Timestamp generatedTimestamp = user.getTfaCodeGeneratedTimestamp();
		if (generatedTimestamp == null) {
			return false;
		}

		long elapsedMillis = currentTimeMillis() - generatedTimestamp.getTime();
		return elapsedMillis < (UtilImpl.TWO_FACTOR_AUTH_RESEND_COOLDOWN_SECONDS * 1000L);
	}

	@SuppressWarnings("static-method")
	protected long currentTimeMillis() {
		return System.currentTimeMillis();
	}
}
