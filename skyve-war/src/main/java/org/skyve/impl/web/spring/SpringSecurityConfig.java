package org.skyve.impl.web.spring;

import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.LoginServlet;
import org.skyve.util.Util;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Import;
import org.springframework.http.HttpMethod;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.security.oauth2.client.registration.ClientRegistrationRepository;
import org.springframework.security.provisioning.UserDetailsManager;
import org.springframework.security.web.DefaultSecurityFilterChain;
import org.springframework.security.web.SecurityFilterChain;
import org.springframework.security.web.authentication.UsernamePasswordAuthenticationFilter;
//import org.springframework.security.saml2.provider.service.registration.InMemoryRelyingPartyRegistrationRepository;
//import org.springframework.security.saml2.provider.service.registration.RelyingPartyRegistration;
//import org.springframework.security.saml2.provider.service.registration.RelyingPartyRegistrationRepository;
//import org.springframework.security.saml2.provider.service.registration.RelyingPartyRegistrations;
import org.springframework.security.web.authentication.rememberme.PersistentTokenRepository;

/**
 * Supplies Spring Security beans and request-filter configuration for Skyve WAR.
 *
 * <p>Side effects: defines URL authorization rules, login/logout behavior,
 * remember-me persistence, optional OAuth login, and two-factor pre-processing.
 */
@Configuration
@Import(SkyveSpringSecurityConfig.class)
@EnableWebSecurity
public class SpringSecurityConfig {
	private static final String LOGIN_ERROR_QUERY = "?error";
	private static final String REMEMBER_ME_PARAMETER = "remember";

	/**
	 * Security adapter that provides concrete Spring Security collaborators.
	 */
	@Autowired
	@SuppressWarnings("java:S6813")
	private SkyveSpringSecurity skyve;

	/**
	 * Default constructor for Spring.
	 */
	@Autowired
	public SpringSecurityConfig() {
		// Spring injects after construction.
	}

	/**
	 * Test-only constructor that allows direct instantiation with a mock adapter.
	 *
	 * @param skyve the Spring Security adapter to delegate to
	 */
	SpringSecurityConfig(SkyveSpringSecurity skyve) {
		this.skyve = skyve;
	}
	
	/**
	 * Builds the application {@link SecurityFilterChain}.
	 *
	 * <p>Side effects: configures URL authorisation, remember-me, login/logout handlers, and two-factor email push filtering.
	 *
	 * @param http the mutable Spring security builder
	 * @return the configured security filter chain
	 * @throws Exception if chain construction fails
	 */
	@Bean
	@SuppressWarnings("java:S4502") // Suppress CSRF turned off as Skyve implements its own
	public SecurityFilterChain filterChain(HttpSecurity http) throws Exception {
		http.authorizeHttpRequests(c -> {
			if (UtilImpl.DEV_LOGIN_FILTER_USED) {
				// Open SC list view servlet
				c.requestMatchers("/smartlist").permitAll()
					// Open SC snap servlet
					.requestMatchers("/smartsnap").permitAll()
					// Open SC tag servlet
					.requestMatchers("/smarttag").permitAll()
					// Open export URL
					.requestMatchers("/export").permitAll();
			}
			else {
				// Secure SC list view servlet
				c.requestMatchers("/smartlist").authenticated()
					// Secure SC snap servlet
					.requestMatchers("/smartsnap").authenticated()
					// Secure SC tag servlet
					.requestMatchers("/smarttag").authenticated()
					// Secure export URL
					.requestMatchers("/export").authenticated();
			}
			
			// Permit H2 servlet if enabled
			c.requestMatchers("/h2/**").permitAll()
				// Enable access to all rest endpoints as these will have Servlet Filters to secure.
				.requestMatchers("/rest/**").permitAll()
				// Permit the login servlet resource
				.requestMatchers(HttpMethod.GET, LoginServlet.LOGIN_PATH, LoginServlet.LOGGED_OUT_PATH).permitAll()
				// Permit the spring login mechanism
				.requestMatchers(HttpMethod.POST, SkyveSpringSecurity.LOGIN_ATTEMPT_PATH).permitAll()
				// Permit the reset resources
				.requestMatchers("/pages/requestPasswordReset.jsp", "/pages/resetPassword.jsp").permitAll()
				// Permit home.jsp as it controls access to public and private pages itself
				.requestMatchers("/home.jsp").permitAll()
				// Permit device.jsp as it forwards to home.jsp
				.requestMatchers("/device.jsp").permitAll()
				// Permit the health servlet resource
				.requestMatchers("/health").permitAll()
				// Secure the loggedIn.jsp so that redirect occurs after login
				.requestMatchers("/loggedIn.jsp").authenticated()
				// Secure the changePassword JSP
				.requestMatchers("/pages/changePassword.jsp").authenticated()
				// Secure the CKEditor JSPs
				.requestMatchers("/pages/htmlEdit/browseImages.jsp", "/pages/htmlEdit/browseDocuments.jsp").authenticated()
				// Do not secure the home servlet as this ensures the right login page is displayed
				.requestMatchers("/home").permitAll()
				// Do not secure faces pages as they are secured by a FacesSecurityFilter
				.requestMatchers("/**/*.xhtml").permitAll()
				// Secure dynamic image URLs
				.requestMatchers("/dynamic.*").authenticated()
				// Secure report URL
				.requestMatchers("/report").authenticated()
				// Secure chart servlet
				.requestMatchers("/chart").authenticated()
				// Secure Image Servlet for HTML reporting through Jasper
				.requestMatchers("/image").authenticated()
				// Do not secure the customer resource servlet as it checks for a user
				.requestMatchers("/resource", "/content").permitAll()
				// Do not secure the Download Servlet as it checks for a user
				.requestMatchers("/download").permitAll()
				// Secure meta data servlet
				.requestMatchers("/meta").authenticated()
				// Secure SC edit view servlet
				.requestMatchers("/smartedit").authenticated()
				// Secure SC view generation servlet
				.requestMatchers("/smartgen").authenticated()
				// Secure SC complete servlet
				.requestMatchers("/smartcomplete").authenticated()
				// Secure SC text search servlet
				.requestMatchers("/smartsearch").authenticated()
				// Secure map servlet
				.requestMatchers("/map").authenticated()
				// Secure the Bizport Export Servlet
				.requestMatchers("/bizexport.*").authenticated()
				// Secure the Server-sent event stream; see SseApplication
				.requestMatchers("/sse/**").authenticated()
				// Permit all GET requests by default
				.requestMatchers(HttpMethod.GET, "/**").permitAll()
				//  Secure all POST requests by default
				.requestMatchers(HttpMethod.POST, "/**").authenticated()
				// Only allow get and post methods by default
				.anyRequest().denyAll();
			}
		)
		.sessionManagement(c -> c.sessionFixation().changeSessionId())
		.rememberMe(c ->
			c.key(REMEMBER_ME_PARAMETER)
				.tokenValiditySeconds(UtilImpl.REMEMBER_ME_TOKEN_TIMEOUT_HOURS * 60 * 60)
				.rememberMeParameter(REMEMBER_ME_PARAMETER)
				.rememberMeCookieName(REMEMBER_ME_PARAMETER)
				.tokenRepository(tokenRepository())
				.useSecureCookie(Util.isSecureUrl())
		)
		.formLogin(c -> 
			c.defaultSuccessUrl(Util.getHomeUrl())
				.loginPage(Util.getLoginUrl())
				.loginProcessingUrl(SkyveSpringSecurity.LOGIN_ATTEMPT_PATH)
				.failureUrl(Util.getLoginUrl() + LOGIN_ERROR_QUERY)
				.successHandler(new SkyveAuthenticationSuccessHandler(userDetailsManager()))
		)
		.logout(c -> 
			c.logoutSuccessUrl(Util.getLoggedOutUrl())
				.deleteCookies("JSESSIONID")
		)
		.csrf(c -> c.disable())
		.httpBasic(c -> c.disable())
		.headers(c -> 
			c.httpStrictTransportSecurity(ic -> ic.disable())
				.frameOptions(ic -> ic.disable())
				.contentTypeOptions(ic -> ic.disable())
		);

		if ((UtilImpl.AUTHENTICATION_GOOGLE_CLIENT_ID != null)
				|| (UtilImpl.AUTHENTICATION_FACEBOOK_CLIENT_ID != null)
				|| (UtilImpl.AUTHENTICATION_GITHUB_CLIENT_ID != null)
				|| (UtilImpl.AUTHENTICATION_AZUREAD_TENANT_ID != null)) {
			http.oauth2Login(c ->
				c.defaultSuccessUrl(Util.getHomeUrl())
					.loginPage(Util.getLoginUrl())
					.failureUrl(Util.getLoginUrl() + LOGIN_ERROR_QUERY)
					.successHandler(new SkyveAuthenticationSuccessHandler(userDetailsManager()))
			);
		}

//		http.saml2Login(c -> 
//			c.defaultSuccessUrl(Util.getHomeUrl())
//				.loginPage(Util.getLoginUrl())
//				.failureUrl(Util.getLoginUrl() + "?error")
//				.successHandler(new SkyveAuthenticationSuccessHandler(userDetailsManager()))
//		);

		TwoFactorAuthPushEmailFilter tfaEmail = new TwoFactorAuthPushEmailFilter(userDetailsManager());
		http.addFilterBefore(tfaEmail, UsernamePasswordAuthenticationFilter.class);

		DefaultSecurityFilterChain result = http.build();
		// Note AuthenticationManager is not available as a shared object until after build()
		AuthenticationManager authenticationManager = http.getSharedObject(AuthenticationManager.class);
		tfaEmail.setAuthenticationManager(authenticationManager);
		return result;
	}

/*
	@Bean
	public RelyingPartyRegistrationRepository relyingPartyRegistrationRepository() {
		RelyingPartyRegistration relyingPartyRegistration = RelyingPartyRegistrations
				.fromMetadataLocation("https://login.microsoftonline.com/blah-blah-blah")
				.registrationId("<app-name-registered-with-AD>")
				.entityId("https://{baseHost}/saml2/service-provider-metadata/{registrationId}")
				.assertionConsumerServiceLocation("https://{baseHost}/login/saml2/sso/{registrationId}")
				.build();
		return new InMemoryRelyingPartyRegistrationRepository(relyingPartyRegistration);
	}
*/
    
	/**
	 * Exposes the password encoder used by authentication providers.
	 *
	 * @return the configured password encoder
	 */
	@Bean
	public PasswordEncoder passwordEncoder() {
		return skyve.passwordEncoder();
	}
	
	/**
	 * Exposes the remember-me token repository.
	 *
	 * @return the configured persistent token repository
	 */
	@Bean
	public PersistentTokenRepository tokenRepository() {
		return skyve.tokenRepository();
	}
	
	/**
	 * Exposes the JDBC-backed user-details manager.
	 *
	 * @return the configured user-details manager
	 */
	@Bean
	public UserDetailsManager userDetailsManager() {
		return skyve.jdbcUserDetailsManager();
	}
	
	/**
	 * Exposes OAuth client registration definitions.
	 *
	 * @return the configured client registration repository
	 */
 	@Bean
    public ClientRegistrationRepository clientRegistrationRepository() {
 		return skyve.clientRegistrationRepository();
    }

}
/*
    <!-- WAFFLE Configuration -->
<!--
	<http auto-config="false" use-expressions="true" entry-point-ref="negotiateSecurityFilterEntryPoint">
		<custom-filter ref="waffleNegotiateSecurityFilter" position="PRE_AUTH_FILTER" />
		...
    </http>
	<b:bean id="waffleWindowsAuthProvider" class="waffle.windows.auth.impl.WindowsAuthProviderImpl" />
	<b:bean id="negotiateSecurityFilterProvider" class="waffle.servlet.spi.NegotiateSecurityFilterProvider">
		<b:constructor-arg ref="waffleWindowsAuthProvider" />
	</b:bean>
	<b:bean id="basicSecurityFilterProvider" class="waffle.servlet.spi.BasicSecurityFilterProvider">
		<b:constructor-arg ref="waffleWindowsAuthProvider" />
	</b:bean>
	<b:bean id="waffleSecurityFilterProviderCollection" class="waffle.servlet.spi.SecurityFilterProviderCollection">
		<b:constructor-arg>
			<b:list>
				<b:ref bean="negotiateSecurityFilterProvider" />
			</b:list>
		</b:constructor-arg>
	</b:bean>
	<b:bean id="negotiateSecurityFilterEntryPoint" class="waffle.spring.NegotiateSecurityFilterEntryPoint">
		<b:property name="provider" ref="waffleSecurityFilterProviderCollection" />
	</b:bean>
	<b:bean id="waffleNegotiateSecurityFilter" class="waffle.spring.NegotiateSecurityFilter">
		<b:property name="provider" ref="waffleSecurityFilterProviderCollection" />
		<b:property name="allowGuestLogin" value="false" />
		<b:property name="principalFormat" value="fqn" />
		<b:property name="roleFormat" value="both" />
	</b:bean>
-->
	<!-- Single Customer Basic Auth -->
<!--
	<authentication-manager id="authenticationManager">
		...
	</authentication-manager>

	<http auto-config="false" use-expressions="true" entry-point-ref="skyveEntryPoint">
 		<custom-filter ref="skyveFilter" before="BASIC_AUTH_FILTER" />
 		...
 	</http>
	<b:bean id="skyveEntryPoint" class="org.springframework.security.web.authentication.www.BasicAuthenticationEntryPoint">
 		<b:property name="realmName" value="Skyve" />
 	</b:bean>
 	<b:bean id="skyveFilter" class="org.skyve.impl.web.spring.SingleCustomerBasicAuthenticationFilter">
 		<b:constructor-arg name="authenticationManager" ref="authenticationManager" />
 	</b:bean>
 -->
*/
