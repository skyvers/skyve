package org.skyve.impl.web.spring;

import org.skyve.impl.util.UtilImpl;
import org.skyve.util.Util;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Import;
import org.springframework.http.HttpMethod;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.builders.WebSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.config.annotation.web.configuration.WebSecurityConfigurerAdapter;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.security.oauth2.client.registration.ClientRegistrationRepository;
//import org.springframework.security.saml2.provider.service.registration.InMemoryRelyingPartyRegistrationRepository;
//import org.springframework.security.saml2.provider.service.registration.RelyingPartyRegistration;
//import org.springframework.security.saml2.provider.service.registration.RelyingPartyRegistrationRepository;
//import org.springframework.security.saml2.provider.service.registration.RelyingPartyRegistrations;
import org.springframework.security.web.authentication.rememberme.PersistentTokenRepository;

/**
 * This class supplies named spring beans to the OOTB security.xml
 */
@Configuration
@Import(SkyveSpringSecurityConfig.class)
@EnableWebSecurity
public class SpringSecurityConfig extends WebSecurityConfigurerAdapter {
	@Autowired
	private SkyveSpringSecurity skyve;
	
	@Override
	protected void configure(HttpSecurity http) throws Exception {
		http
			.authorizeRequests()
				// Permit the login servlet resource
				.antMatchers(HttpMethod.GET, "/login", "/loggedOut").permitAll()
				// Permit the spring login mechanism and the SC JS login mechanism
				.antMatchers(HttpMethod.POST, "/loginAttempt", "/smartClientJavascriptLogin").permitAll()
				// Permit the reset resources
				.antMatchers("/pages/requestPasswordReset.jsp", "/pages/resetPassword.jsp").permitAll()
				// Permit home.jsp as it controls access to public and private pages itself
				.antMatchers("/home.jsp").permitAll()
				// Secure the loggedIn.jsp so that redirect occurs after login
				.antMatchers("/loggedIn.jsp").authenticated()
				// Secure the system JSPs and HTMLs
				.antMatchers("/pages/changePassword.jsp", "/pages/htmlEdit/browseImages.jsp", "/pages/map/geolocate.jsp").authenticated()
				// Do not secure the home servlet as this ensures the right login page is displayed
				.antMatchers("/home").permitAll()
				// Do not secure faces pages as they are secured by a FacesSecurityFilter
				.antMatchers("/**/*.xhtml").permitAll()
				// Secure dynamic image URLs
				.antMatchers("/images/dynamic.*").authenticated()
				// Secure all report URLs
				.antMatchers("/report", "/export").authenticated()
				// Secure chart servlet
				.antMatchers("/chart").authenticated()
				// Secure Image Servlet for HTML reporting through Jasper
				.antMatchers("/image").authenticated()
				// Secure customer resource servlet
				.antMatchers("/resource", "/content").authenticated()
				// Secure SC edit view servlet
				.antMatchers("/smartedit").authenticated()
				// Secure SC list view servlet
				.antMatchers("/smartlist").authenticated()
				// Secure SC view generation servlet
				.antMatchers("/smartgen").authenticated()
				// Secure SC snap servlet
				.antMatchers("/smartsnap").authenticated()
				// Secure SC tag servlet
				.antMatchers("/smarttag").authenticated()
				// Secure SC complete servlet
				.antMatchers("/smartcomplete").authenticated()
				// Secure SC text search servlet
				.antMatchers("/smartsearch").authenticated()
				// Secure Prime initialisation servlet
				.antMatchers("/primeinit").authenticated()
				// Secure map servlet
				.antMatchers("/map").authenticated()
				// Secure the Bizport Export Servlet
				.antMatchers("/bizexport.*").authenticated()
				// Secure the Download Servlet
				.antMatchers("/download").authenticated()
				// Secure the Push endpoint
				.antMatchers("/omnifaces.push/**").authenticated()
				// Do not Secure trackmate servlet - it handles authentication itself
				.antMatchers("/tracks").permitAll()
				// Permit all GET requests by default
				.antMatchers(HttpMethod.GET, "/**").permitAll()
				//  Secure all POST requests by default
				.antMatchers(HttpMethod.POST, "/**").authenticated()
				// Only allow get and post methods by default
				.anyRequest().denyAll()
				.and()
			.sessionManagement()
				.sessionFixation().changeSessionId()
				.and()
			.rememberMe()
				.key("remember")
				.tokenValiditySeconds(1209600)
				.rememberMeParameter("remember")
				.rememberMeCookieName("remember")
				.tokenRepository(tokenRepository())
				.useSecureCookie(Util.isSecureUrl())
				.and()
			.formLogin()
				.defaultSuccessUrl(Util.getHomeUrl())
				.loginPage(Util.getLoginUrl())
				.loginProcessingUrl("/loginAttempt")
				.failureUrl(Util.getLoginUrl() + "?error")
				.successHandler(new SkyveAuthenticationSuccessHandler())
				.and()
			.logout()
				.logoutSuccessUrl(Util.getLoggedOutUrl())
				.deleteCookies("JSESSIONID")
				.and()
			.csrf().disable()
			.headers()
				.httpStrictTransportSecurity().disable()
				.frameOptions().disable()
				.contentTypeOptions().disable();

			if ((UtilImpl.AUTHENTICATION_GOOGLE_CLIENT_ID != null) ||
					(UtilImpl.AUTHENTICATION_FACEBOOK_CLIENT_ID != null) ||
					(UtilImpl.AUTHENTICATION_GITHUB_CLIENT_ID != null)) {
				http.oauth2Login().loginPage(Util.getLoginUrl());
			}
			
//			http.saml2Login()
//					.loginPage(Util.getLoginUrl())
//					.defaultSuccessUrl(Util.getHomeUrl());
	}

	@Override
	public void configure(WebSecurity web) throws Exception {
		web.ignoring().antMatchers("/h2/**", "/rest/**");
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
	@Bean
	public PasswordEncoder passwordEncoder() {
		return skyve.passwordEncoder();
	}
	
	@Bean
	public PersistentTokenRepository tokenRepository() throws Exception {
		return skyve.tokenRepository();
	}

	@Bean
	@Override
	public UserDetailsService userDetailsService() {
		return skyve.jdbcUserDetailsService();
	}
	
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