package org.skyve.impl.web.spring;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Timestamp;
import java.util.List;

import javax.naming.InitialContext;
import javax.sql.DataSource;

import org.skyve.domain.messages.DomainException;
import org.skyve.impl.persistence.hibernate.AbstractHibernatePersistence;
import org.skyve.impl.persistence.hibernate.dialect.SkyveDialect;
import org.skyve.impl.persistence.hibernate.dialect.SkyveDialect.RDBMS;
import org.skyve.impl.security.SkyveLegacyPasswordEncoder;
import org.skyve.impl.security.SkyveRememberMeTokenRepository;
import org.skyve.impl.util.UtilImpl;
import org.skyve.util.Util;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.HttpMethod;
import org.springframework.jdbc.core.RowMapper;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.builders.WebSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.config.annotation.web.configuration.WebSecurityConfigurerAdapter;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.authority.AuthorityUtils;
import org.springframework.security.core.userdetails.User;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.crypto.factory.PasswordEncoderFactories;
import org.springframework.security.crypto.password.DelegatingPasswordEncoder;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.security.provisioning.JdbcUserDetailsManager;
import org.springframework.security.web.authentication.rememberme.PersistentTokenRepository;

/**
 * This class supplies named spring beans to the OOTB security.xml
 */
@Configuration
@EnableWebSecurity(debug = true)
public class SpringSecurityConfig extends WebSecurityConfigurerAdapter {
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
			.rememberMe()
				.key("remember")
				.tokenValiditySeconds(1209600)
				.rememberMeParameter("remember")
				.rememberMeCookieName("remember")
				.tokenRepository(tokenRepository())
				.and()
			.formLogin()
				.defaultSuccessUrl(Util.getSkyveContextUrl() + '/')
				.loginPage(Util.getSkyveContextUrl() + "/login")
				.loginProcessingUrl("/loginAttempt")
				.failureUrl(Util.getSkyveContextUrl() + "/login?error")
				.and()
			.logout()
				.logoutSuccessUrl(Util.getSkyveContextUrl() + "/loggedOut")
				.deleteCookies("JSESSIONID")
				.and()
			.csrf().disable()
			.headers()
				.httpStrictTransportSecurity().disable()
				.frameOptions().disable()
				.contentTypeOptions().disable();
	}
/*	
	@Override
	protected void configure(AuthenticationManagerBuilder auth) throws Exception {
		auth.jdbcAuthentication().and().userDetailsService(jdbcUserService()).passwordEncoder(passwordEncoder());
	}
*/
	@Override
	public void configure(WebSecurity web) throws Exception {
		web.ignoring().antMatchers("/h2/**", "/rest/**");
	}

	@Bean
	@SuppressWarnings("static-method")
	public PasswordEncoder passwordEncoder() {
		DelegatingPasswordEncoder result = (DelegatingPasswordEncoder) PasswordEncoderFactories.createDelegatingPasswordEncoder();
		result.setDefaultPasswordEncoderForMatches(new SkyveLegacyPasswordEncoder());
		return result;
	}
	
	@Bean
	public PersistentTokenRepository tokenRepository() throws Exception {
		SkyveRememberMeTokenRepository result = new SkyveRememberMeTokenRepository();
		result.setDataSource(dataSource());
		return result;
	}

	@Bean
	@SuppressWarnings("static-method")
	public DataSource dataSource() {
		try {
			InitialContext ctx = new InitialContext();
			return (DataSource) ctx.lookup(UtilImpl.DATA_STORE.getJndiDataSourceName());
		}
		catch (Exception e) {
			throw new DomainException("Cannot obtain the JNDI datasource", e);
		}
	}
/* In-memory user details for demo/admin
	@Bean
	@Override
	public UserDetailsService userDetailsService() {
		UserDetails user =
			 User.withDefaultPasswordEncoder()
				.username("demo/admin")
				.password("admin")
				.build();

		return new InMemoryUserDetailsManager(user);
	}
*/

	@Bean 
	@Override
	public UserDetailsService userDetailsService() {
		JdbcUserDetailsManager result = new JdbcUserDetailsManager() {
			
			// return the user just queried (with the 
			@Override
			protected UserDetails createUserDetails(String username,
														UserDetails userFromUserQuery,
														List<GrantedAuthority> combinedAuthorities) {
				return userFromUserQuery;
			}
			
			@Override
			protected List<UserDetails> loadUsersByUsername(String username) {
				return getJdbcTemplate().query(
						getUsersByUsernameQuery(),
						new String[] {username},
						new RowMapper<UserDetails>() {
							@Override
							public UserDetails mapRow(ResultSet rs, int rowNum)
							throws SQLException {
								String user = rs.getString(1);
								String password = rs.getString(2);
								boolean enabled = rs.getBoolean(3);
								
								// Determine if the account is locked
								// if at the threshold each subsequent failed attempt adds another lockout duration
								boolean locked = false;
								int authenticationFailures = rs.getInt(4);
								if (rs.wasNull()) {
									authenticationFailures = 0;
								}
								Timestamp lastAuthenticationFailure = rs.getTimestamp(5);
								if ((lastAuthenticationFailure != null) &&
										(UtilImpl.ACCOUNT_LOCKOUT_THRESHOLD > 0) && 
										(UtilImpl.ACCOUNT_LOCKOUT_DURATION_MULTIPLE_IN_SECONDS > 0)) {
									if (authenticationFailures >= UtilImpl.ACCOUNT_LOCKOUT_THRESHOLD) {
										long lockoutMillis = authenticationFailures * UtilImpl.ACCOUNT_LOCKOUT_DURATION_MULTIPLE_IN_SECONDS * 1000;
										long millisRemaining = (lastAuthenticationFailure.getTime() + lockoutMillis) - System.currentTimeMillis();
										if (millisRemaining > 0) {
											long secondsRemaining = millisRemaining / 1000;
											if (secondsRemaining == 0) {
												secondsRemaining++;
											}
											locked = true;
											UtilImpl.LOGGER.warning("Account " + username + " is locked for another " + secondsRemaining + " seconds");
										}
									}
								}

								return new User(user,
													password,
													enabled,
													true,
													true,
													! locked,
													AuthorityUtils.NO_AUTHORITIES);
							}
						});
			}
		};
		
		SkyveDialect dialect = AbstractHibernatePersistence.getDialect(UtilImpl.DATA_STORE.getDialectClassName());
		RDBMS rdbms = dialect.getRDBMS();
		if (RDBMS.h2.equals(rdbms)) {
			result.setUsersByUsernameQuery("select bizCustomer || '/' || userName, password, not ifNull(inactive, false) and ifNull(activated, true), authenticationFailures, lastAuthenticationFailure from ADM_SecurityUser where bizCustomer || '/' || userName = ?");
			result.setAuthoritiesByUsernameQuery("select bizCustomer || '/' || userName, 'NoAuth' from ADM_SecurityUser where bizCustomer || '/' || userName = ?");
			result.setGroupAuthoritiesByUsernameQuery("select bizCustomer || '/' || userName, bizCustomer || '/' || userName, 'NoAuth' from ADM_SecurityUser where bizCustomer || '/' || userName = ?");
		}
		else if (RDBMS.mysql.equals(rdbms)) {
			result.setUsersByUsernameQuery("select concat(bizCustomer, '/', userName), password, not ifNull(inactive, false) and ifNull(activated, true), authenticationFailures, lastAuthenticationFailure from ADM_SecurityUser where concat(bizCustomer, '/', userName) = ?");
			result.setAuthoritiesByUsernameQuery("select concat(bizCustomer, '/', userName), 'NoAuth' from ADM_SecurityUser where concat(bizCustomer, '/', userName) = ?");
			result.setGroupAuthoritiesByUsernameQuery("select concat(bizCustomer, '/', userName), concat(bizCustomer, '/', userName), 'NoAuth' from ADM_SecurityUser where concat(bizCustomer, '/', userName) = ?");
			
		}
		else if (RDBMS.sqlserver.equals(rdbms)) {
			result.setUsersByUsernameQuery("select bizCustomer + '/' + userName, password, case when coalesce(inactive, 0) = 0 and coalesce(activated, 1) = 1 then 1 else 0 end, authenticationFailures, lastAuthenticationFailure from ADM_SecurityUser where bizCustomer + '/' + userName = ?");
			result.setAuthoritiesByUsernameQuery("select bizCustomer + '/' + userName, 'NoAuth' from ADM_SecurityUser where bizCustomer + '/' + userName = ?");
			result.setGroupAuthoritiesByUsernameQuery("select bizCustomer + '/' + userName, bizCustomer + '/' + userName, 'NoAuth' from ADM_SecurityUser where bizCustomer + '/' + userName = ?");
		}
		else if (RDBMS.postgresql.equals(rdbms)) {
			result.setUsersByUsernameQuery("select bizCustomer || '/' || userName, password, not coalesce(inactive, false) and coalesce(activated, true), authenticationFailures, lastAuthenticationFailure from ADM_SecurityUser where bizCustomer || '/' || userName = ?");
			result.setAuthoritiesByUsernameQuery("select bizCustomer || '/' || userName, 'NoAuth' from ADM_SecurityUser where bizCustomer || '/' || userName = ?");
			result.setGroupAuthoritiesByUsernameQuery("select bizCustomer || '/' || userName, bizCustomer || '/' || userName, 'NoAuth' from ADM_SecurityUser where bizCustomer || '/' || userName = ?");
		}
		result.setDataSource(dataSource());
		result.setRolePrefix("none");
		return result;
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