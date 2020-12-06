package org.skyve.impl.web.spring;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Timestamp;
import java.util.ArrayList;
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
import org.springframework.jdbc.core.RowMapper;
import org.springframework.security.config.oauth2.client.CommonOAuth2Provider;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.authority.AuthorityUtils;
import org.springframework.security.core.userdetails.User;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.crypto.factory.PasswordEncoderFactories;
import org.springframework.security.crypto.password.DelegatingPasswordEncoder;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.security.oauth2.client.registration.ClientRegistration;
import org.springframework.security.oauth2.client.registration.ClientRegistrationRepository;
import org.springframework.security.oauth2.client.registration.InMemoryClientRegistrationRepository;
import org.springframework.security.oauth2.core.AuthorizationGrantType;
import org.springframework.security.oauth2.core.ClientAuthenticationMethod;
import org.springframework.security.oauth2.core.oidc.IdTokenClaimNames;
import org.springframework.security.provisioning.InMemoryUserDetailsManager;
import org.springframework.security.provisioning.JdbcUserDetailsManager;
import org.springframework.security.web.authentication.rememberme.PersistentTokenRepository;

public class SkyveSpringSecurity {
	@SuppressWarnings("static-method")
	public PasswordEncoder passwordEncoder() {
		DelegatingPasswordEncoder result = (DelegatingPasswordEncoder) PasswordEncoderFactories.createDelegatingPasswordEncoder();
		result.setDefaultPasswordEncoderForMatches(new SkyveLegacyPasswordEncoder());
		return result;
	}
	
	public PersistentTokenRepository tokenRepository() throws Exception {
		SkyveRememberMeTokenRepository result = new SkyveRememberMeTokenRepository();
		result.setDataSource(dataSource());
		return result;
	}

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
	
	@SuppressWarnings("static-method")
	public UserDetailsService testUserDetailsService(String customerName, String userName, String password) {
		@SuppressWarnings("deprecation")
		UserDetails user = User.withDefaultPasswordEncoder()
								.username(customerName + '/' + userName)
								.password(password)
								.build();
		return new InMemoryUserDetailsManager(user);
	}
	
	public UserDetailsService jdbcUserDetailsService() {
		JdbcUserDetailsManager result = new JdbcUserDetailsManager() {
			
			// return the user just queried (with all the expiration details queried too)
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
	
	public ClientRegistrationRepository clientRegistrationRepository() {
		List<ClientRegistration> registrations = new ArrayList<>(3);
		if (UtilImpl.AUTHENTICATION_GOOGLE_CLIENT_ID != null) {
			registrations.add(googleClientRegistration());
		}
		if (UtilImpl.AUTHENTICATION_FACEBOOK_CLIENT_ID != null) {
			registrations.add(facebookClientRegistration());
		}
		if (UtilImpl.AUTHENTICATION_GITHUB_CLIENT_ID != null) {
			registrations.add(githubClientRegistration());
		}

		// Add a dummy registration otherwise spring security will moan
		if (registrations.isEmpty()) {
			registrations.add(CommonOAuth2Provider.GOOGLE.getBuilder("dummy").clientId("dummy").clientSecret("dummy").build());
		}

		return new InMemoryClientRegistrationRepository(registrations);
	}

    // https://console.developers.google.com/
 	// Based on CommonOAuth2Provider.GOOGLE
 	// Hit /oauth2/authorization/google
 	@SuppressWarnings("static-method")
	public ClientRegistration googleClientRegistration() {
		return ClientRegistration.withRegistrationId("google")
									.clientId(UtilImpl.AUTHENTICATION_GOOGLE_CLIENT_ID)
									.clientSecret(UtilImpl.AUTHENTICATION_GOOGLE_SECRET)
									.clientAuthenticationMethod(ClientAuthenticationMethod.BASIC)
									.authorizationGrantType(AuthorizationGrantType.AUTHORIZATION_CODE)
									.redirectUriTemplate("{baseUrl}/login/oauth2/code/{registrationId}")
									.scope("openid", "profile", "email", "address", "phone")
									.authorizationUri("https://accounts.google.com/o/oauth2/v2/auth")
									.tokenUri("https://www.googleapis.com/oauth2/v4/token")
									.userInfoUri("https://www.googleapis.com/oauth2/v3/userinfo")
									.userNameAttributeName(IdTokenClaimNames.SUB)
									.jwkSetUri("https://www.googleapis.com/oauth2/v3/certs")
									.clientName("Google")
									.build();
    }
    
    // https://developers.facebook.com/
    // Based on CommonOAuth2Provider.FACEBOOK
 	// Hit /oauth2/authorization/facebook
 	@SuppressWarnings("static-method")
    public ClientRegistration facebookClientRegistration() {
		return ClientRegistration.withRegistrationId("facebook")
									.clientId(UtilImpl.AUTHENTICATION_FACEBOOK_CLIENT_ID)
									.clientSecret(UtilImpl.AUTHENTICATION_FACEBOOK_SECRET)
									.clientAuthenticationMethod(ClientAuthenticationMethod.POST)
									.authorizationGrantType(AuthorizationGrantType.AUTHORIZATION_CODE)
									.redirectUriTemplate("{baseUrl}/login/oauth2/code/{registrationId}")
									.scope("public_profile", "email")
									.authorizationUri("https://www.facebook.com/v2.8/dialog/oauth")
									.tokenUri("https://graph.facebook.com/v2.8/oauth/access_token")
									.userInfoUri("https://graph.facebook.com/me")
									.userNameAttributeName("id")
									.clientName("Facebook")
									.build();
    }
    
	// https://github.com/settings/applications
	// Based on CommonOAuth2Provider.GITHUB
    // Hit /oauth2/authorization/github
 	@SuppressWarnings("static-method")
    public ClientRegistration githubClientRegistration() {
		return ClientRegistration.withRegistrationId("github")
									.clientId(UtilImpl.AUTHENTICATION_GITHUB_CLIENT_ID)
									.clientSecret(UtilImpl.AUTHENTICATION_GITHUB_SECRET)
									.clientAuthenticationMethod(ClientAuthenticationMethod.BASIC)
									.authorizationGrantType(AuthorizationGrantType.AUTHORIZATION_CODE)
									.redirectUriTemplate("{baseUrl}/login/oauth2/code/{registrationId}")
									.scope("read:user")
									.authorizationUri("https://github.com/login/oauth/authorize")
									.tokenUri("https://github.com/login/oauth/access_token")
									.userInfoUri("https://api.github.com/user")
									.userNameAttributeName("id")
									.clientName("GitHub")
									.build();
	}
}
