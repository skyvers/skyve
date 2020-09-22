package org.skyve.impl.web.spring;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Timestamp;
import java.util.List;

import javax.naming.InitialContext;
import javax.sql.DataSource;

import org.skyve.impl.persistence.hibernate.AbstractHibernatePersistence;
import org.skyve.impl.persistence.hibernate.dialect.SkyveDialect;
import org.skyve.impl.persistence.hibernate.dialect.SkyveDialect.RDBMS;
import org.skyve.impl.util.UtilImpl;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.jdbc.core.RowMapper;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.config.annotation.web.configuration.WebSecurityConfigurerAdapter;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.authority.AuthorityUtils;
import org.springframework.security.core.userdetails.User;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.provisioning.JdbcUserDetailsManager;

/**
 * This class supplies named spring beans to the OOTB security.xml
 */
@Configuration
@EnableWebSecurity
public class SpringSecurityConfig extends WebSecurityConfigurerAdapter {
	@Bean
	@SuppressWarnings("static-method")
	public DataSource dataSource() throws Exception {
		InitialContext ctx = new InitialContext();
		return (DataSource) ctx.lookup(UtilImpl.DATA_STORE.getJndiDataSourceName());
	}

	@Bean 
	public JdbcUserDetailsManager jdbcUserService() throws Exception {
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
