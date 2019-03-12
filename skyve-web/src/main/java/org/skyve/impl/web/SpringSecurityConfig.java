package org.skyve.impl.web;

import javax.naming.InitialContext;
import javax.sql.DataSource;

import org.skyve.impl.persistence.hibernate.AbstractHibernatePersistence;
import org.skyve.impl.persistence.hibernate.dialect.SkyveDialect;
import org.skyve.impl.persistence.hibernate.dialect.SkyveDialect.RDBMS;
import org.skyve.impl.util.UtilImpl;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.security.config.annotation.web.configuration.WebSecurityConfigurerAdapter;
import org.springframework.security.provisioning.JdbcUserDetailsManager;

/**
 * This class supplies named spring beans to the OOTB security.xml
 */
@Configuration
public class SpringSecurityConfig extends WebSecurityConfigurerAdapter {
	@Bean
	@SuppressWarnings("static-method")
	public DataSource dataSource() throws Exception {
		InitialContext ctx = new InitialContext();
		return (DataSource) ctx.lookup(UtilImpl.DATA_STORE.getJndiDataSourceName());
	}

	@Bean 
	public JdbcUserDetailsManager jdbcUserService() throws Exception {
		JdbcUserDetailsManager result = new JdbcUserDetailsManager();
		SkyveDialect dialect = AbstractHibernatePersistence.getDialect(UtilImpl.DATA_STORE.getDialectClassName());
		RDBMS rdbms = dialect.getRDBMS();
		if (RDBMS.h2.equals(rdbms)) {
			result.setUsersByUsernameQuery("select bizCustomer || '/' || userName, password, not ifNull(inactive, false) from ADM_SecurityUser where bizCustomer || '/' || userName = ?");
			result.setAuthoritiesByUsernameQuery("select bizCustomer || '/' || userName, 'NoAuth' from ADM_SecurityUser where bizCustomer || '/' || userName = ?");
			result.setGroupAuthoritiesByUsernameQuery("select bizCustomer || '/' || userName, bizCustomer || '/' || userName, 'NoAuth' from ADM_SecurityUser where bizCustomer || '/' || userName = ?");
		}
		else if (RDBMS.mysql.equals(rdbms)) {
			result.setUsersByUsernameQuery("select concat(bizCustomer, '/', userName), password, not ifNull(inactive, false) from ADM_SecurityUser where concat(bizCustomer, '/', userName) = ?");
			result.setAuthoritiesByUsernameQuery("select concat(bizCustomer, '/', userName), 'NoAuth' from ADM_SecurityUser where concat(bizCustomer, '/', userName) = ?");
			result.setGroupAuthoritiesByUsernameQuery("select concat(bizCustomer, '/', userName), concat(bizCustomer, '/', userName), 'NoAuth' from ADM_SecurityUser where concat(bizCustomer, '/', userName) = ?");
			
		}
		else if (RDBMS.sqlserver.equals(rdbms)) {
			result.setUsersByUsernameQuery("select bizCustomer + '/' + userName, password, case when inactive = 1 or not activated = 1 then 0 else 1 end from ADM_SecurityUser where bizCustomer + '/' + userName = ?");
			result.setAuthoritiesByUsernameQuery("select bizCustomer + '/' + userName, 'NoAuth' from ADM_SecurityUser where bizCustomer + '/' + userName = ?");
			result.setGroupAuthoritiesByUsernameQuery("select bizCustomer + '/' + userName, bizCustomer + '/' + userName, 'NoAuth' from ADM_SecurityUser where bizCustomer + '/' + userName = ?");
		}
		result.setDataSource(dataSource());
		result.setRolePrefix("none");
		return result;
	}
}
