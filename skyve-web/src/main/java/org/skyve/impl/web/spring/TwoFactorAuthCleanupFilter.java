package org.skyve.impl.web.spring;

import java.io.IOException;

import javax.servlet.FilterChain;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletRequest;

import org.skyve.impl.util.UtilImpl;
import org.springframework.security.provisioning.JdbcUserDetailsManager;
import org.springframework.security.web.authentication.UsernamePasswordAuthenticationFilter;
import org.springframework.security.web.util.matcher.AntPathRequestMatcher;
import org.springframework.web.filter.GenericFilterBean;

public class TwoFactorAuthCleanupFilter extends GenericFilterBean{
	
	private static final AntPathRequestMatcher DEFAULT_ANT_PATH_REQUEST_MATCHER = new AntPathRequestMatcher("/loginAttempt",
			"POST");
	private JdbcUserDetailsManager userDetailsService;
	
	public TwoFactorAuthCleanupFilter (JdbcUserDetailsManager userDetailsService) {
		this.userDetailsService = userDetailsService;
	}

	@Override
	public void doFilter(ServletRequest request, ServletResponse response, FilterChain chain)
			throws IOException, ServletException {
		
		if (!DEFAULT_ANT_PATH_REQUEST_MATCHER.matches((HttpServletRequest)  request)) {
			chain.doFilter(request, response);
			return;
		}
		
		if (SkyveSpringSecurity.isTFAPush()) {
			
			UtilImpl.LOGGER.info("Clearing up 2fa codes for user.");
			
			String username =  request.getParameter(UsernamePasswordAuthenticationFilter.SPRING_SECURITY_FORM_USERNAME_KEY);
			UserTFA user = (UserTFA) userDetailsService.loadUserByUsername(username);
			user.setTfaCodeGeneratedDateTime(null);
			user.setTfaCode(null);
			user.setTfaToken(null);
			userDetailsService.updateUser(user);
		}
		
		chain.doFilter(request, response);
	}
	
}
