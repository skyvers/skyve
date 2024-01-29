package org.skyve.impl.web.spring;

import java.io.IOException;
import java.util.Base64;

import org.skyve.impl.util.UtilImpl;
import org.springframework.security.authentication.AnonymousAuthenticationToken;
import org.springframework.security.authentication.AuthenticationDetailsSource;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.AuthenticationException;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.web.AuthenticationEntryPoint;
import org.springframework.security.web.authentication.NullRememberMeServices;
import org.springframework.security.web.authentication.RememberMeServices;
import org.springframework.security.web.authentication.WebAuthenticationDetailsSource;
import org.springframework.security.web.authentication.www.BasicAuthenticationFilter;

import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

/**
 * Processes a HTTP request's BASIC authorization headers, putting the result into the
 * <code>SecurityContextHolder</code>.<p/>
 * 
 * This is an extension of Spring's BasicAuthenticationFilter with the customer prepended
 * to the user name as it would be from the login form.
 * The only differing line is marked below, the other LOC exists to get around private visibility
 * in the spring class.
 * 
 * security.xml looks like...
 * 
 * 	<authentication-manager id="authenticationManager">
 *      ...
 *  </authentication-manager>
 *  
 *  <http auto-config="false" use-expressions="true" entry-point-ref="skyveEntryPoint">
 *      <custom-filter ref="skyveFilter" before="BASIC_AUTH_FILTER" />
 *      ...
 *  </http>
 *  <b:bean id="skyveEntryPoint" class="org.springframework.security.web.authentication.www.BasicAuthenticationEntryPoint">
 *      <b:property name="realmName" value="Skyve" />
 *  </b:bean>
 *  <b:bean id="skyveFilter" class="org.skyve.impl.web.spring.SingleCustomerBasicAuthenticationFilter">
 *      <b:constructor-arg name="authenticationManager" ref="authenticationManager" />
 *  </b:bean>
 */
public class SingleCustomerBasicAuthenticationFilter extends BasicAuthenticationFilter {
	public SingleCustomerBasicAuthenticationFilter(AuthenticationManager authenticationManager) {
		super(authenticationManager);
	}

	public SingleCustomerBasicAuthenticationFilter(AuthenticationManager authenticationManager,
										AuthenticationEntryPoint authenticationEntryPoint) {
		super(authenticationManager, authenticationEntryPoint);
	}
	
	@Override
	protected void doFilterInternal(HttpServletRequest request, HttpServletResponse response, FilterChain chain)
	throws IOException, ServletException {
		final boolean debug = this.logger.isDebugEnabled();

		String header = request.getHeader("Authorization");

		if (header == null || !header.startsWith("Basic ")) {
			chain.doFilter(request, response);
			return;
		}

		try {
			String[] tokens = extractAndDecodeHeader(header, request);
			assert tokens.length == 2;

			String username = tokens[0];

			if (debug) {
				this.logger
						.debug("Basic Authentication Authorization header found for user '"
								+ username + "'");
			}

			if (authenticationRequired(username)) {
				UsernamePasswordAuthenticationToken authRequest = new UsernamePasswordAuthenticationToken(
						username, tokens[1]);
				authRequest.setDetails(
						this.authenticationDetailsSource.buildDetails(request));
				Authentication authResult = getAuthenticationManager().authenticate(authRequest);

				if (debug) {
					this.logger.debug("Authentication success: " + authResult);
				}

				SecurityContextHolder.getContext().setAuthentication(authResult);

				this.rememberMeServices.loginSuccess(request, response, authResult);

				onSuccessfulAuthentication(request, response, authResult);
			}

		}
		catch (AuthenticationException failed) {
			SecurityContextHolder.clearContext();

			if (debug) {
				this.logger.debug("Authentication request for failed: " + failed);
			}

			this.rememberMeServices.loginFail(request, response);

			onUnsuccessfulAuthentication(request, response, failed);

			if (isIgnoreFailure()) {
				chain.doFilter(request, response);
			}
			else {
				getAuthenticationEntryPoint().commence(request, response, failed);
			}

			return;
		}

		chain.doFilter(request, response);
	}

	private String[] extractAndDecodeHeader(String header, HttpServletRequest request)
	throws IOException {
		byte[] base64Token = header.substring(6).getBytes("UTF-8");
		byte[] decoded;
		try {
			decoded = Base64.getDecoder().decode(base64Token);
		}
		catch (@SuppressWarnings("unused") IllegalArgumentException e) {
			throw new BadCredentialsException("Failed to decode basic authentication token");
		}

		String token = new String(decoded, getCredentialsCharset(request));
		int delim = token.indexOf(":");

		if (delim == -1) {
			throw new BadCredentialsException("Invalid basic authentication token");
		}
		// THIS IS THE ONLY REAL CHANGE TO THE BASE CLASS
		return new String[] { UtilImpl.CUSTOMER + '/' + token.substring(0, delim), token.substring(delim + 1) };
		// THIS IS THE ONLY REAL CHANGE TO THE BASE CLASS
	}

	private static boolean authenticationRequired(String username) {
		Authentication existingAuth = SecurityContextHolder.getContext().getAuthentication();
		if (existingAuth == null || ! existingAuth.isAuthenticated()) {
			return true;
		}
		if (existingAuth instanceof UsernamePasswordAuthenticationToken && ! existingAuth.getName().equals(username)) {
			return true;
		}
		if (existingAuth instanceof AnonymousAuthenticationToken) {
			return true;
		}

		return false;
	}

	private AuthenticationDetailsSource<HttpServletRequest, ?> authenticationDetailsSource = new WebAuthenticationDetailsSource();
	@Override
	public void setAuthenticationDetailsSource(AuthenticationDetailsSource<HttpServletRequest, ?> authenticationDetailsSource) {
		this.authenticationDetailsSource = authenticationDetailsSource;
		super.setAuthenticationDetailsSource(authenticationDetailsSource);
	}

	private RememberMeServices rememberMeServices =  new NullRememberMeServices();
	@Override
	public void setRememberMeServices(RememberMeServices rememberMeServices) {
		this.rememberMeServices = rememberMeServices;
		super.setRememberMeServices(rememberMeServices);
	}
}
