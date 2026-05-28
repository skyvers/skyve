package org.skyve.impl.web.spring;

import java.io.IOException;

import org.springframework.security.core.AuthenticationException;
import org.springframework.security.web.authentication.ForwardAuthenticationFailureHandler;

import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

public class TwoFactorAuthForwardHandler extends ForwardAuthenticationFailureHandler {
	/**
	 * Request attribute key indicating an invalid two-factor code was supplied.
	 */
	public static final String TWO_FACTOR_AUTH_ERROR_ATTRIBUTE = "tfaerror";
	
	/**
	 * Creates a failure handler that forwards authentication failures to the supplied URL.
	 *
	 * @param forwardUrl the servlet path used for forwarding authentication failures
	 */
	public TwoFactorAuthForwardHandler(String forwardUrl) {
		super(forwardUrl);
	}

	/**
	 * Marks invalid two-factor submissions before delegating to the standard forward handler.
	 *
	 * @param request the HTTP request associated with the authentication failure
	 * @param response the HTTP response associated with the authentication failure
	 * @param exception the authentication exception raised by Spring Security
	 * @throws IOException when forwarding fails due to I/O errors
	 * @throws ServletException when forwarding fails in the servlet container
	 */
	@Override
	public void onAuthenticationFailure(HttpServletRequest request,
											HttpServletResponse response,
		AuthenticationException exception)
	throws IOException, ServletException {
		if ((exception instanceof TwoFactorAuthRequiredException tfaEx) && tfaEx.isInvalidTwoFactorCode()) {
			request.setAttribute(TWO_FACTOR_AUTH_ERROR_ATTRIBUTE, "1");
		}
		
		super.onAuthenticationFailure(request, response, exception);
	}
}
