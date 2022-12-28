package org.skyve.impl.web.spring;

import java.io.IOException;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.springframework.security.core.AuthenticationException;
import org.springframework.security.web.authentication.ForwardAuthenticationFailureHandler;

public class TwoFactorAuthForwardHandler extends ForwardAuthenticationFailureHandler {
	public static String TWO_FACTOR_AUTH_ERROR_ATTRIBUTE = "tfaerror";
	
	public TwoFactorAuthForwardHandler(String forwardUrl) {
		super(forwardUrl);
	}

	@Override
	public void onAuthenticationFailure(HttpServletRequest request,
											HttpServletResponse response,
											AuthenticationException exception)
	throws IOException, ServletException {
		if (exception instanceof TwoFactorAuthRequiredException) {
			TwoFactorAuthRequiredException tfaEx = (TwoFactorAuthRequiredException) exception;
			
			if (tfaEx.isAuthenticationFailure()) {
				request.setAttribute(TWO_FACTOR_AUTH_ERROR_ATTRIBUTE, "1");
			}
		}
		
		super.onAuthenticationFailure(request, response, exception);
	}
}
