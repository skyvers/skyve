package org.skyve.impl.web.spring;

import org.springframework.security.web.authentication.ForwardAuthenticationFailureHandler;

public class TwoFactorAuthenticationForwardHandler  extends ForwardAuthenticationFailureHandler {

	public TwoFactorAuthenticationForwardHandler(String forwardUrl) {
		super(forwardUrl);
	}

//	@Override
//	public void onAuthenticationFailure(HttpServletRequest request, HttpServletResponse response,
//			AuthenticationException exception) throws IOException, ServletException {
//		
//		if (exception instanceof TwoFactorAuthRequiredException) {
//			TwoFactorAuthRequiredException tfaEx = (TwoFactorAuthRequiredException) exception;
//			
//			if (tfaEx.isAuthenticationFailure()) {
//				request.setAttribute("error", "1");
//			} else {
//				UtilImpl.LOGGER.info("ELTRACEDEV show OTP and set attributes");
//			}
//		}
//		
//		super.onAuthenticationFailure(request, response, exception);
//	}
//	
	
}
