package org.skyve.impl.web.spring;

import java.io.IOException;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.skyve.util.Util;
import org.springframework.security.core.Authentication;
import org.springframework.security.web.authentication.SavedRequestAwareAuthenticationSuccessHandler;
import org.springframework.security.web.savedrequest.HttpSessionRequestCache;
import org.springframework.security.web.savedrequest.RequestCache;
import org.springframework.security.web.savedrequest.SavedRequest;

/**
 * This class will redirect to the saved URL after login unless that URL is not a regular url.
 * Like smart client AJAX request and omnifaces web sockets etc must redirect to the context URL after login.
 * But if its an xhtml or jsp or /? type request then it goes through for redirecting to after login.
 * @author mike
 */
public class SkyveAuthenticationSuccessHandler extends SavedRequestAwareAuthenticationSuccessHandler {
	@Override
	public void onAuthenticationSuccess(HttpServletRequest request,
											HttpServletResponse response,
											Authentication authentication) throws ServletException, IOException {
		RequestCache requestCache = new HttpSessionRequestCache();
		SavedRequest savedRequest = requestCache.getRequest(request, response);
		if (savedRequest != null) {
			String redirectUrl = savedRequest.getRedirectUrl();
			if (redirectUrl != null) {
				if (! (redirectUrl.contains(".xhtml") || 
						redirectUrl.contains(".jsp") ||
						redirectUrl.startsWith(Util.getHomeUrl() + "?"))) {
					requestCache.removeRequest(request, response);
					clearAuthenticationAttributes(request);
					getRedirectStrategy().sendRedirect(request, response, Util.getSkyveContextUrl());
					return;
				}
			}
		}
		super.onAuthenticationSuccess(request, response, authentication);
	}
}
