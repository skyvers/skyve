package org.skyve.impl.web.spring;

import java.io.IOException;

import org.skyve.impl.util.TwoFactorAuthConfigurationSingleton;
import org.skyve.impl.util.UtilImpl;
import org.skyve.util.Util;
import org.springframework.security.core.Authentication;
import org.springframework.security.provisioning.UserDetailsManager;
import org.springframework.security.web.authentication.SavedRequestAwareAuthenticationSuccessHandler;
import org.springframework.security.web.savedrequest.DefaultSavedRequest;
import org.springframework.security.web.savedrequest.HttpSessionRequestCache;
import org.springframework.security.web.savedrequest.RequestCache;
import org.springframework.security.web.savedrequest.SavedRequest;

import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

/**
 * This class will redirect to the saved URL after login unless that URL is not a regular url.
 * Like smart client AJAX request and omnifaces web sockets etc must redirect to the home URL after login.
 * But if its an xhtml or jsp or /? type request then it goes through for redirecting to after login.
 * Also, if Skyve is running behind a TLS terminating proxy server then the request will need to be doctored.
 * @author mike
 */
public class SkyveAuthenticationSuccessHandler extends SavedRequestAwareAuthenticationSuccessHandler {
	private UserDetailsManager userDetailsManager;
	
	public SkyveAuthenticationSuccessHandler(UserDetailsManager userDetailsManager) {
		this.userDetailsManager = userDetailsManager;
	}
	
	@Override
	public void onAuthenticationSuccess(HttpServletRequest request,
										HttpServletResponse response,
										Authentication authentication)
	throws ServletException, IOException {
		String redirectUrl = null;
		RequestCache requestCache = new HttpSessionRequestCache();
		SavedRequest savedRequest = requestCache.getRequest(request, response);
		if (savedRequest != null) {
			redirectUrl = savedRequest.getRedirectUrl();
			if (redirectUrl != null) {
				UtilImpl.LOGGER.info("Redirect after login requested to " + redirectUrl);
				// its http behind proxy server terminating TLS or some other edge case
				if (Util.isSecureUrl() && redirectUrl.startsWith("http://")) { // could be https:// or ws:// or wss://
					if (savedRequest instanceof DefaultSavedRequest) {
						// Remake the url from the skyve server URL, the request URI and any query parameters
						DefaultSavedRequest defaultSavedRequest = (DefaultSavedRequest) savedRequest;
						StringBuilder url = new StringBuilder(256);
						url.append(Util.getServerUrl()).append(defaultSavedRequest.getRequestURI());
						String query = defaultSavedRequest.getQueryString();
						if (query != null) {
							url.append('?').append(query);
						}
						redirectUrl = url.toString();
					}
					else {
						// Can't do much without a good saved request, so go home
						redirectUrl = Util.getHomeUrl();
					}
				}
				// If this is an XHR or some other exotic URL, then go home
				if (! (redirectUrl.contains(".xhtml") || 
						redirectUrl.contains(".jsp") ||
						redirectUrl.startsWith(Util.getHomeUrl() + "?"))) {
					redirectUrl = Util.getHomeUrl();
				}
			}
			else {
				// No redirect URL, so go home
				redirectUrl = Util.getHomeUrl();
			}
		}
		else {
			// no saved request, so go home
			redirectUrl = Util.getHomeUrl();
		}
		
		if (userDetailsManager != null) {
			Object principal = authentication.getPrincipal();
			if (principal instanceof TwoFactorAuthUser) {
				TwoFactorAuthUser tfaUser = (TwoFactorAuthUser) principal;
				String customerName = tfaUser.getCustomer();
				if (customerName != null) {
					if (TwoFactorAuthConfigurationSingleton.getInstance().isPushTfa(customerName)) {
						cleanupTFACodes(tfaUser);
					}
				}
			}
		}
		
		UtilImpl.LOGGER.info("Redirected to " + redirectUrl);
		requestCache.removeRequest(request, response);
		clearAuthenticationAttributes(request);
		getRedirectStrategy().sendRedirect(request, response, redirectUrl);
	}
	
	private void cleanupTFACodes(TwoFactorAuthUser principal) {
		principal.setTfaCodeGeneratedTimestamp(null);
		principal.setTfaCode(null);
		principal.setTfaToken(null);
		userDetailsManager.updateUser(principal);
	}
}
