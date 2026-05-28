package org.skyve.impl.web.filter.rest;

import java.io.IOException;
import java.security.Principal;

import org.skyve.domain.messages.SessionEndedException;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.web.WebErrorUtil;
import org.skyve.impl.web.WebUtil;
import org.skyve.metadata.user.User;
import org.slf4j.Logger;
import org.skyve.util.logging.SkyveLoggerFactory;

import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.ServletRequest;
import jakarta.servlet.ServletResponse;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

/**
 * Resolves the authenticated servlet principal into a Skyve {@link User} and binds it to persistence for REST calls.
 */
public class SessionFilter extends AbstractRestFilter {
    private static final Logger LOGGER = SkyveLoggerFactory.getLogger(SessionFilter.class);

	/**
	 * Validates session-backed REST access and establishes the persistence user context for the request.
	 *
	 * @param request inbound servlet request
	 * @param response outbound servlet response
	 * @param chain downstream filter chain
	 * @throws IOException when the response cannot be written
	 * @throws ServletException when downstream servlet processing fails
	 */
	@Override
	public void doFilter(ServletRequest request, ServletResponse response, FilterChain chain)
	throws IOException, ServletException {
		if (super.doUnsecuredFilter(request, response, chain)) {
			return;
		}
		
		HttpServletRequest httpRequest = (HttpServletRequest) request;
		HttpServletResponse httpResponse = (HttpServletResponse) response;
		
		AbstractPersistence persistence = null;
		try {
			persistence = AbstractPersistence.get();
			persistence.evictAllCached();
			persistence.begin();

		    	Principal userPrincipal = httpRequest.getUserPrincipal();
			User user = WebUtil.processUserPrincipalForRequest(httpRequest,
														(userPrincipal == null) ? null : userPrincipal.getName());
			if (user == null) {
				error(persistence, httpRequest, httpResponse, HttpServletResponse.SC_UNAUTHORIZED, realm, new SessionEndedException(httpRequest.getLocale()).getMessage());
		    	return;
			}
			persistence.setUser(user);

			chain.doFilter(request, response);
		}
		catch (Throwable t) {
			if (persistence != null) {
				persistence.rollback();
			}
			
			String reference = WebErrorUtil.logUnexpectedAndGetReference(LOGGER, "REST session filter failed", t);
			error(persistence, httpRequest, httpResponse, WebErrorUtil.genericMessage(reference));
		}
		finally {
			if (persistence != null) {
				persistence.commit(true);
			}
		}
	}
}
