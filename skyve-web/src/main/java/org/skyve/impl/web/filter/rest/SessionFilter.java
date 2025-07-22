package org.skyve.impl.web.filter.rest;

import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.security.Principal;

import org.skyve.domain.messages.SessionEndedException;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.web.WebUtil;
import org.skyve.metadata.user.User;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.ServletRequest;
import jakarta.servlet.ServletResponse;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

public class SessionFilter extends AbstractRestFilter {

    private static final Logger LOGGER = LoggerFactory.getLogger(SessionFilter.class);

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
			try {
				persistence = AbstractPersistence.get();
				persistence.evictAllCached();
				persistence.begin();
	
		    	Principal userPrincipal = httpRequest.getUserPrincipal();
				User user = WebUtil.processUserPrincipalForRequest(httpRequest, 
																	(userPrincipal == null) ? null : userPrincipal.getName());
				if (user == null) {
			    	error(persistence, httpResponse, HttpServletResponse.SC_UNAUTHORIZED, realm, new SessionEndedException(httpRequest.getLocale()).getMessage());
			    	return;
				}
				persistence.setUser(user);
	
				chain.doFilter(request, response);
			}
			catch (InvocationTargetException e) {
				throw e.getTargetException();
			}
		}
		catch (Throwable t) {
			if (persistence != null) {
				persistence.rollback();
			}
			
			LOGGER.error(t.getLocalizedMessage(), t);
			error(persistence, httpResponse, t.getLocalizedMessage());
		}
		finally {
			if (persistence != null) {
				persistence.commit(true);
			}
		}
	}
}
