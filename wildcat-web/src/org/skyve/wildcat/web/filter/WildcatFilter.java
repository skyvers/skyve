package org.skyve.wildcat.web.filter;

import java.io.IOException;
import java.util.Enumeration;

import javax.servlet.Filter;
import javax.servlet.FilterChain;
import javax.servlet.FilterConfig;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletRequest;

import org.skyve.web.WebContext;
import org.skyve.wildcat.metadata.user.UserImpl;
import org.skyve.wildcat.util.UtilImpl;
import org.skyve.wildcat.util.WebStatsUtil;
import org.skyve.wildcat.web.WebUtil;

public class WildcatFilter implements Filter {
	@Override
	public void doFilter(ServletRequest request, ServletResponse response, FilterChain chain)
	throws IOException, ServletException {
		if (UtilImpl.HTTP_TRACE) {
			HttpServletRequest httpRequest = (HttpServletRequest) request;
			UtilImpl.LOGGER.info("************************** REQUEST ***************************");
			UtilImpl.LOGGER.info("ContextPath=" + httpRequest.getContextPath());
			UtilImpl.LOGGER.info("LocalAddr=" + request.getLocalAddr());
			UtilImpl.LOGGER.info("LocalName=" + request.getLocalName());
			UtilImpl.LOGGER.info("LocalPort=" + request.getLocalPort());
			UtilImpl.LOGGER.info("Method=" + httpRequest.getMethod());
			UtilImpl.LOGGER.info("PathInfo=" + httpRequest.getPathInfo());
			UtilImpl.LOGGER.info("PathTranslated=" + httpRequest.getPathTranslated());
			UtilImpl.LOGGER.info("Protocol=" + request.getProtocol());
			UtilImpl.LOGGER.info("QueryString=" + httpRequest.getQueryString());
			UtilImpl.LOGGER.info("RemoteAddr=" + request.getRemoteAddr());
			UtilImpl.LOGGER.info("RemoteHost=" + request.getRemoteHost());
			UtilImpl.LOGGER.info("RemotePort=" + request.getRemotePort());
			UtilImpl.LOGGER.info("RemoteUser=" + httpRequest.getRemoteUser());
			UtilImpl.LOGGER.info("RequestedSessionId=" + httpRequest.getRequestedSessionId());
			UtilImpl.LOGGER.info("RequestURI=" + httpRequest.getRequestURI());
			UtilImpl.LOGGER.info("RequestURL=" + httpRequest.getRequestURL().toString());
			UtilImpl.LOGGER.info("Scheme=" + request.getScheme());
			UtilImpl.LOGGER.info("ServerName=" + request.getServerName());
			UtilImpl.LOGGER.info("ServerPort=" + request.getServerPort());
			UtilImpl.LOGGER.info("ServletPath=" + httpRequest.getServletPath());
			UtilImpl.LOGGER.info("UserPrincipal=" + httpRequest.getUserPrincipal());
			UtilImpl.LOGGER.info("************************* PARAMETERS *************************");
			Enumeration<String> parameterNames = request.getParameterNames();
			while (parameterNames.hasMoreElements()) {
				String parameterName = parameterNames.nextElement();
				UtilImpl.LOGGER.info(parameterName + "=" + request.getParameter(parameterName));
			}
			UtilImpl.LOGGER.info("************************** HEADERS ***************************");
			Enumeration<String> headerNames = httpRequest.getHeaderNames();
			while (headerNames.hasMoreElements()) {
				String headerName = headerNames.nextElement();
				UtilImpl.LOGGER.info(headerName + "=" + httpRequest.getHeader(headerName));
			}
			UtilImpl.LOGGER.info("************************** CACHE *****************************");
			WebUtil.logConversationsStats();
		}

		UserImpl user = (UserImpl) ((HttpServletRequest) request).getSession().getAttribute(WebContext.USER_SESSION_ATTRIBUTE_NAME);

		try {
			// It is possible that the user has not been determined for
			// this web request yet, so ignore this stat as these
			// types of requests are few and far between.
			if (user != null) {
				WebStatsUtil.recordHit(user);
			}
			else {
				if (UtilImpl.COMMAND_TRACE) UtilImpl.LOGGER.info("DIDNT RECORD HIT AS WE ARE NOT LOGGED IN");
			}
		}
		catch (Exception e) {
			throw new ServletException(e);
		}
		finally {
			// pass the request/response on
			chain.doFilter(request, response);
		}
	}

	@Override
	public synchronized void init(FilterConfig config) throws ServletException {
		// nothing to do here
	}

	@Override
	public synchronized void destroy() {
		// nothing to do here
	}
}
