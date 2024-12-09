package org.skyve.impl.web.filter;

import java.io.IOException;
import java.security.Principal;
import java.util.Enumeration;

import org.skyve.impl.cache.StateUtil;
import org.skyve.impl.metadata.user.UserImpl;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.WebStatsUtil;
import org.skyve.impl.web.UserAgent;
import org.skyve.impl.web.WebContainer;
import org.skyve.util.Monitoring;
import org.skyve.util.logging.Category;
import org.skyve.web.UserAgentType;
import org.skyve.web.WebContext;
import org.slf4j.Logger;

import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.ServletRequest;
import jakarta.servlet.ServletResponse;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.servlet.http.HttpSession;

/**
 * Log and collect stats on requests, if this is not a static resource.
 */
public class RequestLoggingAndStatisticsFilter extends ExcludeStaticFilter {

    private static final Logger HTTP_LOGGER = Category.HTTP.logger();
    private static final Logger COMMAND_LOGGER = Category.COMMAND.logger();

    @Override
	public void doFilter(ServletRequest request, ServletResponse response, FilterChain chain)
	throws IOException, ServletException {
		HttpServletRequest httpRequest = (HttpServletRequest) request;
    	if (staticURLPrefix(httpRequest)) {
    		chain.doFilter(request, response);
    		return;
    	}

		try {
			// Set the request and response in WebContainer
			WebContainer.setHttpServletRequestResponse((HttpServletRequest) request, (HttpServletResponse) response);

			if (UtilImpl.HTTP_TRACE) {
				HTTP_LOGGER.info("*********************************** REQUEST ************************************");
				HTTP_LOGGER.info("ContextPath=" + httpRequest.getContextPath());
				HTTP_LOGGER.info("LocalAddr=" + request.getLocalAddr());
				HTTP_LOGGER.info("LocalName=" + request.getLocalName());
				HTTP_LOGGER.info("LocalPort=" + request.getLocalPort());
				HTTP_LOGGER.info("Method=" + httpRequest.getMethod());
				HTTP_LOGGER.info("PathInfo=" + httpRequest.getPathInfo());
				HTTP_LOGGER.info("PathTranslated=" + httpRequest.getPathTranslated());
				HTTP_LOGGER.info("Protocol=" + request.getProtocol());
				HTTP_LOGGER.info("QueryString=" + httpRequest.getQueryString());
				HTTP_LOGGER.info("RemoteAddr=" + request.getRemoteAddr());
				HTTP_LOGGER.info("RemoteHost=" + request.getRemoteHost());
				HTTP_LOGGER.info("RemotePort=" + request.getRemotePort());
				HTTP_LOGGER.info("RemoteUser=" + httpRequest.getRemoteUser());
				HTTP_LOGGER.info("RequestedSessionId=" + httpRequest.getRequestedSessionId());
				HTTP_LOGGER.info("RequestURI=" + httpRequest.getRequestURI());
				HTTP_LOGGER.info("RequestURL=" + httpRequest.getRequestURL().toString());
				HTTP_LOGGER.info("Scheme=" + request.getScheme());
				HTTP_LOGGER.info("ServerName=" + request.getServerName());
				HTTP_LOGGER.info("ServerPort=" + request.getServerPort());
				HTTP_LOGGER.info("ServletPath=" + httpRequest.getServletPath());
				Principal principal = httpRequest.getUserPrincipal();
				HTTP_LOGGER.info("UserPrincipal=" + ((principal == null) ? "<null>" : principal.getName()));
				HTTP_LOGGER.info("********************************** PARAMETERS **********************************");
				Enumeration<String> parameterNames = request.getParameterNames();
				while (parameterNames.hasMoreElements()) {
					String parameterName = parameterNames.nextElement();
					if (parameterName != null) {
						if (parameterName.toLowerCase().contains("password")) {
							HTTP_LOGGER.info(parameterName + "=***PASSWORD***");
						}
						else {
							String[] parameterValues = request.getParameterValues(parameterName);
							if (parameterValues != null) {
								for (String parameterValue : parameterValues) {
									int parameterValueLength = parameterValue.length();
									if (parameterValueLength > 51200) { // 50K
										HTTP_LOGGER.info(parameterName + "=***LENGTH " + (parameterValueLength / 1024) + "K***");
									}
									else if (parameterValue.toLowerCase().contains("password")) {
										HTTP_LOGGER.info(parameterName + "=***CONTAINS PASSWORD***");
									}
									else {
										HTTP_LOGGER.info(String.format("%s=%s", parameterName, parameterValue));
									}
								}
							}
						}
					}
				}
				HTTP_LOGGER.info("*********************************** HEADERS ************************************");
				Enumeration<String> headerNames = httpRequest.getHeaderNames();
				while (headerNames.hasMoreElements()) {
					String headerName = headerNames.nextElement();
					HTTP_LOGGER.info(headerName + "=" + httpRequest.getHeader(headerName));
				}
				HTTP_LOGGER.info("***************************** SESSION/CONVERSATION *****************************");
				StateUtil.logStateStats();
			}

			HttpSession session = httpRequest.getSession(false);
			UserImpl user = (session == null) ? null : (UserImpl) session.getAttribute(WebContext.USER_SESSION_ATTRIBUTE_NAME);

			try {
				// It is possible that the user has not been determined for
				// this web request yet, so ignore this stat as these
				// types of requests are few and far between.
				if (user != null) {
					String userAgentHeader = httpRequest.getHeader("User-Agent");
					UserAgentType userAgentType = UserAgent.getType(httpRequest);
					WebStatsUtil.recordHit(user, userAgentHeader, userAgentType);
				}
				else {
					if (UtilImpl.COMMAND_TRACE) COMMAND_LOGGER.info("DIDNT RECORD HIT AS WE ARE NOT LOGGED IN");
				}
			}
			catch (Exception e) {
				throw new ServletException(e);
			}
			finally {
				// Determine CPU and MEM before
				double loadPre = Monitoring.systemLoadAverage();
				int memPctPre = Monitoring.percentageUsedMomory();
				long millis = System.currentTimeMillis();

				// pass the request/response on
				chain.doFilter(request, response);

				// Determine CPU and MEM after
				double loadPost = Monitoring.systemLoadAverage();
				int memPctPost = Monitoring.percentageUsedMomory();

				HTTP_LOGGER.info("******************************* TIMING/RESOURCES *******************************");
				HTTP_LOGGER.info(String.format("TIME=%,d PRE/POST(DELTA) CPU=%.2f/%.2f(%.2f) MEM=%d%%/%d%%(%d%%)",
						Long.valueOf(System.currentTimeMillis() - millis),
						Double.valueOf(loadPre), Double.valueOf(loadPost), Double.valueOf(loadPost - loadPre),
						Integer.valueOf(memPctPre), Integer.valueOf(memPctPost), Integer.valueOf(memPctPost - memPctPre)));
				if (UtilImpl.HTTP_TRACE)
				    HTTP_LOGGER.info("********************************************************************************");
			}
		} finally {
			// Clear the request/response in WebContainer
			WebContainer.clear();
		}
	}
}
