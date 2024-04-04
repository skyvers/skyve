package org.skyve.impl.web.filter;

import java.io.IOException;
import java.security.Principal;
import java.util.Enumeration;

import org.skyve.impl.cache.StateUtil;
import org.skyve.impl.metadata.user.UserImpl;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.WebStatsUtil;
import org.skyve.impl.web.UserAgent;
import org.skyve.util.Monitoring;
import org.skyve.util.Util;
import org.skyve.web.UserAgentType;
import org.skyve.web.WebContext;

import jakarta.servlet.Filter;
import jakarta.servlet.FilterChain;
import jakarta.servlet.FilterConfig;
import jakarta.servlet.ServletException;
import jakarta.servlet.ServletRequest;
import jakarta.servlet.ServletResponse;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpSession;

public class RequestLoggingAndStatisticsFilter implements Filter {
    // A list of all excluded URL prefixes
    private String[] excludedURLPrefixes;

	@Override
	public void init(FilterConfig config) throws ServletException {
		String urls = Util.processStringValue(config.getInitParameter("excluded"));
		if (urls != null) {
			excludedURLPrefixes = urls.split("\n");
			for (int i = 0, l = excludedURLPrefixes.length; i < l; i++) {
				excludedURLPrefixes[i] = Util.processStringValue(excludedURLPrefixes[i]);
			}
		}
	}

	@Override
	public void destroy() {
		excludedURLPrefixes = null;
	}

    @Override
	public void doFilter(ServletRequest request, ServletResponse response, FilterChain chain)
	throws IOException, ServletException {
		HttpServletRequest httpRequest = (HttpServletRequest) request;
		String servletPath = httpRequest.getServletPath();

        // Test if this URL is unsecured in the web.xml, and bug out if so
        // NB can't use queryString here as there could be AJAX posts etc in faces so not good practice
        if (excludedURLPrefixes != null) {
	        for (String excludedURLPrefix : excludedURLPrefixes) {
	        	if ((excludedURLPrefix != null) && servletPath.startsWith(excludedURLPrefix)) {
        			chain.doFilter(request, response);
	        		return;
	        	}
	        }
        }
 
        if (UtilImpl.HTTP_TRACE) {
			UtilImpl.LOGGER.info("*********************************** REQUEST ************************************");
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
			UtilImpl.LOGGER.info("ServletPath=" + servletPath);
			Principal principal = httpRequest.getUserPrincipal();
			UtilImpl.LOGGER.info("UserPrincipal=" + ((principal == null) ? "<null>" : principal.getName()));
			UtilImpl.LOGGER.info("********************************** PARAMETERS **********************************");
			Enumeration<String> parameterNames = request.getParameterNames();
			while (parameterNames.hasMoreElements()) {
				String parameterName = parameterNames.nextElement();
				if (parameterName != null) {
					if (parameterName.toLowerCase().contains("password")) {
						UtilImpl.LOGGER.info(parameterName + "=***PASSWORD***");
					}
					else {
						String[] parameterValues = request.getParameterValues(parameterName);
						if (parameterValues != null) {
							for (String parameterValue : parameterValues) {
								int parameterValueLength = parameterValue.length();
								if (parameterValueLength > 51200) { // 50K
									UtilImpl.LOGGER.info(parameterName + "=***LENGTH " + (parameterValueLength / 1024) + "K***");
								}
								else if (parameterValue.toLowerCase().contains("password")) {
									UtilImpl.LOGGER.info(parameterName + "=***CONTAINS PASSWORD***");
								}
								else {
									UtilImpl.LOGGER.info(String.format("%s=%s", parameterName, parameterValue));
								}
							}
						}
					}
				}
			}
			UtilImpl.LOGGER.info("*********************************** HEADERS ************************************");
			Enumeration<String> headerNames = httpRequest.getHeaderNames();
			while (headerNames.hasMoreElements()) {
				String headerName = headerNames.nextElement();
				UtilImpl.LOGGER.info(headerName + "=" + httpRequest.getHeader(headerName));
			}
			UtilImpl.LOGGER.info("***************************** SESSION/CONVERSATION *****************************");
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
				if (UtilImpl.COMMAND_TRACE) UtilImpl.LOGGER.info("DIDNT RECORD HIT AS WE ARE NOT LOGGED IN");
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

			UtilImpl.LOGGER.info("******************************* TIMING/RESOURCES *******************************");
			UtilImpl.LOGGER.info(String.format("TIME=%,d PRE/POST(DELTA) CPU=%.2f/%.2f(%.2f) MEM=%d%%/%d%%(%d%%)",
													Long.valueOf(System.currentTimeMillis() - millis),
													Double.valueOf(loadPre), Double.valueOf(loadPost), Double.valueOf(loadPost - loadPre),
													Integer.valueOf(memPctPre), Integer.valueOf(memPctPost), Integer.valueOf(memPctPost - memPctPre)));
			if (UtilImpl.HTTP_TRACE) UtilImpl.LOGGER.info("********************************************************************************");
		}
	}
}
