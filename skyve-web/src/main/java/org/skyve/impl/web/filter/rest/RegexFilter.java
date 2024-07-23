package org.skyve.impl.web.filter.rest;

import java.io.IOException;
import java.security.Principal;
import java.util.Enumeration;
import java.util.Map;
import java.util.Map.Entry;
import java.util.TreeMap;

import org.skyve.impl.util.UtilImpl;
import org.skyve.util.Util;

import jakarta.servlet.FilterChain;
import jakarta.servlet.FilterConfig;
import jakarta.servlet.ServletException;
import jakarta.servlet.ServletRequest;
import jakarta.servlet.ServletResponse;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

public class RegexFilter extends AbstractRestFilter {
	private Map<String, String> initParameters = new TreeMap<>();
	
	@Override
	public void init(FilterConfig config) throws ServletException {
		super.init(config);
		
		Enumeration<String> e = config.getInitParameterNames();
		while (e.hasMoreElements()) {
			String initParameterName = UtilImpl.processStringValue(e.nextElement());
			String initParameterValue =  Util.processStringValue(config.getInitParameter(initParameterName));
			if ((initParameterName != null) && 
					(initParameterValue != null) && 
					(! REALM_INIT_PARAMETER.equals(initParameterName)) && 
					(! UNSECURED_INIT_PARAMETER.equals(initParameterName))) {
				initParameters.put(initParameterName, initParameterValue);
			}
		}
	}
	
	/**
	 * Exposed to perform initialisation programmatically if required.
	 * @return	The init parameters.
	 */
	protected Map<String, String> getInitParameters() {
		return initParameters;
	}
	
	@Override
	public void doFilter(ServletRequest request, ServletResponse response, FilterChain chain)
	throws IOException, ServletException {
		HttpServletRequest httpRequest = (HttpServletRequest) request;
		HttpServletResponse httpResponse = (HttpServletResponse) response;

		for (Entry<String, String> initParameter : initParameters.entrySet()) {
			String name = initParameter.getKey();
			String regex = initParameter.getValue();
			if ("ContextPath".equals(name) && fails(httpRequest.getContextPath(), regex)) {
				error(httpResponse, HttpServletResponse.SC_FORBIDDEN, "Forbidden");
				return;
			}
			else if ("LocalAddr".equals(name) && fails(httpRequest.getLocalAddr(), regex)) {
				error(httpResponse, HttpServletResponse.SC_FORBIDDEN, "Forbidden");
				return;
			}
			else if ("LocalName".equals(name) && fails(httpRequest.getLocalName(), regex)) {
				error(httpResponse, HttpServletResponse.SC_FORBIDDEN, "Forbidden");
				return;
			}
			else if ("LocalPort".equals(name) && fails(String.valueOf(httpRequest.getLocalPort()), regex)) {
				error(httpResponse, HttpServletResponse.SC_FORBIDDEN, "Forbidden");
				return;
			}
			else if ("Method".equals(name) && fails(httpRequest.getMethod(), regex)) {
				error(httpResponse, HttpServletResponse.SC_FORBIDDEN, "Forbidden");
				return;
			}
			else if ("PathInfo".equals(name) && fails(httpRequest.getPathInfo(), regex)) {
				error(httpResponse, HttpServletResponse.SC_FORBIDDEN, "Forbidden");
				return;
			}
			else if ("PathTranslated".equals(name) && fails(httpRequest.getPathTranslated(), regex)) {
				error(httpResponse, HttpServletResponse.SC_FORBIDDEN, "Forbidden");
				return;
			}
			else if ("Protocol".equals(name) && fails(httpRequest.getProtocol(), regex)) {
				error(httpResponse, HttpServletResponse.SC_FORBIDDEN, "Forbidden");
				return;
			}
			else if ("QueryString".equals(name) && fails(httpRequest.getQueryString(), regex)) {
				error(httpResponse, HttpServletResponse.SC_FORBIDDEN, "Forbidden");
				return;
			}
			else if ("RemoteAddr".equals(name) && fails(httpRequest.getRemoteAddr(), regex)) {
				error(httpResponse, HttpServletResponse.SC_FORBIDDEN, "Forbidden");
				return;
			}
			else if ("RemoteHost".equals(name) && fails(httpRequest.getRemoteHost(), regex)) {
				error(httpResponse, HttpServletResponse.SC_FORBIDDEN, "Forbidden");
				return;
			}
			else if ("RemotePort".equals(name) && fails(String.valueOf(httpRequest.getRemotePort()), regex)) {
				error(httpResponse, HttpServletResponse.SC_FORBIDDEN, "Forbidden");
				return;
			}
			else if ("RemoteUser".equals(name) && fails(httpRequest.getRemoteUser(), regex)) {
				error(httpResponse, HttpServletResponse.SC_FORBIDDEN, "Forbidden");
				return;
			}
			else if ("RequestedSessionId".equals(name) && fails(httpRequest.getRequestedSessionId(), regex)) {
				error(httpResponse, HttpServletResponse.SC_FORBIDDEN, "Forbidden");
				return;
			}
			else if ("RequestURI".equals(name) && fails(httpRequest.getRequestURI(), regex)) {
				error(httpResponse, HttpServletResponse.SC_FORBIDDEN, "Forbidden");
				return;
			}
			else if ("RequestURL".equals(name)) {
				StringBuffer value = httpRequest.getRequestURL();
				if ((value == null) || fails(value.toString(), regex)) {
					error(httpResponse, HttpServletResponse.SC_FORBIDDEN, "Forbidden");
					return;
				}
			}
			else if ("Scheme".equals(name) && fails(httpRequest.getScheme(), regex)) {
				error(httpResponse, HttpServletResponse.SC_FORBIDDEN, "Forbidden");
				return;
			}
			else if ("ServerName".equals(name) && fails(httpRequest.getServerName(), regex)) {
				error(httpResponse, HttpServletResponse.SC_FORBIDDEN, "Forbidden");
				return;
			}
			else if ("ServerPort".equals(name) && fails(String.valueOf(httpRequest.getServerPort()), regex)) {
				error(httpResponse, HttpServletResponse.SC_FORBIDDEN, "Forbidden");
				return;
			}
			else if ("ServletPath".equals(name) && fails(httpRequest.getServletPath(), regex)) {
				error(httpResponse, HttpServletResponse.SC_FORBIDDEN, "Forbidden");
				return;
			}
			else if ("UserPrincipal".equals(name)) {
				Principal value = httpRequest.getUserPrincipal();
				if ((value == null) || fails(value.getName(), regex)) {
					error(httpResponse, HttpServletResponse.SC_FORBIDDEN, "Forbidden");
					return;
				}
			}
			else {
				// Check if a header matches
				Enumeration<String> headers = httpRequest.getHeaders(name);
				if ((headers != null) && headers.hasMoreElements()) {
					while (headers.hasMoreElements()) {
						if (fails(headers.nextElement(), regex)) {
							error(httpResponse, HttpServletResponse.SC_FORBIDDEN, "Forbidden");
							return;
						}
					}
				}
				else {
					String[] parameters = httpRequest.getParameterValues(name);
					if ((parameters != null) && (parameters.length > 0)) {
						for (String parameter : parameters) {
							if (fails(parameter, regex)) {
								error(httpResponse, HttpServletResponse.SC_FORBIDDEN, "Forbidden");
								return;
							}
						}
					}
					else {
						error(httpResponse, HttpServletResponse.SC_FORBIDDEN, "Forbidden");
						return;
					}
				}
			}
		}
		
        chain.doFilter(request, response);
	}

	private static boolean fails(String value, String regex) {
		if (regex != null) {
			return ((value == null) || (! value.matches(regex)));
		}
		return false;
	}
}
