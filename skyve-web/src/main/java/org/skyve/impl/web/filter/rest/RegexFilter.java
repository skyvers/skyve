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

/**
 * Filters inbound or outbound requests before downstream web processing.
 */
public class RegexFilter extends AbstractRestFilter {
	private static final String FORBIDDEN = "Forbidden";
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

			if (!passes(httpRequest, name, regex)) {
				error(httpResponse, HttpServletResponse.SC_FORBIDDEN, FORBIDDEN);
				return;
			}
		}
		
        chain.doFilter(request, response);
	}

	private static boolean passes(HttpServletRequest request, String name, String regex) {
		String value = requestValue(request, name);
		if ((value != null) || isRequestPropertyName(name)) {
			return !fails(value, regex);
		}

		Enumeration<String> headers = request.getHeaders(name);
		if ((headers != null) && headers.hasMoreElements()) {
			while (headers.hasMoreElements()) {
				if (fails(headers.nextElement(), regex)) {
					return false;
				}
			}
			return true;
		}

		return parametersPass(request.getParameterValues(name), regex);
	}

	private static boolean parametersPass(String[] parameters, String regex) {
		if ((parameters != null) && (parameters.length > 0)) {
			for (String parameter : parameters) {
				if (fails(parameter, regex)) {
					return false;
				}
			}
			return true;
		}
		return false;
	}

	private static boolean isRequestPropertyName(String name) {
		// NB Don't test requestedSessionId as it is doctorable by the user
		return switch (name) {
			case "ContextPath", "LocalAddr", "LocalName", "LocalPort", "Method", "PathInfo", "PathTranslated", "Protocol",
					"QueryString", "RemoteAddr", "RemoteHost", "RemotePort", "RemoteUser", "RequestURI", "RequestURL",
					"Scheme", "ServerName", "ServerPort", "ServletPath", "UserPrincipal" -> true;
			default -> false;
		};
	}

	private static String requestValue(HttpServletRequest request, String name) {
		return switch (name) {
			case "ContextPath" -> request.getContextPath();
			case "LocalAddr" -> request.getLocalAddr();
			case "LocalName" -> request.getLocalName();
			case "LocalPort" -> String.valueOf(request.getLocalPort());
			case "Method" -> request.getMethod();
			case "PathInfo" -> request.getPathInfo();
			case "PathTranslated" -> request.getPathTranslated();
			case "Protocol" -> request.getProtocol();
			case "QueryString" -> request.getQueryString();
			case "RemoteAddr" -> request.getRemoteAddr();
			case "RemoteHost" -> request.getRemoteHost();
			case "RemotePort" -> String.valueOf(request.getRemotePort());
			case "RemoteUser" -> request.getRemoteUser();
			case "RequestURI" -> request.getRequestURI();
			case "RequestURL" -> (request.getRequestURL() == null) ? null : new StringBuilder(request.getRequestURL()).toString();
			case "Scheme" -> request.getScheme();
			case "ServerName" -> request.getServerName();
			case "ServerPort" -> String.valueOf(request.getServerPort());
			case "ServletPath" -> request.getServletPath();
			case "UserPrincipal" -> {
				Principal principal = request.getUserPrincipal();
				yield (principal == null) ? null : principal.getName();
			}
			default -> null;
		};
	}

	private static boolean fails(String value, String regex) {
		if (regex != null) {
			return ((value == null) || (! value.matches(regex)));
		}
		return false;
	}
}
