package org.skyve.impl.web.filter.rest;

import java.io.IOException;
import java.util.logging.Level;

import org.skyve.impl.util.UtilImpl;
import org.skyve.persistence.Persistence;
import org.skyve.util.Util;

import jakarta.servlet.Filter;
import jakarta.servlet.FilterChain;
import jakarta.servlet.FilterConfig;
import jakarta.servlet.ServletException;
import jakarta.servlet.ServletOutputStream;
import jakarta.servlet.ServletRequest;
import jakarta.servlet.ServletResponse;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.ws.rs.core.MediaType;

public abstract class AbstractRestFilter implements Filter {
	protected static final String REALM_INIT_PARAMETER = "realm";
	protected static final String UNSECURED_INIT_PARAMETER = "unsecured";
	
	protected String realm = "Skyve";
	protected String[] unsecuredURLPrefixes;

	@Override
	public void init(FilterConfig config) throws ServletException {
		String param = Util.processStringValue(config.getInitParameter(REALM_INIT_PARAMETER));
		if (param != null) {
			realm = param;
		}
		param = Util.processStringValue(config.getInitParameter(UNSECURED_INIT_PARAMETER));
		if (param != null) {
			unsecuredURLPrefixes = param.split("\n");
			for (int i = 0, l = unsecuredURLPrefixes.length; i < l; i++) {
				unsecuredURLPrefixes[i] = Util.processStringValue(unsecuredURLPrefixes[i]);
			}
		}
	}
	
	@Override
	public void destroy() {
		// nothing to see here
	}

	protected boolean doUnsecuredFilter(ServletRequest request, ServletResponse response, FilterChain chain)
	throws IOException, ServletException {
		HttpServletRequest httpRequest = (HttpServletRequest) request;

        String pathToTest = httpRequest.getServletPath();

        // Test if this URL is unsecured, and bug out if so
        if (unsecuredURLPrefixes != null) {
	        for (String unsecuredURLPrefix : unsecuredURLPrefixes) {
	        	if (pathToTest.startsWith(unsecuredURLPrefix)) {
	        		if (UtilImpl.COMMAND_TRACE) UtilImpl.LOGGER.info(String.format("%s is unsecured", pathToTest));
	        		chain.doFilter(request, response);
	        		return true;
	        	}
	        }
        }
        
        return false;
	}
	
	public static void error(HttpServletResponse response, String message) {
			error(response, HttpServletResponse.SC_INTERNAL_SERVER_ERROR, message);
	}

	public static void error(Persistence persistence, 
								HttpServletResponse response, 
								String message) {
		error(persistence, response, HttpServletResponse.SC_INTERNAL_SERVER_ERROR, message);
	}

	public static void error(HttpServletResponse response, 
								int status,
								String message) {
		error(null, response, status, null, message);
	}

	public static void error(Persistence persistence, 
								HttpServletResponse response, 
								int status,
								String message) {
		error(persistence, response, status, null, message);
	}

	public static void error(Persistence persistence, 
								HttpServletResponse response, 
								int status,
								String realm,
								String message) {
    	if (persistence != null) {
    		persistence.rollback();
    	}

		response.resetBuffer();
		response.setStatus(status);
		if (realm != null) {
			response.setHeader("WWW-Authenticate", "Basic realm=\"" + realm + "\"");
		}
		response.setCharacterEncoding(Util.UTF8);
		try {
			try (ServletOutputStream out = response.getOutputStream()) {
				String contentType = response.getContentType();
				if ((contentType != null) && contentType.contains(MediaType.APPLICATION_JSON)) {
					out.print(String.format("{\"error\":\"%s\"}", message));
				}
				else {
					out.print(String.format("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<error>%s</error>", message));
				}
			}
			response.flushBuffer();
		}
		catch (IOException e) {
			// can only log it and move on at this stage
			UtilImpl.LOGGER.log(Level.WARNING, e.getLocalizedMessage(), e);
		}
	}
}
