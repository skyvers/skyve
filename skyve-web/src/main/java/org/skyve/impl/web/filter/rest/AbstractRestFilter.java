package org.skyve.impl.web.filter.rest;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.Collections;

import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.WebErrorUtil;
import org.skyve.persistence.Persistence;
import org.skyve.util.JSON;
import org.skyve.util.Util;
import org.skyve.util.logging.Category;
import org.slf4j.Logger;
import org.skyve.util.logging.SkyveLoggerFactory;

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

/**
 * Provides shared configuration and error-response helpers for REST servlet filters.
 */
public abstract class AbstractRestFilter implements Filter {
	protected static final String REALM_INIT_PARAMETER = "realm";
	protected static final String UNSECURED_INIT_PARAMETER = "unsecured";
	
	private static final Logger LOGGER = SkyveLoggerFactory.getLogger(AbstractRestFilter.class);
	private static final Logger COMMAND_LOGGER = Category.COMMAND.logger();
	
	protected String realm = "Skyve";
	protected String[] unsecuredURLPrefixes;

	/**
	 * Initializes realm and unsecured URL-prefix settings from filter init parameters.
	 *
	 * @param config filter configuration
	 * @throws ServletException when initialization fails
	 */
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
	
	/**
	 * No-op lifecycle hook for filter destruction.
	 */
	@Override
	public void destroy() {
		// nothing to see here
	}

	/**
	 * Short-circuits processing for configured unsecured URL prefixes.
	 *
	 * @param request inbound servlet request
	 * @param response outbound servlet response
	 * @param chain downstream filter chain
	 * @return true when the request was handled as unsecured and downstream processing should stop
	 * @throws IOException when downstream filter invocation fails
	 * @throws ServletException when downstream filter invocation fails
	 */
	protected boolean doUnsecuredFilter(ServletRequest request, ServletResponse response, FilterChain chain)
	throws IOException, ServletException {
		HttpServletRequest httpRequest = (HttpServletRequest) request;

        String pathToTest = httpRequest.getServletPath();

        // Test if this URL is unsecured, and bug out if so
        if (unsecuredURLPrefixes != null) {
	        for (String unsecuredURLPrefix : unsecuredURLPrefixes) {
	        	if (pathToTest.startsWith(unsecuredURLPrefix)) {
	        		if (UtilImpl.COMMAND_TRACE) COMMAND_LOGGER.info("{} is unsecured", pathToTest);
	        		chain.doFilter(request, response);
	        		return true;
	        	}
	        }
        }
        
        return false;
	}
	
	/**
	 * Writes a generic internal-server-error REST response.
	 *
	 * @param response servlet response to write to
	 * @param message error message payload
	 */
	public static void error(HttpServletResponse response, String message) {
		error(response, HttpServletResponse.SC_INTERNAL_SERVER_ERROR, message);
	}

	/**
	 * Writes a generic internal-server-error REST response and rolls back persistence if supplied.
	 *
	 * @param persistence active persistence context to roll back, or null
	 * @param response servlet response to write to
	 * @param message error message payload
	 */
	public static void error(Persistence persistence, 
								HttpServletResponse response, 
								String message) {
		error(persistence, response, HttpServletResponse.SC_INTERNAL_SERVER_ERROR, message);
	}

	/**
	 * Writes a generic internal-server-error REST response using request-derived media type defaults.
	 *
	 * @param persistence active persistence context to roll back, or null
	 * @param request servlet request used to infer default response media type
	 * @param response servlet response to write to
	 * @param message error message payload
	 */
	protected static void error(Persistence persistence,
									HttpServletRequest request,
									HttpServletResponse response,
									String message) {
		error(persistence, request, response, HttpServletResponse.SC_INTERNAL_SERVER_ERROR, null, message);
	}

	/**
	 * Writes a REST error response with the supplied HTTP status.
	 *
	 * @param response servlet response to write to
	 * @param status HTTP status code
	 * @param message error message payload
	 */
	public static void error(HttpServletResponse response, 
								int status,
								String message) {
		error(null, response, status, null, message);
	}

	/**
	 * Writes a REST error response with the supplied HTTP status and optional persistence rollback.
	 *
	 * @param persistence active persistence context to roll back, or null
	 * @param response servlet response to write to
	 * @param status HTTP status code
	 * @param message error message payload
	 */
	public static void error(Persistence persistence, 
								HttpServletResponse response, 
								int status,
								String message) {
		error(persistence, response, status, null, message);
	}

	/**
	 * Writes a REST error response with explicit status, optional auth realm header, and payload format negotiation.
	 *
	 * @param persistence active persistence context to roll back, or null
	 * @param response servlet response to write to
	 * @param status HTTP status code
	 * @param realm optional BASIC auth realm value for {@code WWW-Authenticate}
	 * @param message error message payload
	 */
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
		response.setCharacterEncoding(StandardCharsets.UTF_8.name());
		try {
			try (ServletOutputStream out = response.getOutputStream()) {
				String contentType = response.getContentType();
				if ((contentType != null) && contentType.contains(MediaType.APPLICATION_XML)) {
					response.setContentType(MediaType.APPLICATION_XML);
					out.print("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<error>");
					out.print(WebErrorUtil.escapeXmlText(message));
					out.print("</error>");
				}
				else {
					response.setContentType(MediaType.APPLICATION_JSON);
					out.print(JSON.marshall(Collections.singletonMap("error", message)));
				}
			}
			response.flushBuffer();
		}
		catch (IOException e) {
			// can only log it and move on at this stage
			LOGGER.warn("Could not write REST error response", e);
		}
	}

	/**
	 * Writes a REST error response with explicit status and request-derived media type defaults.
	 *
	 * @param persistence active persistence context to roll back, or null
	 * @param request servlet request used to infer default response media type
	 * @param response servlet response to write to
	 * @param status HTTP status code
	 * @param realm optional BASIC auth realm value for {@code WWW-Authenticate}
	 * @param message error message payload
	 */
	protected static void error(Persistence persistence,
									HttpServletRequest request,
									HttpServletResponse response,
									int status,
									String realm,
									String message) {
		if (response.getContentType() == null) {
			response.setContentType(isXmlRequest(request) ? MediaType.APPLICATION_XML : MediaType.APPLICATION_JSON);
		}
		error(persistence, response, status, realm, message);
	}

	/**
	 * Determines whether the request path targets an XML REST endpoint.
	 *
	 * @param request servlet request
	 * @return true when the URI indicates XML format
	 */
	private static boolean isXmlRequest(HttpServletRequest request) {
		String requestURI = request.getRequestURI();
		return (requestURI != null) && requestURI.contains("/xml/");
	}
}
