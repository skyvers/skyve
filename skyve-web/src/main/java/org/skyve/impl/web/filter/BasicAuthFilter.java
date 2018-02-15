package org.skyve.impl.web.filter;

import java.io.IOException;
import java.security.MessageDigest;
import java.util.Base64;

import javax.servlet.Filter;
import javax.servlet.FilterChain;
import javax.servlet.FilterConfig;
import javax.servlet.ServletException;
import javax.servlet.ServletOutputStream;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.ws.rs.core.MediaType;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.util.SQLMetaDataUtil;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.user.User;
import org.skyve.persistence.DocumentFilter;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.Persistence;
import org.skyve.util.Util;

public class BasicAuthFilter implements Filter {
	private String realm = "Protected";
	private String[] unsecuredURLPrefixes;

	@Override
	public void init(FilterConfig config) throws ServletException {
		String param = Util.processStringValue(config.getInitParameter("realm"));
		if (param != null) {
			realm = param;
		}
    	param = Util.processStringValue(config.getInitParameter("unsecured"));
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

	@Override
	public void doFilter(ServletRequest request, ServletResponse response, FilterChain chain)
			throws IOException, ServletException {
		HttpServletRequest httpRequest = (HttpServletRequest) request;
		HttpServletResponse httpResponse = (HttpServletResponse) response;

        String pathToTest = httpRequest.getServletPath();

        // Test if this URL is unsecured, and bug out if so
        if (unsecuredURLPrefixes != null) {
	        for (String unsecuredURLPrefix : unsecuredURLPrefixes) {
	        	if (pathToTest.startsWith(unsecuredURLPrefix)) {
	        		if (UtilImpl.COMMAND_TRACE) UtilImpl.LOGGER.info(String.format("%s is unsecured", pathToTest));
	        		chain.doFilter(request, response);
	        		return;
	        	}
	        }
        }
		
		// check the request is authenticated
		final String authorization = httpRequest.getHeader("Authorization");
		if ((authorization != null) && authorization.startsWith("Basic")) {
			// Authorization: Basic base64credentials
			final String base64Credentials = authorization.substring("Basic".length()).trim();

			String credentials = new String(Base64.getDecoder().decode(base64Credentials), Util.UTF8);
			// credentials = username:password
			final String[] values = credentials.split(":", 2);
			final String username = values[0];
			final String password = values[1];

			if (username != null && password != null) {
				if (UtilImpl.COMMAND_TRACE) UtilImpl.LOGGER.info(String.format("Basic Auth for username: {} URI: {}", username, httpRequest.getRequestURI()));

				AbstractPersistence persistence = AbstractPersistence.get();
				try {
					persistence.begin();

					User user = CORE.getRepository().retrieveUser(username);
					if (user != null) {
						persistence.setUser(user);
						validateUserCredentials(persistence, username, password);
						chain.doFilter(httpRequest, httpResponse);
					}
					else {
						error(httpResponse, HttpServletResponse.SC_FORBIDDEN, "Unable to authenticate with the provided credentials");
					}
				}
				catch (Exception e) {
					if (persistence != null) {
						persistence.rollback();
					}

					error(httpResponse, HttpServletResponse.SC_FORBIDDEN, "Unable to authenticate with the provided credentials");
				} finally {
					if (persistence != null) {
						persistence.commit(true);
					}
				}
			}
			else {
				error(httpResponse, HttpServletResponse.SC_UNAUTHORIZED, "Unable to authenticate with the provided credentials");
			}
		}
		else {
			error(httpResponse, HttpServletResponse.SC_UNAUTHORIZED, "No credentials");
		}
	}
	
	private static void validateUserCredentials(Persistence p, String username, String password)
	throws Exception {
		// convert the password into the user's password hash
		MessageDigest md = MessageDigest.getInstance(Util.getPasswordHashingAlgorithm());
		String hashedPassword = new String(Base64.getEncoder().encode(md.digest(password.getBytes())));

		DocumentQuery q = p.newDocumentQuery(SQLMetaDataUtil.ADMIN_MODULE_NAME, SQLMetaDataUtil.USER_DOCUMENT_NAME);
		DocumentFilter f = q.getFilter();
		f.addEquals(SQLMetaDataUtil.USER_NAME_PROPERTY_NAME, username);
		f.addEquals(SQLMetaDataUtil.PASSWORD_PROPERTY_NAME, hashedPassword);
		Bean user = q.beanResult();
		if (user == null) {
			throw new SecurityException("Invalid username/password");
		}
	}

	private void error(HttpServletResponse response, int status, String message) throws IOException {
		response.resetBuffer();
		response.setStatus(status);
		response.setHeader("WWW-Authenticate", "Basic realm=\"" + realm + "\"");
		response.setContentType(MediaType.APPLICATION_JSON );
		response.setCharacterEncoding(Util.UTF8);
		try (ServletOutputStream out = response.getOutputStream()) {
			out.print(String.format("{\"error\":\"%s\"}", message));
		}
		response.flushBuffer();
	}
}
