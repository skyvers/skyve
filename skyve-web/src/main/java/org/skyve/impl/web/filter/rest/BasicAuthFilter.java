package org.skyve.impl.web.filter.rest;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.Base64;

import org.skyve.EXT;
import org.skyve.domain.Bean;
import org.skyve.domain.app.AppConstants;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.impl.metadata.user.UserImpl;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.web.WebErrorUtil;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.WebUtil;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.repository.ProvidedRepository;
import org.skyve.persistence.Persistence;
import org.skyve.persistence.SQL;
import org.skyve.util.logging.Category;
import org.slf4j.Logger;
import org.skyve.util.logging.SkyveLoggerFactory;

import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.ServletRequest;
import jakarta.servlet.ServletResponse;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

/**
 * Authenticates REST requests using HTTP Basic credentials and establishes the Skyve persistence user context.
 *
 * <p>Side effects: starts and commits/rolls back a persistence transaction, updates request session identity
 * metadata through {@link WebUtil#setSessionId(org.skyve.metadata.user.User, jakarta.servlet.http.HttpServletRequest)},
 * and writes REST error responses on authentication or processing failure.
 */
public class BasicAuthFilter extends AbstractRestFilter {
    private static final Logger LOGGER = SkyveLoggerFactory.getLogger(BasicAuthFilter.class);
    private static final Logger COMMAND_LOGGER = Category.COMMAND.logger();

    private static final String AUTH_FAILURE_MESSAGE = "Unable to authenticate with the provided credentials";
	private static final String INVALID_CREDENTIALS = "Invalid username/password";

	@Override
	public void doFilter(ServletRequest request, ServletResponse response, FilterChain chain)
	throws IOException, ServletException {
		if (super.doUnsecuredFilter(request, response, chain)) {
			return;
		}
		
		HttpServletRequest httpRequest = (HttpServletRequest) request;
		HttpServletResponse httpResponse = (HttpServletResponse) response;

		// check the request is authenticated
		final String authorization = httpRequest.getHeader("Authorization");
		if ((authorization == null) || (! authorization.startsWith("Basic"))) {
			error(null, httpRequest, httpResponse, HttpServletResponse.SC_UNAUTHORIZED, realm, "No credentials");
			return;
		}
		
		// Authorization: Basic base64credentials
		final String base64Credentials = authorization.substring("Basic".length()).trim();
		String credentials = new String(Base64.getMimeDecoder().decode(base64Credentials), StandardCharsets.UTF_8);

		// credentials = username:password or customer/username:password
		final String[] values = credentials.split(":", 2);
		final String username = UtilImpl.processStringValue(values[0]);
		final String password = UtilImpl.processStringValue(values[1]);

		if ((username == null) || (password == null)) {
			error(null, httpRequest, httpResponse, HttpServletResponse.SC_UNAUTHORIZED, realm, AUTH_FAILURE_MESSAGE);
			return;
		}

		if (UtilImpl.COMMAND_TRACE) COMMAND_LOGGER.info("Basic Auth for username: {} URI: {}", username, httpRequest.getRequestURI());

		processAuthenticatedRequest(httpRequest, httpResponse, chain, username, password);
	}

	private void processAuthenticatedRequest(HttpServletRequest httpRequest,
										HttpServletResponse httpResponse,
										FilterChain chain,
										String username,
										String password) {
		AbstractPersistence persistence = null;
		try {
			persistence = AbstractPersistence.get();
			persistence.evictAllCached();
			persistence.begin();

			UserImpl user = ProvidedRepositoryFactory.get().retrieveUser(username);
			if (user == null) {
				error(persistence, httpRequest, httpResponse, HttpServletResponse.SC_FORBIDDEN, realm, AUTH_FAILURE_MESSAGE);
				return;
			}

			WebUtil.setSessionId(user, httpRequest);
			persistence.setUser(user);
			validateUserCredentials(persistence, username, password);
			chain.doFilter(httpRequest, httpResponse);
		}
		catch (Exception e) {
			handleAuthFailure(persistence, httpRequest, httpResponse, e);
		}
		finally {
			if (persistence != null) {
				persistence.commit(true);
			}
		}
	}

	private void handleAuthFailure(AbstractPersistence persistence,
								HttpServletRequest httpRequest,
								HttpServletResponse httpResponse,
								Throwable throwable) {
		if (throwable instanceof Error error) {
			throw error;
		}

		if (persistence != null) {
			persistence.rollback();
		}

		if ((throwable instanceof SecurityException) || (throwable instanceof MetaDataException)) {
			error(persistence, httpRequest, httpResponse, HttpServletResponse.SC_FORBIDDEN, realm, AUTH_FAILURE_MESSAGE);
			return;
		}

		String reference = WebErrorUtil.logUnexpectedAndGetReference(LOGGER, "REST basic authentication failed", throwable);
		error(persistence, httpRequest, httpResponse, WebErrorUtil.genericMessage(reference));
	}
	
	private static void validateUserCredentials(Persistence p, String username, String password) {
		ProvidedRepository r = ProvidedRepositoryFactory.get();
		Module admin = r.getModule(null, AppConstants.ADMIN_MODULE_NAME);
		String admSecurityUser = admin.getDocument(null, AppConstants.USER_DOCUMENT_NAME).getPersistent().getPersistentIdentifier();
		StringBuilder sql = new StringBuilder(128);
		sql.append("select ").append(AppConstants.PASSWORD_ATTRIBUTE_NAME).append(" from ").append(admSecurityUser);
		sql.append(" where " ).append(AppConstants.USER_NAME_ATTRIBUTE_NAME).append(" = :").append(AppConstants.USER_NAME_ATTRIBUTE_NAME);

		final String[] customerAndUser = username.split("/");
		if (customerAndUser.length > 1) {
			sql.append(" and ").append(Bean.CUSTOMER_NAME).append(" = :").append(Bean.CUSTOMER_NAME);
		}
		SQL q = p.newSQL(sql.toString());
		if (customerAndUser.length == 1) {
			if (UtilImpl.CUSTOMER == null) { // multi-tenant
				throw new SecurityException(INVALID_CREDENTIALS);
			}
			q.putParameter(AppConstants.USER_NAME_ATTRIBUTE_NAME, customerAndUser[0], false);
		}
		else {
			if (UtilImpl.CUSTOMER == null) { // multi-tenant
				q.putParameter(Bean.CUSTOMER_NAME, customerAndUser[0], false);
			}
			else {
				throw new SecurityException(INVALID_CREDENTIALS);
			}
			q.putParameter(AppConstants.USER_NAME_ATTRIBUTE_NAME, customerAndUser[1], false);
		}

		String hashedPassword = q.scalarResult(String.class);
		if (hashedPassword == null) {
			throw new SecurityException(INVALID_CREDENTIALS);
		}
		if (! EXT.checkPassword(password, hashedPassword)) {
			throw new SecurityException(INVALID_CREDENTIALS);
		}
	}
}
