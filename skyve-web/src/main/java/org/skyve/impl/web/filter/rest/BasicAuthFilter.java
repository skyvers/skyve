package org.skyve.impl.web.filter.rest;

import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.nio.charset.StandardCharsets;
import java.util.Base64;

import org.skyve.EXT;
import org.skyve.domain.Bean;
import org.skyve.domain.app.AppConstants;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.impl.metadata.user.UserImpl;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.WebUtil;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.repository.ProvidedRepository;
import org.skyve.persistence.Persistence;
import org.skyve.persistence.SQL;
import org.skyve.util.logging.Category;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.ServletRequest;
import jakarta.servlet.ServletResponse;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

public class BasicAuthFilter extends AbstractRestFilter {

    private static final Logger LOGGER = LoggerFactory.getLogger(BasicAuthFilter.class);
    private static final Logger COMMAND_LOGGER = Category.COMMAND.logger();

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
			error(null, httpResponse, HttpServletResponse.SC_UNAUTHORIZED, realm, "No credentials");
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
			error(null, httpResponse, HttpServletResponse.SC_UNAUTHORIZED, realm, "Unable to authenticate with the provided credentials");
			return;
		}

		if (UtilImpl.COMMAND_TRACE) COMMAND_LOGGER.info("Basic Auth for username: {} URI: {}", username, httpRequest.getRequestURI());

		AbstractPersistence persistence = null;
		try {
			try {
				persistence = AbstractPersistence.get();
				persistence.evictAllCached();
				persistence.begin();

				UserImpl user = ProvidedRepositoryFactory.get().retrieveUser(username);
				if (user != null) {
					WebUtil.setSessionId(user, httpRequest);
					persistence.setUser(user);
					validateUserCredentials(persistence, username, password);
					chain.doFilter(httpRequest, httpResponse);
				}
				else {
					error(persistence, httpResponse, HttpServletResponse.SC_FORBIDDEN, realm, "Unable to authenticate with the provided credentials");
				}
			}
			catch (InvocationTargetException e) {
				throw e.getTargetException();
			}
		}
		catch (Throwable t) {
			if (persistence != null) {
				persistence.rollback();
			}

			if (t instanceof SecurityException) {
				error(persistence, httpResponse, HttpServletResponse.SC_FORBIDDEN, realm, "Unable to authenticate with the provided credentials");
			}
			else if (t instanceof MetaDataException) {
				error(persistence, httpResponse, HttpServletResponse.SC_FORBIDDEN, realm, "Unable to authenticate with the provided credentials");
			}
			else {
				t.printStackTrace();
				LOGGER.error(t.getLocalizedMessage(), t);
				error(persistence, httpResponse, t.getLocalizedMessage());
			}
		}
		finally {
			if (persistence != null) {
				persistence.commit(true);
			}
		}
	}
	
	private static void validateUserCredentials(Persistence p, String username, String password)
	throws Exception {
		ProvidedRepository r = ProvidedRepositoryFactory.get();
		Module admin = r.getModule(null, AppConstants.ADMIN_MODULE_NAME);
		@SuppressWarnings("null")
		String ADM_SecurityUser = admin.getDocument(null, AppConstants.USER_DOCUMENT_NAME).getPersistent().getPersistentIdentifier();
		StringBuilder sql = new StringBuilder(128);
		sql.append("select ").append(AppConstants.PASSWORD_ATTRIBUTE_NAME).append(" from ").append(ADM_SecurityUser);
		sql.append(" where " ).append(AppConstants.USER_NAME_ATTRIBUTE_NAME).append(" = :").append(AppConstants.USER_NAME_ATTRIBUTE_NAME);

		final String[] customerAndUser = username.split("/");
		if (customerAndUser.length > 1) {
			sql.append(" and ").append(Bean.CUSTOMER_NAME).append(" = :").append(Bean.CUSTOMER_NAME);
		}
		SQL q = p.newSQL(sql.toString());
		if (customerAndUser.length == 1) {
			if (UtilImpl.CUSTOMER == null) { // multi-tenant
				throw new SecurityException("Invalid username/password");
			}
			q.putParameter(AppConstants.USER_NAME_ATTRIBUTE_NAME, customerAndUser[0], false);
		}
		else {
			if (UtilImpl.CUSTOMER == null) { // multi-tenant
				q.putParameter(Bean.CUSTOMER_NAME, customerAndUser[0], false);
			}
			else {
				throw new SecurityException("Invalid username/password");
			}
			q.putParameter(AppConstants.USER_NAME_ATTRIBUTE_NAME, customerAndUser[1], false);
		}

		String hashedPassword = q.scalarResult(String.class);
		if (hashedPassword == null) {
			throw new SecurityException("Invalid username/password");
		}
		if (! EXT.checkPassword(password, hashedPassword)) {
			throw new SecurityException("Invalid username/password");
		}
	}
}
