package org.skyve.impl.web.filter.rest;

import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.util.Base64;
import java.util.logging.Level;

import javax.servlet.FilterChain;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.domain.Bean;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.util.SQLMetaDataUtil;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.user.User;
import org.skyve.persistence.DocumentFilter;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.Persistence;
import org.skyve.util.Util;

public class BasicAuthFilter extends AbstractRestFilter {
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
		String credentials = new String(Base64.getMimeDecoder().decode(base64Credentials), Util.UTF8);

		// credentials = username:password or customer/username:password
		final String[] values = credentials.split(":", 2);
		final String username = UtilImpl.processStringValue(values[0]);
		final String password = UtilImpl.processStringValue(values[1]);

		if ((username == null) || (password == null)) {
			error(null, httpResponse, HttpServletResponse.SC_UNAUTHORIZED, realm, "Unable to authenticate with the provided credentials");
			return;
		}
		
		if (UtilImpl.COMMAND_TRACE) UtilImpl.LOGGER.info(String.format("Basic Auth for username: %s URI: %s", username, httpRequest.getRequestURI()));

		AbstractPersistence persistence = null;
		try {
			try {
				persistence = AbstractPersistence.get();
				persistence.evictAllCached();
				persistence.begin();

				User user = CORE.getRepository().retrieveUser(username);
				if (user != null) {
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
		catch (@SuppressWarnings("unused") SecurityException e) {
			error(persistence, httpResponse, HttpServletResponse.SC_FORBIDDEN, realm, "Unable to authenticate with the provided credentials");
		}
		catch (@SuppressWarnings("unused") MetaDataException e) {
			error(persistence, httpResponse, HttpServletResponse.SC_FORBIDDEN, realm, "Unable to authenticate with the provided credentials");
		}
		catch (Throwable t) {
			t.printStackTrace();
			UtilImpl.LOGGER.log(Level.SEVERE, t.getLocalizedMessage(), t);
			error(persistence, httpResponse, t.getLocalizedMessage());
		}
		finally {
			if (persistence != null) {
				persistence.commit(true);
			}
		}
	}
	
	private static void validateUserCredentials(Persistence p, String username, String password)
	throws Exception {
		DocumentQuery q = p.newDocumentQuery(SQLMetaDataUtil.ADMIN_MODULE_NAME, SQLMetaDataUtil.USER_DOCUMENT_NAME);
		DocumentFilter f = q.getFilter();
		final String[] customerAndUser = username.split("/");
		if (customerAndUser.length == 1) {
			if (UtilImpl.CUSTOMER == null) {
				throw new SecurityException("Invalid username/password");
			}
			f.addEquals(Bean.CUSTOMER_NAME, UtilImpl.CUSTOMER);
			f.addEquals(SQLMetaDataUtil.USER_NAME_PROPERTY_NAME, username);
		}
		else {
			f.addEquals(Bean.CUSTOMER_NAME, customerAndUser[0]);
			f.addEquals(SQLMetaDataUtil.USER_NAME_PROPERTY_NAME, customerAndUser[1]);
		}
		Bean user = q.beanResult();
		if (user == null) {
			throw new SecurityException("Invalid username/password");
		}
		if (! EXT.checkPassword(password, (String) BindUtil.get(user, SQLMetaDataUtil.PASSWORD_PROPERTY_NAME))) {
			throw new SecurityException("Invalid username/password");
		}
	}
}
