package org.skyve.impl.web.filter.rest;

import java.io.IOException;
import java.time.Duration;
import java.time.Instant;
import java.util.logging.Level;

import javax.servlet.FilterChain;
import javax.servlet.FilterConfig;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.skyve.CORE;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.user.User;
import org.skyve.util.Util;

/**
 * <p>
 * Injects a user into AbstractPersistence so that incoming REST requests
 * are able to query and interact with the datastore.
 * </p>
 * <p>
 * To enable, this filter requires web.xml to be updated to specify this
 * filter, and to customise two parameters:
 * </p>
 * <dl>
 * <dt>&lt;param-value&gt;{restUsername}&lt;/param-value&gt;
 * <dd>The username of a user created in the datastore which has the appropriate
 * permissions to read and/or write to the Documents this REST endpoint accesses.
 * <dt>&lt;url-pattern&gt;{restUrlPattern}&lt;/url-pattern&gt;
 * <dd>The url pattern this filter will intercept. E.g. <code>/api/*</code>
 * </dl>
 * <p>
 * The full filter definition would then be inserted into your web.xml as follows:
 * </p>
 * 
 * <pre>
 * &lt;filter&gt;
 * 	&lt;filter-name&gt;RestUserPersistenceFilter&lt;/filter-name&gt;
 *	&lt;filter-class&gt;org.skyve.impl.web.filter.rest.RestUserPersistenceFilter&lt;/filter-class&gt;
 *	&lt;init-param&gt;
 *		&lt;param-name&gt;PersistenceUser&lt;/param-name&gt;
 *		&lt;param-value&gt;{restUsername}&lt;/param-value&gt;
 *	&lt;/init-param&gt;
 * &lt;/filter&gt;
 * &lt;filter-mapping&gt;
 * 	&lt;filter-name&gt;RestUserPersistenceFilter&lt;/filter-name&gt;
 *	&lt;url-pattern&gt;{restUrlPattern}&lt;/url-pattern&gt;
 * &lt;/filter-mapping&gt;
 * </pre>
 */
public class RestUserPersistenceFilter extends AbstractRestFilter {

	private String persistenceUser;

	@Override
	public void init(FilterConfig config) throws ServletException {
		persistenceUser = config.getInitParameter("PersistenceUser");

		super.init(config);
	}

	@Override
	public void doFilter(ServletRequest request, ServletResponse response, FilterChain chain)
			throws IOException, ServletException {
		Instant start = Instant.now();
		Util.LOGGER.fine("RestUserPersistenceFilter intercepted request");

		HttpServletRequest httpRequest = (HttpServletRequest) request;
		HttpServletResponse httpResponse = (HttpServletResponse) response;

		AbstractPersistence persistence = null;
		try {
			persistence = AbstractPersistence.get();
			persistence.evictAllCached();
			persistence.begin();

			User user = CORE.getRepository().retrieveUser(persistenceUser);
			if (user != null) {
				Util.LOGGER.fine("Setting persistence user to: " + persistenceUser);
				persistence.setUser(user);
				Util.LOGGER.fine(
						String.format("RestUserPersistenceFilter persistence injection took: %S",
								Duration.between(start, Instant.now())));
				chain.doFilter(httpRequest, httpResponse);
			} else {
				error(persistence, httpResponse, HttpServletResponse.SC_FORBIDDEN, realm,
						"Unable to authenticate with the provided credentials");
			}
		} catch (@SuppressWarnings("unused") SecurityException e) {
			error(persistence, httpResponse, HttpServletResponse.SC_FORBIDDEN, realm,
					"Unable to authenticate with the provided credentials");
		} catch (@SuppressWarnings("unused") MetaDataException e) {
			error(persistence, httpResponse, HttpServletResponse.SC_FORBIDDEN, realm,
					"Unable to authenticate with the provided credentials");
		} catch (Throwable t) {
			t.printStackTrace();
			UtilImpl.LOGGER.log(Level.SEVERE, t.getLocalizedMessage(), t);
			error(persistence, httpResponse, t.getLocalizedMessage());
		} finally {
			if (persistence != null) {
				persistence.commit(true);
			}
		}
		Util.LOGGER.fine(
				String.format("RestUserPersistenceFilter total request handling took: %S", Duration.between(start, Instant.now())));
	}
}
