package org.skyve.impl.web.filter.rest;

import java.io.IOException;

import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.impl.metadata.user.UserImpl;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.web.WebUtil;
import org.skyve.metadata.MetaDataException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import jakarta.servlet.FilterChain;
import jakarta.servlet.FilterConfig;
import jakarta.servlet.ServletException;
import jakarta.servlet.ServletRequest;
import jakarta.servlet.ServletResponse;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

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

    private static final Logger LOGGER = LoggerFactory.getLogger(RestUserPersistenceFilter.class);

	private String persistenceUser;

	@Override
	public void init(FilterConfig config) throws ServletException {
		persistenceUser = config.getInitParameter("PersistenceUser");

		super.init(config);
	}

	@Override
	public void doFilter(ServletRequest request, ServletResponse response, FilterChain chain)
	throws IOException, ServletException {
		LOGGER.debug("RestUserPersistenceFilter intercepted request");

		HttpServletRequest httpRequest = (HttpServletRequest) request;
		HttpServletResponse httpResponse = (HttpServletResponse) response;

		AbstractPersistence persistence = null;
		try {
			persistence = AbstractPersistence.get();
			persistence.evictAllCached();
			persistence.begin();

			UserImpl user = ProvidedRepositoryFactory.get().retrieveUser(persistenceUser);
			if (user != null) {
				LOGGER.debug("Setting persistence user to: " + persistenceUser);
				WebUtil.setSessionId(user, httpRequest);
				persistence.setUser(user);
				chain.doFilter(httpRequest, httpResponse);
			}
			else {
				error(persistence, httpResponse, HttpServletResponse.SC_FORBIDDEN, realm, "Unable to authenticate with the provided credentials");
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
}
