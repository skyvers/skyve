package org.skyve.impl.web.filter.rest;

import java.io.IOException;

import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.impl.metadata.user.UserImpl;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.web.WebErrorUtil;
import org.skyve.impl.web.WebUtil;
import org.skyve.metadata.MetaDataException;
import org.slf4j.Logger;
import org.skyve.util.logging.SkyveLoggerFactory;

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
 *
 * <p>Side effects: starts a persistence transaction, sets the thread-local persistence user,
 * and writes REST error responses on authentication/metadata/unexpected failures.
 *
 * <p>Threading: this filter stores configuration in {@code persistenceUser} during
 * initialization and then treats it as read-only for request processing.
 */
public class RestUserPersistenceFilter extends AbstractRestFilter {
    private static final Logger LOGGER = SkyveLoggerFactory.getLogger(RestUserPersistenceFilter.class);
    private static final String AUTHENTICATE_ERROR_MESSAGE = "Unable to authenticate with the provided credentials";
    
	private String persistenceUser;

	/**
	 * Reads the configured persistence user and initializes shared REST filter settings.
	 *
	 * @param config filter configuration containing the PersistenceUser parameter
	 * @throws ServletException when initialization fails
	 */
	@Override
	public void init(FilterConfig config) throws ServletException {
		persistenceUser = config.getInitParameter("PersistenceUser");

		super.init(config);
	}

	/**
	 * Processes a REST request under the configured persistence user.
	 *
	 * <p>When the configured user cannot be resolved, this method returns HTTP 403.
	 * For non-security failures, it logs an unexpected error reference and returns
	 * a generic REST error payload.
	 *
	 * @param request the incoming servlet request
	 * @param response the outgoing servlet response
	 * @param chain the downstream filter chain
	 * @throws IOException if writing the response fails
	 * @throws ServletException if downstream filter processing fails
	 */
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
				LOGGER.debug("Setting persistence user to: {}", persistenceUser);
				WebUtil.setSessionId(user, httpRequest);
				persistence.setUser(user);
				chain.doFilter(httpRequest, httpResponse);
			}
			else {
				error(persistence, httpRequest, httpResponse, HttpServletResponse.SC_FORBIDDEN, realm, AUTHENTICATE_ERROR_MESSAGE);
			}
		}
		catch (Throwable t) {
			if (persistence != null) {
				persistence.rollback();
			}
			
			if (t instanceof SecurityException) {
				error(persistence, httpRequest, httpResponse, HttpServletResponse.SC_FORBIDDEN, realm, AUTHENTICATE_ERROR_MESSAGE);
			}
			else if (t instanceof MetaDataException) {
				error(persistence, httpRequest, httpResponse, HttpServletResponse.SC_FORBIDDEN, realm, AUTHENTICATE_ERROR_MESSAGE);
			}
			else {
				String reference = WebErrorUtil.logUnexpectedAndGetReference(LOGGER, "REST user persistence filter failed", t);
				error(persistence, httpRequest, httpResponse, WebErrorUtil.genericMessage(reference));
			}
		}
		finally {
			if (persistence != null) {
				persistence.commit(true);
			}
		}
	}
}