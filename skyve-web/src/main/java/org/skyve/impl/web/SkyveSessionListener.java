package org.skyve.impl.web;

import org.skyve.impl.cache.StateUtil;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.repository.DefaultRepository;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.metadata.repository.ProvidedRepository;
import org.skyve.metadata.user.User;
import org.skyve.web.WebContext;
import org.slf4j.Logger;
import org.skyve.util.logging.SkyveLoggerFactory;

import jakarta.annotation.Nonnull;
import jakarta.servlet.ServletContext;
import jakarta.servlet.http.HttpSession;
import jakarta.servlet.http.HttpSessionEvent;
import jakarta.servlet.http.HttpSessionIdListener;
import jakarta.servlet.http.HttpSessionListener;

/**
 * Used to count the active sessions on a server.
 * The session count is on StateUtil as this is in skyve-ext, not skyve-web 
 * and so is available to all application tiers.
 * Also notifies customers through Observers that a user session has ended,
 * and drops a session's CSRF token when its id changes.
 * 
 * @author mike
 */
public class SkyveSessionListener implements HttpSessionListener, HttpSessionIdListener {
    private static final Logger LOGGER = SkyveLoggerFactory.getLogger(SkyveSessionListener.class);

	/**
	 * Increment the session count
	 *
	 * @param se the session event for the created session
	 */
	@Override
	public void sessionCreated(@SuppressWarnings("null") @Nonnull HttpSessionEvent se) {
		StateUtil.incrementSessionCount();
	}

	/**
	 * Drops the session's CSRF token, so that a new one is issued.
	 * The id changes at sign in (Spring Security's session fixation protection), and a token seen before
	 * sign in must not work after it.
	 *
	 * @param se the session event for the session whose id changed
	 * @param oldSessionId the session's id before the change
	 */
	@Override
	public void sessionIdChanged(@SuppressWarnings("null") @Nonnull HttpSessionEvent se,
									@SuppressWarnings("null") @Nonnull String oldSessionId) {
		StateUtil.removeSessionToken(se.getSession());
	}

	/**
	 * Decrement the session count, 
	 * and if a logged in user exists, notify the customer that they have logged out,
	 * and remove the session repository if it exists.
	 *
	 * @param se the session event for the destroyed session
	 */
	@Override
	@SuppressWarnings("java:S3776") // Complexity OK
	public void sessionDestroyed(@SuppressWarnings("null") @Nonnull HttpSessionEvent se) {
		StateUtil.decrementSessionCount();
		
		HttpSession session = se.getSession();
		User user = (User) session.getAttribute(WebContext.USER_SESSION_ATTRIBUTE_NAME);
		if (user != null) { // a Skyve user was present
			// Remove the session from the session cache
			StateUtil.removeSession(user.getId(), session);
			
			try {
				ServletContext servletContext = session.getServletContext();
				boolean shuttingDown = (servletContext != null) &&
										Boolean.TRUE.equals(servletContext.getAttribute(SkyveContextListener.SHUTTING_DOWN_ATTRIBUTE_NAME));
				if (! shuttingDown) {
					// Notify the customer of the logout
					String customerName = user.getCustomerName();
					if (customerName != null) {
						// The user.customer lookup is not a reference, its a lookup of the name through the repository.
						// If the repository has not been initialised its possible that an error will emanate from the User.getCustomer() call below.
						// Note the current thread may not have a Persistence instance associated with it.
						CustomerImpl customer = null;
						try {
							customer = (CustomerImpl) user.getCustomer();
						}
						catch (Exception e) {
							LOGGER.warn("Could not get the user customer {} from the repository to call notifyLogout() on session destroyed.", customerName, e);
						}
						if (customer != null) {
							customer.notifyLogout(user, session);
						}
					}
					else {
						LOGGER.warn("Could not get the user customer as it is null so cannot call notifyLogout() on session destroyed.");
					}
				}
			}
			// Remove the session repository if it exists
			finally {
				// sessions can be destroyed during shutdown, after the repository has gone
				ProvidedRepository repository = ProvidedRepositoryFactory.get();
				if (repository instanceof DefaultRepository defaultRepository) {
					defaultRepository.removeSessionRepository(user);
				}
			}
		}
	}
}
