package org.skyve.impl.web;

import org.skyve.impl.cache.StateUtil;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.repository.DefaultRepository;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.repository.ProvidedRepository;
import org.skyve.metadata.user.User;
import org.skyve.web.WebContext;

import jakarta.servlet.http.HttpSession;
import jakarta.servlet.http.HttpSessionEvent;
import jakarta.servlet.http.HttpSessionListener;

/**
 * Used to count the active sessions on a server.
 * The session count is on StateUtil as this is in skyve-ext, not skyve-web 
 * and so is available to all application tiers.
 * Also notifies customers through Observers that a user session has ended.
 * 
 * @author mike
 */
public class SkyveSessionListener implements HttpSessionListener {
	/**
	 * Increment the session count
	 */
	@Override
	public void sessionCreated(HttpSessionEvent se) {
		StateUtil.incrementSessionCount();
	}

	/**
	 * Decrement the session count, 
	 * and if a logged in user exists, notify the customer that they have logged out,
	 * and remove the session repository if it exists.
	 */
	@Override
	public void sessionDestroyed(HttpSessionEvent se) {
		StateUtil.decrementSessionCount();
		
		// Notify the customer of the logout
		HttpSession session = se.getSession();
		User user = (User) session.getAttribute(WebContext.USER_SESSION_ATTRIBUTE_NAME);
		if (user != null) { // a Skyve user was present
			try {
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
						UtilImpl.LOGGER.warning("Could not get the user customer " + customerName + " from the repository to call notifyLogout() on session destroyed.");
						e.printStackTrace();
					}
					if (customer != null) {
						customer.notifyLogout(user, session);
					}
				}
				else {
					UtilImpl.LOGGER.warning("Could not get the user customer as it is null so cannot call notifyLogout() on session destroyed.");
				}
			}
			// Remove the session repository if it exists
			finally {
				ProvidedRepository repository = ProvidedRepositoryFactory.get();
				if (repository instanceof DefaultRepository) {
					((DefaultRepository) repository).removeSessionRepository(user);
				}
			}
		}
	}
}
