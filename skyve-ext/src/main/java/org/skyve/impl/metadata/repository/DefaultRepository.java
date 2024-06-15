package org.skyve.impl.metadata.repository;

import org.skyve.metadata.repository.DelegatingProvidedRepositoryChain;
import org.skyve.metadata.repository.ProvidedRepository;
import org.skyve.metadata.repository.SessionScopedDelegatingProvidedRepository;
import org.skyve.metadata.user.User;

import jakarta.annotation.Nonnull;

/**
 * The default repository for Skyve.
 * This is a delegating repository chain with a session-scoped delegating repository as the first delegate
 * and a LocalDataStoreRepository as the second delegate.
 * setSessionRepository() and removeSessionRepository() are used to control the first delegate.
 */
public class DefaultRepository extends DelegatingProvidedRepositoryChain {
	private SessionScopedDelegatingProvidedRepository sessionScopedDelegatingProvidedRepository = null;

	/**
	 * Default constructor
	 */
	public DefaultRepository() {
		super(new SessionScopedDelegatingProvidedRepository(), new LocalDataStoreRepository());
		sessionScopedDelegatingProvidedRepository = (SessionScopedDelegatingProvidedRepository) delegates.get(0);
	}
	
	/**
	 * Set the session-scoped repository for the given user's session.
	 */
	public void setSessionRepository(@Nonnull User user, @Nonnull ProvidedRepository delegate) {
		sessionScopedDelegatingProvidedRepository.setSessionDelegate(user, delegate);
	}
	
	/**
	 * Remove the session-scoped repository for the user session.
	 */
	public void removeSessionRepository(@Nonnull User user) {
		sessionScopedDelegatingProvidedRepository.removeSessionDelegate(user);
	}
}
