package org.skyve.impl.metadata.repository;

import org.skyve.metadata.repository.DelegatingProvidedRepositoryChain;
import org.skyve.metadata.repository.ProvidedRepository;
import org.skyve.metadata.repository.SessionScopedDelegatingProvidedRepository;
import org.skyve.metadata.user.User;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

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
	 * Get the session-scoped repository for the current user's session.
	 * @return 
	 */
	public @Nullable ProvidedRepository getSessionRepository() {
		return sessionScopedDelegatingProvidedRepository.getSessionDelegate();
	}

	/**
	 * Get the session-scoped repository for the given user's session.
	 * @return 
	 */
	public @Nullable ProvidedRepository getSessionRepository(@Nonnull User user) {
		return sessionScopedDelegatingProvidedRepository.getSessionDelegate(user);
	}

	/**
	 * Set the session-scoped repository for the current user's session.
	 */
	public void setSessionRepository(@Nonnull ProvidedRepository delegate) {
		sessionScopedDelegatingProvidedRepository.setSessionDelegate(delegate);
	}

	/**
	 * Set the session-scoped repository for the given user's session.
	 */
	public void setSessionRepository(@Nonnull User user, @Nonnull ProvidedRepository delegate) {
		sessionScopedDelegatingProvidedRepository.setSessionDelegate(user, delegate);
	}
	
	/**
	 * Remove the session-scoped repository for the current user's session.
	 */
	public void removeSessionRepository() {
		sessionScopedDelegatingProvidedRepository.removeSessionDelegate();
	}

	/**
	 * Remove the session-scoped repository for the given user's session.
	 */
	public void removeSessionRepository(@Nonnull User user) {
		sessionScopedDelegatingProvidedRepository.removeSessionDelegate(user);
	}
}
