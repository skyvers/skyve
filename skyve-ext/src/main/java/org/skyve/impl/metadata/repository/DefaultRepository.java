package org.skyve.impl.metadata.repository;

import org.skyve.metadata.repository.DelegatingProvidedRepositoryChain;

public class DefaultRepository extends DelegatingProvidedRepositoryChain {
	public DefaultRepository() {
		super(new LocalSecureRepository());
	}
}
