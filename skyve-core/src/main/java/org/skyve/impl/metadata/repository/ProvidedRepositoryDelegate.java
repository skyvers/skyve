package org.skyve.impl.metadata.repository;

import org.skyve.metadata.repository.ProvidedRepository;

public abstract class ProvidedRepositoryDelegate extends ProvidedRepositoryFactory {
	private transient ProvidedRepository delegator;
	
	@Override
	public ProvidedRepository getDelegator() {
		if (delegator == null) {
			return this;
		}
		return delegator.getDelegator();
	}

	@Override
	public void setDelegator(ProvidedRepository delegator) {
		this.delegator = delegator;
	}
}
