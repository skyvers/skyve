package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.Inject;

public class FluentInject extends FluentWidget {
	private Inject inject = null;
	
	public FluentInject() {
		inject = new Inject();
	}

	public FluentInject(Inject inject) {
		this.inject = inject;
	}

	public FluentInject from(@SuppressWarnings("hiding") Inject inject) {
		return this;
	}

	@Override
	public Inject get() {
		return inject;
	}
}
