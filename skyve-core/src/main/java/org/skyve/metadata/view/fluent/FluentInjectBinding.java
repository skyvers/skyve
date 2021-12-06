package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.InjectBinding;

public class FluentInjectBinding extends FluentBoundWidget<FluentInjectBinding> {
	private InjectBinding binding = null;

	public FluentInjectBinding() {
		binding = new InjectBinding();
	}

	public FluentInjectBinding(InjectBinding binding) {
		this.binding = binding;
	}

	public FluentInjectBinding from(@SuppressWarnings("hiding") InjectBinding binding) {
		readOnly(binding.getReadOnly());
		super.from(binding);
		return this;
	}

	public FluentInjectBinding readOnly(Boolean readOnly) {
		binding.setReadOnly(readOnly);
		return this;
	}

	@Override
	public InjectBinding get() {
		return binding;
	}

}
