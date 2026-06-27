package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.InjectBinding;

/**
 * Builds {@link InjectBinding} metadata.
 */
public class FluentInjectBinding extends FluentBoundWidget<FluentInjectBinding> {
	private InjectBinding binding = null;

	/**
	 * Creates a builder backed by a new {@link InjectBinding}.
	 */
	public FluentInjectBinding() {
		binding = new InjectBinding();
	}

	/**
	 * Creates a builder backed by the supplied {@link InjectBinding}.
	 *
	 * @param binding
	 *            the metadata instance to mutate
	 */
	public FluentInjectBinding(InjectBinding binding) {
		this.binding = binding;
	}

	/**
	 * Copies binding state from runtime metadata.
	 *
	 * @param binding
	 *            the source metadata to copy
	 * @return this builder
	 */
	public FluentInjectBinding from(@SuppressWarnings("hiding") InjectBinding binding) {
		readOnly(binding.getReadOnly());
		super.from(binding);
		return this;
	}

	/**
	 * Sets whether the injected binding is read-only.
	 *
	 * @param readOnly
	 *            {@code Boolean.TRUE} to mark the binding read-only, {@code Boolean.FALSE} to allow changes, or {@code null} to clear the setting
	 * @return this builder
	 */
	public FluentInjectBinding readOnly(Boolean readOnly) {
		binding.setReadOnly(readOnly);
		return this;
	}

	/**
	 * Returns the underlying mutable metadata.
	 *
	 * @return the wrapped inject-binding metadata
	 */
	@Override
	public InjectBinding get() {
		return binding;
	}

}
