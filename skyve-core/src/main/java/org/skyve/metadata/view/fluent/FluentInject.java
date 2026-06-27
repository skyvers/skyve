package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.Inject;

/**
 * Builds {@link Inject} metadata for script-based client injection.
 */
public class FluentInject extends FluentWidget {
	private Inject inject = null;

	/**
	 * Creates a builder backed by a new {@link Inject}.
	 */
	public FluentInject() {
		inject = new Inject();
	}

	/**
	 * Creates a builder backed by the supplied {@link Inject}.
	 *
	 * @param inject
	 *            the metadata instance to mutate
	 */
	public FluentInject(Inject inject) {
		this.inject = inject;
	}

	/**
	 * Copies script and binding metadata from runtime injection metadata.
	 *
	 * @param inject
	 *            the source metadata to copy
	 * @return this builder
	 */
	public FluentInject from(@SuppressWarnings("hiding") Inject inject) {

		script(inject.getScript());

		inject.getBindings().forEach(b -> addBinding(new FluentInjectBinding().from(b)));

		return this;
	}

	/**
	 * Sets the client-side script executed by the injection.
	 *
	 * @param script
	 *            the script body or identifier
	 * @return this builder
	 */
	public FluentInject script(String script) {
		inject.setScript(script);
		return this;
	}

	/**
	 * Appends an inject-binding definition.
	 *
	 * @param binding
	 *            the binding builder whose metadata is appended
	 * @return this builder
	 */
	public FluentInject addBinding(FluentInjectBinding binding) {
		inject.getBindings().add(binding.get());
		return this;
	}

	/**
	 * Returns the underlying mutable metadata.
	 *
	 * @return the wrapped inject metadata
	 */
	@Override
	public Inject get() {
		return inject;
	}
}
