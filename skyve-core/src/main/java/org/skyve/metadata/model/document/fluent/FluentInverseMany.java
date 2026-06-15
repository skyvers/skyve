package org.skyve.metadata.model.document.fluent;

import org.skyve.impl.metadata.model.document.InverseMany;

/**
 * Provides a fluent builder for FluentInverseMany metadata.
 */
public class FluentInverseMany extends FluentInverse<FluentInverseMany> {
	private InverseMany inverse = null;
	
	/**
	 * Creates a fluent builder instance.
	 */
	public FluentInverseMany() {
		inverse = new InverseMany();
	}

	/**
	 * Creates a fluent builder instance.
	 */
	public FluentInverseMany(InverseMany inverse) {
		this.inverse = inverse;
	}

	/**
	 * Copies metadata values from an existing definition into this builder.
	 */

	public FluentInverseMany from(@SuppressWarnings("hiding") InverseMany inverse) {
		super.from(inverse);
		return this;
	}
	
	@Override
	/**
	 * Returns the mutable metadata instance represented by this builder.
	 */
	public InverseMany get() {
		return inverse;
	}
}
