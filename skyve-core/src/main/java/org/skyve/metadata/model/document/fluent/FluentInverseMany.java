package org.skyve.metadata.model.document.fluent;

import org.skyve.impl.metadata.model.document.InverseMany;

import jakarta.annotation.Nonnull;

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
	public FluentInverseMany(@Nonnull InverseMany inverse) {
		this.inverse = inverse;
	}

	/**
	 * Copies metadata values from an existing definition into this builder.
	 */

	public @Nonnull FluentInverseMany from(@SuppressWarnings("hiding") @Nonnull InverseMany inverse) {
		super.from(inverse);
		return this;
	}
	
	/**
	 * Returns the mutable metadata instance represented by this builder.
	 */
	@Override
	public @Nonnull InverseMany get() {
		return inverse;
	}
}
