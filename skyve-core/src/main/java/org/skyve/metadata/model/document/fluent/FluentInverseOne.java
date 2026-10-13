package org.skyve.metadata.model.document.fluent;

import org.skyve.impl.metadata.model.document.InverseOne;

import jakarta.annotation.Nonnull;

/**
 * Provides a fluent builder for FluentInverseOne metadata.
 */
public class FluentInverseOne extends FluentInverse<FluentInverseOne> {
	private InverseOne inverse = null;
	
	/**
	 * Creates a fluent builder instance.
	 */
	public FluentInverseOne() {
		inverse = new InverseOne();
	}

	/**
	 * Creates a fluent builder instance.
	 */
	public FluentInverseOne(@Nonnull InverseOne inverse) {
		this.inverse = inverse;
	}

	/**
	 * Copies metadata values from an existing definition into this builder.
	 */

	public @Nonnull FluentInverseOne from(@SuppressWarnings("hiding") @Nonnull InverseOne inverse) {
		super.from(inverse);
		return this;
	}
	
	/**
	 * Returns the mutable metadata instance represented by this builder.
	 */
	@Override
	public @Nonnull InverseOne get() {
		return inverse;
	}
}
