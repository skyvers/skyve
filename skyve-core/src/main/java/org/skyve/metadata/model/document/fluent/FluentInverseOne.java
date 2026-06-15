package org.skyve.metadata.model.document.fluent;

import org.skyve.impl.metadata.model.document.InverseOne;

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
	public FluentInverseOne(InverseOne inverse) {
		this.inverse = inverse;
	}

	/**
	 * Copies metadata values from an existing definition into this builder.
	 */

	public FluentInverseOne from(@SuppressWarnings("hiding") InverseOne inverse) {
		super.from(inverse);
		return this;
	}
	
	@Override
	/**
	 * Returns the mutable metadata instance represented by this builder.
	 */
	public InverseOne get() {
		return inverse;
	}
}
