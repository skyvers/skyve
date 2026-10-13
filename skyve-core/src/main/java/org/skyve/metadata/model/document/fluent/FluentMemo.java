package org.skyve.metadata.model.document.fluent;

import org.skyve.impl.metadata.model.document.field.Memo;

import jakarta.annotation.Nonnull;

/**
 * Provides a fluent builder for FluentMemo metadata.
 */
public class FluentMemo extends FluentConstrainableField<FluentMemo> {
	private Memo memo = null;
	
	/**
	 * Creates a fluent builder instance.
	 */
	public FluentMemo() {
		memo = new Memo();
	}
	
	/**
	 * Creates a fluent builder instance.
	 */
	public FluentMemo(@Nonnull Memo memo) {
		this.memo = memo;
	}

	/**
	 * Copies metadata values from an existing definition into this builder.
	 */

	public @Nonnull FluentMemo from(@SuppressWarnings("hiding") @Nonnull Memo memo) {
		super.from(memo);
		return this;
	}
	
	/**
	 * Returns the mutable metadata instance represented by this builder.
	 */
	@Override
	public @Nonnull Memo get() {
		return memo;
	}
}
