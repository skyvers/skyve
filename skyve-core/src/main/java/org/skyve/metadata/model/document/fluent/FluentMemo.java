package org.skyve.metadata.model.document.fluent;

import org.skyve.impl.metadata.model.document.field.Memo;

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
	public FluentMemo(Memo memo) {
		this.memo = memo;
	}

	/**
	 * Copies metadata values from an existing definition into this builder.
	 */

	public FluentMemo from(@SuppressWarnings("hiding") Memo memo) {
		super.from(memo);
		return this;
	}
	
	@Override
	/**
	 * Returns the mutable metadata instance represented by this builder.
	 */
	public Memo get() {
		return memo;
	}
}
