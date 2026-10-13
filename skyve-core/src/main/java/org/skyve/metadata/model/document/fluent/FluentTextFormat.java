package org.skyve.metadata.model.document.fluent;

import org.skyve.domain.types.converters.Format.TextCase;
import org.skyve.impl.metadata.model.document.field.TextFormat;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Provides a fluent builder for FluentTextFormat metadata.
 */
public class FluentTextFormat {
	private TextFormat format = null;
	
	/**
	 * Creates a fluent builder instance.
	 */
	public FluentTextFormat() {
		format = new TextFormat();
	}

	/**
	 * Creates a fluent builder instance.
	 */
	public FluentTextFormat(@Nonnull TextFormat format) {
		this.format = format;
	}

	/**
	 * Copies metadata values from an existing definition into this builder.
	 */

	public @Nonnull FluentTextFormat from(@SuppressWarnings("hiding") @Nonnull TextFormat format) {
		mask(format.getMask());
		textCase(format.getCase());
		return this;
	}
	
	/**
	 * Updates metadata on this fluent builder.
	 */
	public @Nonnull FluentTextFormat mask(@Nullable String mask) {
		format.setMask(mask);
		return this;
	}
	
	/**
	 * Updates metadata on this fluent builder.
	 */
	public @Nonnull FluentTextFormat textCase(@Nullable TextCase textCase) {
		format.setCase(textCase);
		return this;
	}
	/**
	 * Returns the mutable metadata instance represented by this builder.
	 */
	public @Nonnull TextFormat get() {
		return format;
	}
}
