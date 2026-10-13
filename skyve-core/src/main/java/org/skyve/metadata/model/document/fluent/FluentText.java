package org.skyve.metadata.model.document.fluent;

import org.skyve.impl.metadata.model.document.field.Text;
import org.skyve.impl.metadata.model.document.field.TextFormat;
import org.skyve.impl.metadata.model.document.field.validator.TextValidator;

import jakarta.annotation.Nonnull;

/**
 * Provides a fluent builder for FluentText metadata.
 */
public class FluentText extends FluentConvertibleField<FluentText> {
	private Text text = null;
	
	/**
	 * Creates a fluent builder instance.
	 */
	public FluentText() {
		text = new Text();
	}
	
	/**
	 * Creates a fluent builder instance.
	 */
	public FluentText(@Nonnull Text text) {
		this.text = text;
	}

	/**
	 * Copies metadata values from an existing definition into this builder.
	 */

	public @Nonnull FluentText from(@SuppressWarnings("hiding") @Nonnull Text text) {
		super.from(text);
		length(text.getLength());
		TextFormat format = text.getFormat();
		if (format != null) {
			format(new FluentTextFormat().from(format));
		}
		TextValidator validator = text.getValidator();
		if (validator != null) {
			validator(new FluentTextValidator().from(validator));
		}
		return this;
	}
	
	/**
	 * Updates metadata on this fluent builder.
	 */
	public @Nonnull FluentText length(int length) {
		text.setLength(length);
		return this;
	}
	
	/**
	 * Updates metadata on this fluent builder.
	 */
	public @Nonnull FluentText format(@Nonnull FluentTextFormat format) {
		text.setFormat(format.get());
		return this;
	}
	/**
	 * Sets validation metadata used by this field definition.
	 */
	public @Nonnull FluentText validator(@Nonnull FluentTextValidator validator) {
		text.setValidator(validator.get());
		return this;
	}

	/**
	 * Returns the mutable metadata instance represented by this builder.
	 */
	@Override
	public @Nonnull Text get() {
		return text;
	}
}
