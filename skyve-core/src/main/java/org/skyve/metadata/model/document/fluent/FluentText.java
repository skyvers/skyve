package org.skyve.metadata.model.document.fluent;

import org.skyve.impl.metadata.model.document.field.Text;
import org.skyve.impl.metadata.model.document.field.TextFormat;
import org.skyve.impl.metadata.model.document.field.validator.TextValidator;

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
	public FluentText(Text text) {
		this.text = text;
	}

	/**
	 * Copies metadata values from an existing definition into this builder.
	 */

	public FluentText from(@SuppressWarnings("hiding") Text text) {
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
	public FluentText length(int length) {
		text.setLength(length);
		return this;
	}
	
	/**
	 * Updates metadata on this fluent builder.
	 */
	public FluentText format(FluentTextFormat format) {
		text.setFormat(format.get());
		return this;
	}
	/**
	 * Sets validation metadata used by this field definition.
	 */
	public FluentText validator(FluentTextValidator validator) {
		text.setValidator(validator.get());
		return this;
	}

	/**
	 * Returns the mutable metadata instance represented by this builder.
	 */
	@Override
	public Text get() {
		return text;
	}
}
