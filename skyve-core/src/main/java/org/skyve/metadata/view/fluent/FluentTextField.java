package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.widget.bound.input.CompleteType;
import org.skyve.impl.metadata.view.widget.bound.input.KeyboardType;
import org.skyve.impl.metadata.view.widget.bound.input.TextField;

/**
 * Builds {@link TextField} widget metadata using a fluent API.
 */
@SuppressWarnings("java:S110") // This inheritance-depth warning is ridiculous for intentional framework hierarchies.
public class FluentTextField extends FluentChangeableInputWidget<FluentTextField> implements FluentAbsoluteWidth<FluentTextField> {
	private TextField text = null;

	/**
	 * Creates a builder backed by a new {@link TextField}.
	 */
	public FluentTextField() {
		text = new TextField();
	}

	/**
	 * Creates a builder backed by the supplied {@link TextField}.
	 *
	 * @param text
	 *            the metadata instance to mutate
	 */
	public FluentTextField(TextField text) {
		this.text = text;
	}

	/**
	 * Copies text-field metadata into this fluent builder.
	 *
	 * @param text
	 *            the source metadata to copy
	 * @return this builder
	 */
	public FluentTextField from(@SuppressWarnings("hiding") TextField text) {
		Boolean b = text.getEditable();
		if (b != null) {
			editable(b.booleanValue());
		}
		keyboardType(text.getKeyboardType());
		complete(text.getComplete());

		absoluteWidth(text, this);

		super.from(text);
		return this;
	}

	/**
	 * Sets whether this text field is editable.
	 *
	 * @param editable
	 *            {@code true} to allow editing
	 * @return this builder
	 */
	public FluentTextField editable(boolean editable) {
		text.setEditable(editable ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	/**
	 * Sets the keyboard type for mobile devices.
	 *
	 * @param keyboardType
	 *            the keyboard type
	 * @return this builder
	 */
	public FluentTextField keyboardType(KeyboardType keyboardType) {
		text.setKeyboardType(keyboardType);
		return this;
	}

	/**
	 * Sets the auto-complete type for this text field.
	 *
	 * @param complete
	 *            the complete type
	 * @return this builder
	 */
	public FluentTextField complete(CompleteType complete) {
		text.setComplete(complete);
		return this;
	}

	/**
	 * Sets the pixel width of this text field.
	 *
	 * @param width
	 *            the pixel width
	 * @return this builder
	 */
	@Override
	public FluentTextField pixelWidth(int width) {
		text.setPixelWidth(Integer.valueOf(width));
		return this;
	}

	/**
	 * Returns the underlying mutable metadata.
	 *
	 * @return the wrapped text-field metadata
	 */
	@Override
	public TextField get() {
		return text;
	}
}
