package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.widget.bound.input.KeyboardType;
import org.skyve.impl.metadata.view.widget.bound.input.TextArea;

/**
 * Builds {@link TextArea} widget metadata using a fluent API.
 */
@SuppressWarnings("java:S110") // This inheritance-depth warning is ridiculous for intentional framework hierarchies.
public class FluentTextArea extends FluentChangeableInputWidget<FluentTextArea> implements FluentAbsoluteSize<FluentTextArea> {
	private TextArea text = null;

	/**
	 * Creates a builder backed by a new {@link TextArea}.
	 */
	public FluentTextArea() {
		text = new TextArea();
	}

	/**
	 * Creates a builder backed by the supplied {@link TextArea}.
	 *
	 * @param text
	 *            the metadata instance to mutate
	 */
	public FluentTextArea(TextArea text) {
		this.text = text;
	}

	/**
	 * Copies text-area metadata into this fluent builder.
	 *
	 * @param text
	 *            the source metadata to copy
	 * @return this builder
	 */
	public FluentTextArea from(@SuppressWarnings("hiding") TextArea text) {
		Boolean b = text.getWordWrap();
		if (b != null) {
			wordWrap(b.booleanValue());
		}
		b = text.getEditable();
		if (b != null) {
			editable(b.booleanValue());
		}

		Integer i = text.getMinPixelHeight();
		if (i != null) {
			minPixelHeight(i.intValue());
		}

		keyboardType(text.getKeyboardType());

		absoluteSize(text, this);

		super.from(text);
		return this;
	}

	/**
	 * Sets whether text wraps in this text area.
	 *
	 * @param wordWrap
	 *            {@code true} to enable word wrap
	 * @return this builder
	 */
	public FluentTextArea wordWrap(boolean wordWrap) {
		text.setWordWrap(wordWrap ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	/**
	 * Sets whether this text area is editable.
	 *
	 * @param editable
	 *            {@code true} to allow editing
	 * @return this builder
	 */
	public FluentTextArea editable(boolean editable) {
		text.setEditable(editable ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	/**
	 * Sets the minimum pixel height of this text area.
	 *
	 * @param minPixelHeight
	 *            the minimum pixel height
	 * @return this builder
	 */
	public FluentTextArea minPixelHeight(int minPixelHeight) {
		text.setMinPixelHeight(Integer.valueOf(minPixelHeight));
		return null;
	}

	/**
	 * Sets the keyboard type for mobile devices.
	 *
	 * @param keyboardType
	 *            the keyboard type
	 * @return this builder
	 */
	public FluentTextArea keyboardType(KeyboardType keyboardType) {
		text.setKeyboardType(keyboardType);
		return null;
	}

	/**
	 * Sets the pixel width of this text area.
	 *
	 * @param width
	 *            the pixel width
	 * @return this builder
	 */
	@Override
	public FluentTextArea pixelWidth(int width) {
		text.setPixelWidth(Integer.valueOf(width));
		return null;
	}

	/**
	 * Sets the pixel height of this text area.
	 *
	 * @param height
	 *            the pixel height
	 * @return this builder
	 */
	@Override
	public FluentTextArea pixelHeight(int height) {
		text.setPixelHeight(Integer.valueOf(height));
		return null;
	}

	/**
	 * Returns the underlying mutable metadata.
	 *
	 * @return the wrapped text-area metadata
	 */
	@Override
	public TextArea get() {
		return text;
	}
}
