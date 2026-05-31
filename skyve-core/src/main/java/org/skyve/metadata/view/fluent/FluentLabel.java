package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.HorizontalAlignment;
import org.skyve.impl.metadata.view.widget.bound.Label;
import org.skyve.metadata.view.TextOutput.Sanitisation;

/**
 * Builds {@link Label} widget metadata using a fluent API.
 */
public class FluentLabel extends FluentBoundWidget<FluentLabel> implements FluentAbsoluteSize<FluentLabel> {
	private Label label = null;

	/**
	 * Creates a fluent builder backed by a new {@link Label} metadata instance.
	 */
	public FluentLabel() {
		label = new Label();
	}

	/**
	 * Creates a fluent builder backed by the supplied {@link Label} metadata instance.
	 *
	 * @param label the metadata instance to mutate
	 */
	public FluentLabel(Label label) {
		this.label = label;
	}

	/**
	 * Copies label metadata into this fluent builder.
	 *
	 * @param label
	 *            the source metadata to copy
	 * @return this builder
	 */
	public FluentLabel from(@SuppressWarnings("hiding") Label label) {

		value(label.getValue());
		forBinding(label.getFor());

		absoluteSize(label, this);

		invisibleConditionName(label.getInvisibleConditionName());
		Boolean b = label.getFormatted();
		if (b != null) {
			formatted(b.booleanValue());
		}
		textAlignment(label.getTextAlignment());
		b = label.getEscape();
		if (b != null) {
			escape(b.booleanValue());
		}
		sanitise(label.getSanitise());

		super.from(label);
		return this;
	}

	/**
	 * Sets the static value text for this label.
	 *
	 * @param value the label text
	 * @return this builder
	 */
	public FluentLabel value(String value) {
		label.setValue(value);
		return this;
	}

	/**
	 * Sets the binding this label is associated with.
	 *
	 * @param forBinding the binding expression
	 * @return this builder
	 */
	public FluentLabel forBinding(String forBinding) {
		label.setFor(forBinding);
		return this;
	}

	/**
	 * Sets the condition name that hides this label.
	 *
	 * @param invisibleConditionName the condition name
	 * @return this builder
	 */
	public FluentLabel invisibleConditionName(String invisibleConditionName) {
		label.setInvisibleConditionName(invisibleConditionName);
		return this;
	}

	/**
	 * Sets whether the bound value is rendered with format string applied.
	 *
	 * @param formatted {@code true} to apply formatting
	 * @return this builder
	 */
	public FluentLabel formatted(boolean formatted) {
		label.setFormatted(formatted ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	/**
	 * Sets the horizontal text alignment for this label.
	 *
	 * @param textAlignment the alignment value
	 * @return this builder
	 */
	public FluentLabel textAlignment(HorizontalAlignment textAlignment) {
		label.setTextAlignment(textAlignment);
		return this;
	}

	/**
	 * Sets whether HTML characters in the label value are escaped.
	 *
	 * @param escape {@code true} to escape HTML characters
	 * @return this builder
	 */
	public FluentLabel escape(boolean escape) {
		label.setEscape(escape ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	/**
	 * Sets the sanitisation policy for this label's output.
	 *
	 * @param sanitise the sanitisation policy
	 * @return this builder
	 */
	public FluentLabel sanitise(Sanitisation sanitise) {
		label.setSanitise(sanitise);
		return this;
	}

	/**
	 * Sets the pixel width of this label.
	 *
	 * @param width the pixel width
	 * @return this builder
	 */
	@Override
	public FluentLabel pixelWidth(int width) {
		label.setPixelWidth(Integer.valueOf(width));
		return this;
	}

	/**
	 * Sets the pixel height of this label.
	 *
	 * @param height the pixel height
	 * @return this builder
	 */
	@Override
	public FluentLabel pixelHeight(int height) {
		label.setPixelHeight(Integer.valueOf(height));
		return this;
	}

	/**
	 * Returns the wrapped {@link Label} metadata instance.
	 *
	 * @return the mutable label metadata being configured
	 */
	@Override
	public Label get() {
		return label;
	}
}
