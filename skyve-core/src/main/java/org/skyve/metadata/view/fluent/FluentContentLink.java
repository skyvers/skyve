package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.widget.bound.input.ContentLink;

/**
 * Builds {@link ContentLink} widget metadata using a fluent API.
 */
public class FluentContentLink extends FluentInputWidget<FluentContentLink> implements FluentAbsoluteWidth<FluentContentLink> {
	private ContentLink link = null;

	/**
	 * Creates a fluent builder backed by a new {@link ContentLink} metadata instance.
	 */
	public FluentContentLink() {
		link = new ContentLink();
	}

	/**
	 * Creates a fluent builder backed by the supplied content-link metadata instance.
	 *
	 * @param link the metadata instance to mutate
	 */
	public FluentContentLink(ContentLink link) {
		this.link = link;
	}

	/**
	 * Copies content-link metadata into this fluent builder.
	 */
	public FluentContentLink from(@SuppressWarnings("hiding") ContentLink link) {

		value(link.getValue());
		Boolean b = link.getEditable();
		if (b != null) {
			editable(b.booleanValue());
		}

		absoluteWidth(link, this);

		link.getParameters().forEach(p -> addParameter(new FluentParameter().from(p)));

		super.from(link);
		return this;
	}

	/**
	 * Sets the static value displayed by this content-link widget.
	 *
	 * @param value the link value
	 * @return this builder
	 */
	public FluentContentLink value(String value) {
		link.setValue(value);
		return this;
	}

	/**
	 * Sets whether this content-link widget is editable.
	 *
	 * @param editable {@code true} to allow editing
	 * @return this builder
	 */
	public FluentContentLink editable(boolean editable) {
		link.setEditable(editable ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	/**
	 * Sets the absolute pixel width of this content-link widget.
	 *
	 * @param width the width in pixels
	 * @return this builder
	 */
	@Override
	public FluentContentLink pixelWidth(int width) {
		link.setPixelWidth(Integer.valueOf(width));
		return this;
	}

	/**
	 * Appends a link parameter definition to the wrapped metadata.
	 */
	public FluentContentLink addParameter(FluentParameter parameter) {
		link.getParameters().add(parameter.get());
		return this;
	}

	/**
	 * Returns the wrapped content-link metadata instance.
	 *
	 * @return the mutable content-link metadata being configured
	 */
	@Override
	public ContentLink get() {
		return link;
	}
}
