package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.widget.bound.input.HTML;
import org.skyve.metadata.view.TextOutput.Sanitisation;

/**
 * Builds {@link HTML} widget metadata using a fluent API.
 */
public class FluentHTML extends FluentInputWidget<FluentHTML> implements FluentAbsoluteSize<FluentHTML> {
	private HTML html = null;

	/**
	 * Creates a fluent builder backed by a new {@link HTML} metadata instance.
	 */
	public FluentHTML() {
		html = new HTML();
	}

	/**
	 * Creates a fluent builder backed by the supplied {@link HTML} metadata instance.
	 *
	 * @param html the metadata instance to mutate
	 */
	public FluentHTML(HTML html) {
		this.html = html;
	}

	/**
	 * Copies HTML widget metadata into this fluent builder.
	 *
	 * @param html
	 *            the source metadata to copy
	 * @return this builder
	 */
	public FluentHTML from(@SuppressWarnings("hiding") HTML html) {

		sanitise(html.getSanitise());

		absoluteSize(html, this);

		super.from(html);
		return this;
	}

	/**
	 * Sets the sanitisation policy for this HTML widget.
	 *
	 * @param sanitise the sanitisation policy to apply
	 * @return this builder
	 */
	public FluentHTML sanitise(Sanitisation sanitise) {
		html.setSanitise(sanitise);
		return this;
	}

	/**
	 * Sets the pixel width of this HTML widget.
	 *
	 * @param width the pixel width
	 * @return this builder
	 */
	@Override
	public FluentHTML pixelWidth(int width) {
		html.setPixelWidth(Integer.valueOf(width));
		return this;
	}

	/**
	 * Sets the pixel height of this HTML widget.
	 *
	 * @param height the pixel height
	 * @return this builder
	 */
	@Override
	public FluentHTML pixelHeight(int height) {
		html.setPixelHeight(Integer.valueOf(height));
		return this;
	}

	/**
	 * Returns the wrapped {@link HTML} metadata instance.
	 *
	 * @return the mutable HTML metadata being configured
	 */
	@Override
	public HTML get() {
		return html;
	}
}
