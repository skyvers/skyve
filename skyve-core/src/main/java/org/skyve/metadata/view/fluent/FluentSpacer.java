package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.widget.Spacer;

/**
 * Fluent builder for spacer widget metadata.
 */
public class FluentSpacer extends FluentWidget implements FluentAbsoluteSize<FluentSpacer> {
	private Spacer spacer = null;

	/**
	 * Creates a builder backed by a new {@link Spacer} metadata instance.
	 */
	public FluentSpacer() {
		spacer = new Spacer();
	}

	/**
	 * Creates a builder backed by the supplied spacer metadata instance.
	 *
	 * @param spacer
	 *            the spacer metadata to mutate
	 */
	public FluentSpacer(Spacer spacer) {
		this.spacer = spacer;
	}

	/**
	 * Copies spacer metadata into this builder.
	 *
	 * @param spacer
	 *            the source spacer metadata
	 * @return this builder
	 */
	public FluentSpacer from(@SuppressWarnings("hiding") Spacer spacer) {

		invisibleConditionName(spacer.getInvisibleConditionName());

		absoluteSize(spacer, this);

		return this;
	}

	/**
	 * Sets the condition name that hides this spacer when it evaluates to true.
	 *
	 * @param invisibleConditionName
	 *            the invisibility condition identifier
	 * @return this builder
	 */
	public FluentSpacer invisibleConditionName(String invisibleConditionName) {
		spacer.setInvisibleConditionName(invisibleConditionName);
		return this;
	}

	/**
	 * Sets spacer width in pixels.
	 *
	 * @param width
	 *            the spacer width in pixels
	 * @return this builder
	 */
	@Override
	public FluentSpacer pixelWidth(int width) {
		spacer.setPixelWidth(Integer.valueOf(width));
		return this;
	}

	/**
	 * Sets spacer height in pixels.
	 *
	 * @param height
	 *            the spacer height in pixels
	 * @return this builder
	 */
	@Override
	public FluentSpacer pixelHeight(int height) {
		spacer.setPixelHeight(Integer.valueOf(height));
		return this;
	}

	/**
	 * Returns the backing metadata instance.
	 *
	 * @return the mutable spacer metadata
	 */
	@Override
	public Spacer get() {
		return spacer;
	}
}
