package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.widget.bound.ProgressBar;

/**
 * Builds {@link ProgressBar} metadata.
 */
public class FluentProgressBar extends FluentBoundWidget<FluentProgressBar>
		implements FluentAbsoluteSize<FluentProgressBar>, FluentConstrainableSize<FluentProgressBar> {
	private ProgressBar bar = null;

	/**
	 * Creates a builder backed by a new {@link ProgressBar}.
	 */
	public FluentProgressBar() {
		bar = new ProgressBar();
	}

	/**
	 * Creates a builder backed by the supplied {@link ProgressBar}.
	 *
	 * @param bar
	 *            the metadata instance to mutate
	 */
	public FluentProgressBar(ProgressBar bar) {
		this.bar = bar;
	}

	/**
	 * Copies progress-bar sizing and visibility state from runtime metadata.
	 *
	 * @param bar
	 *            the source metadata to copy
	 * @return this builder
	 */
	public FluentProgressBar from(@SuppressWarnings("hiding") ProgressBar bar) {

		invisibleConditionName(bar.getInvisibleConditionName());

		absoluteSize(bar, this);

		constrainableSize(bar, this);

		super.from(bar);
		return this;
	}

	/**
	 * Sets the condition name that hides this progress bar.
	 *
	 * @param invisibleConditionName the condition name
	 * @return this builder
	 */
	public FluentProgressBar invisibleConditionName(String invisibleConditionName) {
		bar.setInvisibleConditionName(invisibleConditionName);
		return this;
	}

	/**
	 * Sets the pixel width of this progress bar.
	 *
	 * @param width the pixel width
	 * @return this builder
	 */
	@Override
	public FluentProgressBar pixelWidth(int width) {
		bar.setPixelWidth(Integer.valueOf(width));
		return this;
	}

	/**
	 * Sets the pixel height of this progress bar.
	 *
	 * @param height the pixel height
	 * @return this builder
	 */
	@Override
	public FluentProgressBar pixelHeight(int height) {
		bar.setPixelHeight(Integer.valueOf(height));
		return this;
	}

	/**
	 * Sets the maximum pixel height of this progress bar.
	 *
	 * @param maxPixelHeight the maximum pixel height
	 * @return this builder
	 */
	@Override
	public FluentProgressBar maxPixelHeight(int maxPixelHeight) {
		bar.setMaxPixelHeight(Integer.valueOf(maxPixelHeight));
		return this;
	}

	/**
	 * Sets the minimum pixel height of this progress bar.
	 *
	 * @param minPixelHeight the minimum pixel height
	 * @return this builder
	 */
	@Override
	public FluentProgressBar minPixelHeight(int minPixelHeight) {
		bar.setMinPixelHeight(Integer.valueOf(minPixelHeight));
		return this;
	}

	/**
	 * Sets the minimum pixel width of this progress bar.
	 *
	 * @param minPixelWidth the minimum pixel width
	 * @return this builder
	 */
	@Override
	public FluentProgressBar minPixelWidth(int minPixelWidth) {
		bar.setMinPixelWidth(Integer.valueOf(minPixelWidth));
		return this;
	}

	/**
	 * Sets the maximum pixel width of this progress bar.
	 *
	 * @param maxPixelWidth the maximum pixel width
	 * @return this builder
	 */
	@Override
	public FluentProgressBar maxPixelWidth(int maxPixelWidth) {
		bar.setMaxPixelWidth(Integer.valueOf(maxPixelWidth));
		return this;
	}

	/**
	 * Returns the underlying mutable metadata.
	 *
	 * @return the wrapped progress-bar metadata
	 */
	@Override
	public ProgressBar get() {
		return bar;
	}
}
