package org.skyve.metadata.view.fluent;

import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.view.widget.bound.ProgressBar;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

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
		BindUtil.copyAuthoredCondition(bar.getXmlInvisibleCondition(), bar.getXmlVisibleCondition(), this::invisibleCondition, this::visibleCondition);
		absoluteSize(bar, this);
		constrainableSize(bar, this);
		super.from(bar);
		bar.getProperties().entrySet().forEach(p -> putProperty(p.getKey(), p.getValue()));

		return this;
	}

	/**
	 * Sets the {@code invisible} condition as authored; the opposite sense of {@link #visibleCondition(String)}.
	 *
	 * @param invisibleCondition
	 *            a condition name, an inline condition expression or a boolean literal
	 * @return this builder
	 */
	public @Nonnull FluentProgressBar invisibleCondition(@Nullable String invisibleCondition) {
		bar.setXmlInvisibleCondition(invisibleCondition);
		return this;
	}

	/**
	 * Sets the {@code visible} condition as authored; the opposite sense of {@link #invisibleCondition(String)}.
	 *
	 * @param visibleCondition
	 *            a condition name, an inline condition expression or a boolean literal
	 * @return this builder
	 */
	public @Nonnull FluentProgressBar visibleCondition(@Nullable String visibleCondition) {
		bar.setXmlVisibleCondition(visibleCondition);
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
	 * Adds or replaces a custom property on the wrapped metadata.
	 * Properties keep the order in which they were first added.
	 *
	 * @param k the property name
	 * @param v the property value
	 * @return this builder
	 */
	public @Nonnull FluentProgressBar putProperty(@Nonnull String k, @Nonnull String v) {
		bar.getProperties().put(k, v);
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
