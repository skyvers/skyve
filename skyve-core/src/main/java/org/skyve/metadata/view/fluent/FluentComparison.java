package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.widget.bound.input.Comparison;

/**
 * Builds {@link Comparison} widget metadata using a fluent API.
 */
public class FluentComparison extends FluentInputWidget<FluentComparison> implements FluentRelativeSize<FluentComparison> {
	private Comparison comparison = null;

	/**
	 * Creates a fluent builder backed by a new {@link Comparison} metadata instance.
	 */
	public FluentComparison() {
		comparison = new Comparison();
	}

	/**
	 * Creates a fluent builder backed by the supplied comparison metadata instance.
	 *
	 * @param comparison the metadata instance to mutate
	 */
	public FluentComparison(Comparison comparison) {
		this.comparison = comparison;
	}

	/**
	 * Copies comparison metadata into this fluent builder.
	 */
	public FluentComparison from(@SuppressWarnings("hiding") Comparison comparison) {

		relativeSize(comparison, this);

		modelName(comparison.getModelName());
		Boolean b = comparison.getEditable();
		if (b != null) {
			editable(b.booleanValue());
		}

		super.from(comparison);
		return this;
	}

	/**
	 * Sets the model name used to populate this comparison widget.
	 *
	 * @param modelName the model name
	 * @return this builder
	 */
	public FluentComparison modelName(String modelName) {
		comparison.setModelName(modelName);
		return this;
	}

	/**
	 * Sets whether this comparison widget is editable.
	 *
	 * @param editable {@code true} to allow editing
	 * @return this builder
	 */
	public FluentComparison editable(boolean editable) {
		comparison.setEditable(editable ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	/**
	 * Sets the absolute pixel height of this comparison widget.
	 *
	 * @param height the height in pixels
	 * @return this builder
	 */
	@Override
	public FluentComparison pixelHeight(int height) {
		comparison.setPixelHeight(Integer.valueOf(height));
		return this;
	}

	/**
	 * Sets the absolute pixel width of this comparison widget.
	 *
	 * @param width the width in pixels
	 * @return this builder
	 */
	@Override
	public FluentComparison pixelWidth(int width) {
		comparison.setPixelWidth(Integer.valueOf(width));
		return this;
	}

	/**
	 * Sets the minimum pixel width of this comparison widget.
	 *
	 * @param minPixelWidth the minimum width in pixels
	 * @return this builder
	 */
	@Override
	public FluentComparison minPixelWidth(int minPixelWidth) {
		comparison.setMinPixelWidth(Integer.valueOf(minPixelWidth));
		return this;
	}

	/**
	 * Sets the maximum pixel width of this comparison widget.
	 *
	 * @param maxPixelWidth the maximum width in pixels
	 * @return this builder
	 */
	@Override
	public FluentComparison maxPixelWidth(int maxPixelWidth) {
		comparison.setMaxPixelWidth(Integer.valueOf(maxPixelWidth));
		return this;
	}

	/**
	 * Sets the maximum pixel height of this comparison widget.
	 *
	 * @param maxPixelHeight the maximum height in pixels
	 * @return this builder
	 */
	@Override
	public FluentComparison maxPixelHeight(int maxPixelHeight) {
		comparison.setMaxPixelHeight(Integer.valueOf(maxPixelHeight));
		return this;
	}

	/**
	 * Sets the minimum pixel height of this comparison widget.
	 *
	 * @param minPixelHeight the minimum height in pixels
	 * @return this builder
	 */
	@Override
	public FluentComparison minPixelHeight(int minPixelHeight) {
		comparison.setMinPixelHeight(Integer.valueOf(minPixelHeight));
		return this;
	}

	/**
	 * Sets the percentage width of this comparison widget.
	 *
	 * @param percentageWidth the width as a percentage
	 * @return this builder
	 */
	@Override
	public FluentComparison percentageWidth(int percentageWidth) {
		comparison.setPercentageWidth(Integer.valueOf(percentageWidth));
		return this;
	}

	/**
	 * Sets the percentage height of this comparison widget.
	 *
	 * @param percentageHeight the height as a percentage
	 * @return this builder
	 */
	@Override
	public FluentComparison percentageHeight(int percentageHeight) {
		comparison.setPercentageHeight(Integer.valueOf(percentageHeight));
		return this;
	}

	/**
	 * Sets the responsive width breakpoint for this comparison widget.
	 *
	 * @param responsiveWidth the responsive width value
	 * @return this builder
	 */
	@Override
	public FluentComparison responsiveWidth(int responsiveWidth) {
		comparison.setResponsiveWidth(Integer.valueOf(responsiveWidth));
		return this;
	}

	/**
	 * Sets the small (sm) responsive column span for this comparison widget.
	 *
	 * @param sm the column span at the sm breakpoint
	 * @return this builder
	 */
	@Override
	public FluentComparison sm(int sm) {
		comparison.setSm(Integer.valueOf(sm));
		return this;
	}

	/**
	 * Sets the medium (md) responsive column span for this comparison widget.
	 *
	 * @param md the column span at the md breakpoint
	 * @return this builder
	 */
	@Override
	public FluentComparison md(int md) {
		comparison.setMd(Integer.valueOf(md));
		return this;
	}

	/**
	 * Sets the large (lg) responsive column span for this comparison widget.
	 *
	 * @param lg the column span at the lg breakpoint
	 * @return this builder
	 */
	@Override
	public FluentComparison lg(int lg) {
		comparison.setLg(Integer.valueOf(lg));
		return this;
	}

	/**
	 * Sets the extra-large (xl) responsive column span for this comparison widget.
	 *
	 * @param xl the column span at the xl breakpoint
	 * @return this builder
	 */
	@Override
	public FluentComparison xl(int xl) {
		comparison.setXl(Integer.valueOf(xl));
		return this;
	}

	/**
	 * Returns the wrapped comparison metadata instance.
	 *
	 * @return the mutable comparison metadata being configured
	 */
	@Override
	public Comparison get() {
		return comparison;
	}

}
