package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.widget.bound.input.Slider;

/**
 * Builds {@link Slider} widget metadata using a fluent API.
 */
@SuppressWarnings("java:S110") // This inheritance-depth warning is ridiculous for intentional framework hierarchies.
public class FluentSlider extends FluentChangeableInputWidget<FluentSlider>
		implements FluentAbsoluteSize<FluentSlider>, FluentConstrainableHeight<FluentSlider> {
	private Slider slider = null;

	/**
	 * Creates a fluent builder backed by a new {@link Slider} metadata instance.
	 */
	public FluentSlider() {
		slider = new Slider();
	}

	/**
	 * Creates a fluent builder backed by the supplied {@link Slider} metadata instance.
	 *
	 * @param slider the metadata instance to mutate
	 */
	public FluentSlider(Slider slider) {
		this.slider = slider;
	}

	/**
	 * Copies slider metadata into this fluent builder.
	 *
	 * @param slider
	 *            the source metadata to copy
	 * @return this builder
	 */
	public FluentSlider from(@SuppressWarnings("hiding") Slider slider) {
		Double d = slider.getMin();
		if (d != null) {
			min(d.doubleValue());
		}
		d = slider.getMax();
		if (d != null) {
			max(d.doubleValue());
		}
		Integer i = slider.getNumberOfDiscreteValues();
		if (i != null) {
			numberOfDiscreteValues(i.intValue());
		}
		i = slider.getRoundingPrecision();
		if (i != null) {
			roundingPrecision(i.intValue());
		}
		Boolean b = slider.getVertical();
		if (b != null) {
			vertical(b.booleanValue());
		}

		absoluteSize(slider, this);

		constrainableHeight(slider, this);

		super.from(slider);
		return this;
	}

	/**
	 * Sets the minimum value for this slider.
	 *
	 * @param min the minimum value
	 * @return this builder
	 */
	public FluentSlider min(double min) {
		slider.setMin(Double.valueOf(min));
		return this;
	}

	/**
	 * Sets the maximum value for this slider.
	 *
	 * @param max the maximum value
	 * @return this builder
	 */
	public FluentSlider max(double max) {
		slider.setMax(Double.valueOf(max));
		return this;
	}

	/**
	 * Sets the number of discrete values on this slider.
	 *
	 * @param numberOfDiscreteValues the number of discrete steps
	 * @return this builder
	 */
	public FluentSlider numberOfDiscreteValues(int numberOfDiscreteValues) {
		slider.setNumberOfDiscreteValues(Integer.valueOf(numberOfDiscreteValues));
		return this;
	}

	/**
	 * Sets the rounding precision for the slider value.
	 *
	 * @param roundingPrecision the number of decimal places
	 * @return this builder
	 */
	public FluentSlider roundingPrecision(int roundingPrecision) {
		slider.setRoundingPrecision(Integer.valueOf(roundingPrecision));
		return this;
	}

	/**
	 * Sets whether this slider is oriented vertically.
	 *
	 * @param vertical {@code true} to display the slider vertically
	 * @return this builder
	 */
	public FluentSlider vertical(boolean vertical) {
		slider.setVertical(vertical ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	/**
	 * Sets the pixel width of this slider widget.
	 *
	 * @param width the pixel width
	 * @return this builder
	 */
	@Override
	public FluentSlider pixelWidth(int width) {
		slider.setPixelWidth(Integer.valueOf(width));
		return this;
	}

	/**
	 * Sets the pixel height of this slider widget.
	 *
	 * @param height the pixel height
	 * @return this builder
	 */
	@Override
	public FluentSlider pixelHeight(int height) {
		slider.setPixelHeight(Integer.valueOf(height));
		return this;
	}

	/**
	 * Sets the minimum pixel height of this slider widget.
	 *
	 * @param minPixelHeight the minimum pixel height
	 * @return this builder
	 */
	@Override
	public FluentSlider minPixelHeight(int minPixelHeight) {
		slider.setMinPixelHeight(Integer.valueOf(minPixelHeight));
		return this;
	}

	/**
	 * Sets the maximum pixel height of this slider widget.
	 *
	 * @param maxPixelHeight the maximum pixel height
	 * @return this builder
	 */
	@Override
	public FluentSlider maxPixelHeight(int maxPixelHeight) {
		slider.setMaxPixelHeight(Integer.valueOf(maxPixelHeight));
		return this;
	}

	/**
	 * Returns the underlying mutable metadata.
	 *
	 * @return the wrapped slider metadata
	 */
	@Override
	public Slider get() {
		return slider;
	}
}
