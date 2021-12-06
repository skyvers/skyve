package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.widget.bound.input.Slider;

public class FluentSlider extends FluentChangeableInputWidget<FluentSlider>
		implements FluentAbsoluteSize<FluentSlider>, FluentConstrainableHeight<FluentSlider> {
	private Slider slider = null;

	public FluentSlider() {
		slider = new Slider();
	}

	public FluentSlider(Slider slider) {
		this.slider = slider;
	}

	public FluentSlider from(@SuppressWarnings("hiding") Slider slider) {
		min(slider.getMin());
		max(slider.getMax());
		numberOfDiscreteValues(slider.getNumberOfDiscreteValues());
		roundingPrecision(slider.getRoundingPrecision());
		vertical(slider.getVertical());

		absoluteSize(slider, this);

		constrainableHeight(slider, this);

		super.from(slider);
		return this;
	}

	public FluentSlider min(double min) {
		slider.setMin(Double.valueOf(min));
		return this;
	}

	public FluentSlider max(double max) {
		slider.setMax(Double.valueOf(max));
		return this;
	}

	public FluentSlider numberOfDiscreteValues(int numberOfDiscreteValues) {
		slider.setNumberOfDiscreteValues(Integer.valueOf(numberOfDiscreteValues));
		return this;
	}

	public FluentSlider roundingPrecision(int roundingPrecision) {
		slider.setRoundingPrecision(Integer.valueOf(roundingPrecision));
		return this;
	}

	public FluentSlider vertical(boolean vertical) {
		slider.setVertical(vertical ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	@Override
	public FluentSlider pixelWidth(int width) {
		slider.setPixelWidth(Integer.valueOf(width));
		return this;
	}

	@Override
	public FluentSlider pixelHeight(int height) {
		slider.setPixelHeight(Integer.valueOf(height));
		return this;
	}

	@Override
	public FluentSlider minPixelHeight(int minPixelHeight) {
		slider.setMinPixelHeight(Integer.valueOf(minPixelHeight));
		return this;
	}

	@Override
	public FluentSlider maxPixelHeight(int maxPixelHeight) {
		slider.setMaxPixelHeight(Integer.valueOf(maxPixelHeight));
		return this;
	}

	@Override
	public Slider get() {
		return slider;
	}
}
