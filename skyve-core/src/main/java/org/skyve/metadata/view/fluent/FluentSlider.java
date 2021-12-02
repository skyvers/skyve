package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.widget.bound.input.Slider;

public class FluentSlider extends FluentChangeableInputWidget<FluentSlider> {
	private Slider slider = null;
	
	public FluentSlider() {
		slider = new Slider();
	}

	public FluentSlider(Slider slider) {
		this.slider = slider;
	}

	public FluentSlider from(@SuppressWarnings("hiding") Slider slider) {
		super.from(slider);
		return this;
	}

	@Override
	public Slider get() {
		return slider;
	}
}
