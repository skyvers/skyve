package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.widget.bound.ProgressBar;

public class FluentProgressBar extends FluentBoundWidget<FluentProgressBar>
		implements FluentAbsoluteSize<FluentProgressBar>, FluentConstrainableSize<FluentProgressBar> {
	private ProgressBar bar = null;
	
	public FluentProgressBar() {
		bar = new ProgressBar();
	}
	
	public FluentProgressBar(ProgressBar bar) {
		this.bar = bar;
	}

	public FluentProgressBar from(@SuppressWarnings("hiding") ProgressBar bar) {

		invisibleConditionName(bar.getInvisibleConditionName());

		absoluteSize(bar, this);
		constrainableSize(bar, this);

		super.from(bar);
		return this;
	}
	
	public FluentProgressBar invisibleConditionName(String invisibleConditionName) {
		bar.setInvisibleConditionName(invisibleConditionName);
		return this;
	}

	@Override
	public FluentProgressBar pixelWidth(int width) {
		bar.setPixelWidth(Integer.valueOf(width));
		return this;
	}

	@Override
	public FluentProgressBar pixelHeight(int height) {
		bar.setPixelHeight(Integer.valueOf(height));
		return this;
	}

	@Override
	public FluentProgressBar maxPixelHeight(int maxPixelHeight) {
		bar.setMaxPixelHeight(Integer.valueOf(maxPixelHeight));
		return this;
	}

	@Override
	public FluentProgressBar minPixelHeight(int minPixelHeight) {
		bar.setMinPixelHeight(Integer.valueOf(minPixelHeight));
		return this;
	}

	@Override
	public FluentProgressBar minPixelWidth(int minPixelWidth) {
		bar.setMinPixelWidth(Integer.valueOf(minPixelWidth));
		return this;
	}

	@Override
	public FluentProgressBar maxPixelWidth(int maxPixelWidth) {
		bar.setMaxPixelWidth(Integer.valueOf(maxPixelWidth));
		return this;
	}

	@Override
	public ProgressBar get() {
		return bar;
	}
}
