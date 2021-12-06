package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.widget.Spacer;

public class FluentSpacer extends FluentWidget implements FluentAbsoluteSize<FluentSpacer> {
	private Spacer spacer = null;

	public FluentSpacer() {
		spacer = new Spacer();
	}

	public FluentSpacer(Spacer spacer) {
		this.spacer = spacer;
	}

	public FluentSpacer from(@SuppressWarnings("hiding") Spacer spacer) {

		invisibleConditionName(spacer.getInvisibleConditionName());

		absoluteSize(spacer, this);

		return this;
	}

	public FluentSpacer invisibleConditionName(String invisibleConditionName) {
		spacer.setInvisibleConditionName(invisibleConditionName);
		return this;
	}

	@Override
	public FluentSpacer pixelWidth(int width) {
		spacer.setPixelWidth(Integer.valueOf(width));
		return this;
	}

	@Override
	public FluentSpacer pixelHeight(int height) {
		spacer.setPixelHeight(Integer.valueOf(height));
		return this;
	}

	@Override
	public Spacer get() {
		return spacer;
	}
}
