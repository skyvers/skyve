package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.widget.bound.ZoomIn;
import org.skyve.metadata.view.Action.ActionShow;

public class FluentZoomIn extends FluentBoundWidget<FluentZoomIn>
		implements FluentAbsoluteSize<FluentZoomIn>, FluentConstrainableHeight<FluentZoomIn> {
	private ZoomIn zoom = null;
	
	public FluentZoomIn() {
		zoom = new ZoomIn();
	}
	
	public FluentZoomIn(ZoomIn zoom) {
		this.zoom = zoom;
	}

	public FluentZoomIn from(@SuppressWarnings("hiding") ZoomIn zoom) {
		displayName(zoom.getDisplayName());
		relativeIconFileName(zoom.getRelativeIconFileName());
		iconStyleClass(zoom.getIconStyleClass());
		toolTip(zoom.getToolTip());
		disabledConditionName(zoom.getDisabledConditionName());
		invisibleConditionName(zoom.getInvisibleConditionName());
		show(zoom.getShow());
		absoluteSize(zoom, this);
		constrainableHeight(zoom, this);

		super.from(zoom);
		return this;
	}

	public FluentZoomIn displayName(String displayName) {
		zoom.setDisplayName(displayName);
		return this;
	}

	public FluentZoomIn relativeIconFileName(String relativeIconFileName) {
		zoom.setRelativeIconFileName(relativeIconFileName);
		return this;
	}

	public FluentZoomIn iconStyleClass(String iconStyleClass) {
		zoom.setIconStyleClass(iconStyleClass);
		return this;
	}

	public FluentZoomIn toolTip(String toolTip) {
		zoom.setToolTip(toolTip);
		return this;
	}

	public FluentZoomIn disabledConditionName(String disabledConditionName) {
		zoom.setDisabledConditionName(disabledConditionName);
		return this;
	}

	public FluentZoomIn invisibleConditionName(String invisibleConditionName) {
		zoom.setInvisibleConditionName(invisibleConditionName);
		return this;
	}

	public FluentZoomIn show(ActionShow show) {
		zoom.setShow(show);
		return this;
	}

	@Override
	public FluentZoomIn pixelWidth(int width) {
		zoom.setPixelWidth(Integer.valueOf(width));
		return this;
	}

	@Override
	public FluentZoomIn pixelHeight(int height) {
		zoom.setPixelHeight(Integer.valueOf(height));
		return this;
	}

	@Override
	public FluentZoomIn minPixelHeight(int minPixelHeight) {
		zoom.setMinPixelHeight(Integer.valueOf(minPixelHeight));
		return this;
	}

	@Override
	public FluentZoomIn maxPixelHeight(int maxPixelHeight) {
		zoom.setMaxPixelHeight(Integer.valueOf(maxPixelHeight));
		return this;
	}

	@Override
	public ZoomIn get() {
		return zoom;
	}
}
