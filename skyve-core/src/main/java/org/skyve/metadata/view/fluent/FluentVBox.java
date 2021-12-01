package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.HorizontalAlignment;
import org.skyve.impl.metadata.view.ShrinkWrap;
import org.skyve.impl.metadata.view.VerticalAlignment;
import org.skyve.impl.metadata.view.container.VBox;

public class FluentVBox extends FluentContainer<FluentVBox> implements FluentBox<FluentVBox> {
	private VBox box = null;
	
	public FluentVBox() {
		this.box = new VBox();
	}
	
	public FluentVBox(VBox box) {
		this.box = box;
	}
	
	public FluentVBox from(@SuppressWarnings("hiding") VBox box) {
		widgetId(box.getWidgetId());
		border(Boolean.TRUE.equals(box.getBorder()));
		borderTitle(box.getBorderTitle());
		
		box(box, this);
		
		horizontalAlignment(box.getHorizontalAlignment());
		verticalAlignment(box.getVerticalAlignment());

		invisibleConditionName(box.getInvisibleConditionName());
		
		super.from(box);
		
		return this;
	}
	
	public FluentVBox widgetId(String widgetId) {
		box.setWidgetId(widgetId);
		return this;
	}
	
	public FluentVBox border(boolean border) {
		box.setBorder(border ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}
	
	public FluentVBox borderTitle(String borderTitle) {
		box.setBorderTitle(borderTitle);
		return this;
	}
	
	@Override
	public FluentVBox pixelWidth(int pixelWidth) {
		box.setPixelWidth(Integer.valueOf(pixelWidth));
		return this;
	}

	@Override
	public FluentVBox responsiveWidth(int responsiveWidth) {
		box.setResponsiveWidth(Integer.valueOf(responsiveWidth));
		return this;
	}

	@Override
	public FluentVBox sm(int sm) {
		box.setSm(Integer.valueOf(sm));
		return this;
	}
	
	@Override
	public FluentVBox md(int md) {
		box.setMd(Integer.valueOf(md));
		return this;
	}
	
	@Override
	public FluentVBox lg(int lg) {
		box.setLg(Integer.valueOf(lg));
		return this;
	}

	@Override
	public FluentVBox xl(int xl) {
		box.setXl(Integer.valueOf(xl));
		return this;
	}

	@Override
	public FluentVBox percentageWidth(int percentageWidth) {
		box.setPercentageWidth(Integer.valueOf(percentageWidth));
		return this;
	}

	@Override
	public FluentVBox minPixelWidth(int minPixelWidth) {
		box.setMinPixelWidth(Integer.valueOf(minPixelWidth));
		return this;
	}

	@Override
	public FluentVBox maxPixelWidth(int maxPixelWidth) {
		box.setMaxPixelWidth(Integer.valueOf(maxPixelWidth));
		return this;
	}

	@Override
	public FluentVBox pixelHeight(int pixelHeight) {
		box.setPixelHeight(Integer.valueOf(pixelHeight));
		return this;
	}

	@Override
	public FluentVBox percentageHeight(int percentageHeight) {
		box.setPercentageHeight(Integer.valueOf(percentageHeight));
		return this;
	}

	@Override
	public FluentVBox minPixelHeight(int minPixelHeight) {
		box.setMinPixelHeight(Integer.valueOf(minPixelHeight));
		return this;
	}

	@Override
	public FluentVBox maxPixelHeight(int maxPixelHeight) {
		box.setMaxPixelHeight(Integer.valueOf(maxPixelHeight));
		return this;
	}
	
	@Override
	public FluentVBox pixelPadding(int pixelPadding) {
		box.setPixelPadding(Integer.valueOf(pixelPadding));
		return this;
	}

	@Override
	public FluentVBox pixelMemberPadding(int pixelMemberPadding) {
		box.setPixelMemberPadding(Integer.valueOf(pixelMemberPadding));
		return this;
	}
	
	@Override
	public FluentVBox shrinkWrap(ShrinkWrap shrinkWrap) {
		box.setShrinkWrap(shrinkWrap);
		return this;
	}

	public FluentVBox horizontalAlignment(HorizontalAlignment horizontalAlignment) {
		box.setHorizontalAlignment(horizontalAlignment);
		return this;
	}

	public FluentVBox verticalAlignment(VerticalAlignment verticalAlignment) {
		box.setVerticalAlignment(verticalAlignment);
		return this;
	}
	
	public FluentVBox invisibleConditionName(String invisibleConditionName) {
		box.setInvisibleConditionName(invisibleConditionName);
		return this;
	}

	@Override
	public VBox get() {
		return box;
	}
}
