package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.HorizontalAlignment;
import org.skyve.impl.metadata.view.ShrinkWrap;
import org.skyve.impl.metadata.view.VerticalAlignment;
import org.skyve.impl.metadata.view.container.HBox;

public class FluentHBox extends FluentContainer<FluentHBox> implements FluentBox<FluentHBox> {
	private HBox box = null;
	
	public FluentHBox() {
		this.box = new HBox();
	}
	
	public FluentHBox(HBox box) {
		this.box = box;
	}
	
	public FluentHBox from(@SuppressWarnings("hiding") HBox box) {
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
	
	public FluentHBox widgetId(String widgetId) {
		box.setWidgetId(widgetId);
		return this;
	}
	
	public FluentHBox border(boolean border) {
		box.setBorder(border ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}
	
	public FluentHBox borderTitle(String borderTitle) {
		box.setBorderTitle(borderTitle);
		return this;
	}
	
	@Override
	public FluentHBox pixelWidth(int pixelWidth) {
		box.setPixelWidth(Integer.valueOf(pixelWidth));
		return this;
	}

	@Override
	public FluentHBox responsiveWidth(int responsiveWidth) {
		box.setResponsiveWidth(Integer.valueOf(responsiveWidth));
		return this;
	}

	@Override
	public FluentHBox sm(int sm) {
		box.setSm(Integer.valueOf(sm));
		return this;
	}
	
	@Override
	public FluentHBox md(int md) {
		box.setMd(Integer.valueOf(md));
		return this;
	}
	
	@Override
	public FluentHBox lg(int lg) {
		box.setLg(Integer.valueOf(lg));
		return this;
	}

	@Override
	public FluentHBox xl(int xl) {
		box.setXl(Integer.valueOf(xl));
		return this;
	}

	@Override
	public FluentHBox percentageWidth(int percentageWidth) {
		box.setPercentageWidth(Integer.valueOf(percentageWidth));
		return this;
	}

	@Override
	public FluentHBox minPixelWidth(int minPixelWidth) {
		box.setMinPixelWidth(Integer.valueOf(minPixelWidth));
		return this;
	}

	@Override
	public FluentHBox maxPixelWidth(int maxPixelWidth) {
		box.setMaxPixelWidth(Integer.valueOf(maxPixelWidth));
		return this;
	}

	@Override
	public FluentHBox pixelHeight(int pixelHeight) {
		box.setPixelHeight(Integer.valueOf(pixelHeight));
		return this;
	}

	@Override
	public FluentHBox percentageHeight(int percentageHeight) {
		box.setPercentageHeight(Integer.valueOf(percentageHeight));
		return this;
	}

	@Override
	public FluentHBox minPixelHeight(int minPixelHeight) {
		box.setMinPixelHeight(Integer.valueOf(minPixelHeight));
		return this;
	}

	@Override
	public FluentHBox maxPixelHeight(int maxPixelHeight) {
		box.setMaxPixelHeight(Integer.valueOf(maxPixelHeight));
		return this;
	}
	
	@Override
	public FluentHBox pixelPadding(int pixelPadding) {
		box.setPixelPadding(Integer.valueOf(pixelPadding));
		return this;
	}

	@Override
	public FluentHBox pixelMemberPadding(int pixelMemberPadding) {
		box.setPixelMemberPadding(Integer.valueOf(pixelMemberPadding));
		return this;
	}
	
	@Override
	public FluentHBox shrinkWrap(ShrinkWrap shrinkWrap) {
		box.setShrinkWrap(shrinkWrap);
		return this;
	}

	public FluentHBox horizontalAlignment(HorizontalAlignment horizontalAlignment) {
		box.setHorizontalAlignment(horizontalAlignment);
		return this;
	}

	public FluentHBox verticalAlignment(VerticalAlignment verticalAlignment) {
		box.setVerticalAlignment(verticalAlignment);
		return this;
	}
	
	public FluentHBox invisibleConditionName(String invisibleConditionName) {
		box.setInvisibleConditionName(invisibleConditionName);
		return this;
	}

	@Override
	public HBox get() {
		return box;
	}
}
