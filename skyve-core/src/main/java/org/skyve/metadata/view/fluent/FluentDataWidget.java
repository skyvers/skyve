package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.widget.bound.tabular.AbstractDataWidget;

abstract class FluentDataWidget<T extends FluentDataWidget<T>> extends FluentBound<T> implements FluentRelativeSize<T> {
	protected FluentDataWidget() {
		// nothing to see
	}
	
	@SuppressWarnings("unchecked")
	protected T from(AbstractDataWidget data) {
		widgetId(data.getWidgetId());
		title(data.getTitle());
		relativeSize(data, this);
		invisibleConditionName(data.getInvisibleConditionName());
		return (T) this;
	}
	
	@SuppressWarnings("unchecked")
	public T widgetId(String widgetId) {
		get().setWidgetId(widgetId);
		return (T) this;
	}

	@SuppressWarnings("unchecked")
	public T title(String title) {
		get().setTitle(title);
		return (T) this;
	}
	
	@Override
	@SuppressWarnings("unchecked")
	public T pixelWidth(int pixelWidth) {
		get().setPixelWidth(Integer.valueOf(pixelWidth));
		return (T) this;
	}
	
	@Override
	@SuppressWarnings("unchecked")
	public T responsiveWidth(int responsiveWidth) {
		get().setResponsiveWidth(Integer.valueOf(responsiveWidth));
		return (T) this;
	}

	@Override
	@SuppressWarnings("unchecked")
	public T sm(int sm) {
		get().setSm(Integer.valueOf(sm));
		return (T) this;
	}
	
	@Override
	@SuppressWarnings("unchecked")
	public T md(int md) {
		get().setMd(Integer.valueOf(md));
		return (T) this;
	}

	@Override
	@SuppressWarnings("unchecked")
	public T lg(int lg) {
		get().setLg(Integer.valueOf(lg));
		return (T) this;
	}

	@Override
	@SuppressWarnings("unchecked")
	public T xl(int xl) {
		get().setXl(Integer.valueOf(xl));
		return (T) this;
	}

	@Override
	@SuppressWarnings("unchecked")
	public T percentageWidth(int percentageWidth) {
		get().setPercentageWidth(Integer.valueOf(percentageWidth));
		return (T) this;
	}
	
	@Override
	@SuppressWarnings("unchecked")
	public T minPixelWidth(int minPixelWidth) {
		get().setMinPixelWidth(Integer.valueOf(minPixelWidth));
		return (T) this;
	}
	
	@Override
	@SuppressWarnings("unchecked")
	public T maxPixelWidth(int maxPixelWidth) {
		get().setMaxPixelWidth(Integer.valueOf(maxPixelWidth));
		return (T) this;
	}

	@Override
	@SuppressWarnings("unchecked")
	public T pixelHeight(int pixelHeight) {
		get().setPixelHeight(Integer.valueOf(pixelHeight));
		return (T) this;
	}
	
	@Override
	@SuppressWarnings("unchecked")
	public T percentageHeight(int percentageHeight) {
		get().setPercentageHeight(Integer.valueOf(percentageHeight));
		return (T) this;
	}
	
	@Override
	@SuppressWarnings("unchecked")
	public T minPixelHeight(int minPixelHeight) {
		get().setMinPixelHeight(Integer.valueOf(minPixelHeight));
		return (T) this;
	}
	
	@Override
	@SuppressWarnings("unchecked")
	public T maxPixelHeight(int maxPixelHeight) {
		get().setMaxPixelHeight(Integer.valueOf(maxPixelHeight));
		return (T) this;
	}
	
	@SuppressWarnings("unchecked")
	public T invisibleConditionName(String invisibleConditionName) {
		get().setInvisibleConditionName(invisibleConditionName);
		return (T) this;
	}

	@Override
	public abstract AbstractDataWidget get();
}
