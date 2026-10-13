package org.skyve.metadata.view.fluent;

import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.view.widget.bound.tabular.AbstractDataWidget;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

public abstract class FluentDataWidget<T extends FluentDataWidget<T>> extends FluentBoundWidget<T> implements FluentRelativeSize<T> {
	protected FluentDataWidget() {
		// nothing to see
	}
	
	@SuppressWarnings("unchecked")
	protected @Nonnull T from(@Nonnull AbstractDataWidget data) {
		super.from(data);
		widgetId(data.getWidgetId());
		title(data.getTitle());
		escapeTitle(data.getEscapeTitle());
		relativeSize(data, this);
		BindUtil.copyAuthoredCondition(data.getXmlInvisibleCondition(), data.getXmlVisibleCondition(), this::invisibleCondition, this::visibleCondition);
		return (T) this;
	}
	
	@SuppressWarnings("unchecked")
	public @Nonnull T widgetId(@Nullable String widgetId) {
		get().setWidgetId(widgetId);
		return (T) this;
	}

	@SuppressWarnings("unchecked")
	public @Nonnull T title(@Nullable String title) {
		get().setTitle(title);
		return (T) this;
	}

	/**
	 * Sets whether the data widget title should be escaped before rendering.
	 *
	 * @param escapeTitle {@code false} to allow trusted markup; {@code true} to escape at the renderer boundary
	 * @return this builder
	 */
	public @Nonnull T escapeTitle(boolean escapeTitle) {
		return escapeTitle(escapeTitle ? Boolean.TRUE : Boolean.FALSE);
	}

	/**
	 * Sets whether the data widget title should be escaped before rendering.
	 *
	 * @param escapeTitle {@code Boolean.FALSE} to allow trusted markup; {@code null} or {@code Boolean.TRUE} to escape at the renderer boundary
	 * @return this builder
	 */
	@SuppressWarnings("unchecked")
	public @Nonnull T escapeTitle(@Nullable Boolean escapeTitle) {
		get().setEscapeTitle(escapeTitle);
		return (T) this;
	}
	
	@Override
	@SuppressWarnings("unchecked")
	public @Nonnull T pixelWidth(int pixelWidth) {
		get().setPixelWidth(Integer.valueOf(pixelWidth));
		return (T) this;
	}
	
	@Override
	@SuppressWarnings("unchecked")
	public @Nonnull T responsiveWidth(int responsiveWidth) {
		get().setResponsiveWidth(Integer.valueOf(responsiveWidth));
		return (T) this;
	}

	@Override
	@SuppressWarnings("unchecked")
	public @Nonnull T sm(int sm) {
		get().setSm(Integer.valueOf(sm));
		return (T) this;
	}
	
	@Override
	@SuppressWarnings("unchecked")
	public @Nonnull T md(int md) {
		get().setMd(Integer.valueOf(md));
		return (T) this;
	}

	@Override
	@SuppressWarnings("unchecked")
	public @Nonnull T lg(int lg) {
		get().setLg(Integer.valueOf(lg));
		return (T) this;
	}

	@Override
	@SuppressWarnings("unchecked")
	public @Nonnull T xl(int xl) {
		get().setXl(Integer.valueOf(xl));
		return (T) this;
	}

	@Override
	@SuppressWarnings("unchecked")
	public @Nonnull T percentageWidth(int percentageWidth) {
		get().setPercentageWidth(Integer.valueOf(percentageWidth));
		return (T) this;
	}
	
	@Override
	@SuppressWarnings("unchecked")
	public @Nonnull T minPixelWidth(int minPixelWidth) {
		get().setMinPixelWidth(Integer.valueOf(minPixelWidth));
		return (T) this;
	}
	
	@Override
	@SuppressWarnings("unchecked")
	public @Nonnull T maxPixelWidth(int maxPixelWidth) {
		get().setMaxPixelWidth(Integer.valueOf(maxPixelWidth));
		return (T) this;
	}

	@Override
	@SuppressWarnings("unchecked")
	public @Nonnull T pixelHeight(int pixelHeight) {
		get().setPixelHeight(Integer.valueOf(pixelHeight));
		return (T) this;
	}
	
	@Override
	@SuppressWarnings("unchecked")
	public @Nonnull T percentageHeight(int percentageHeight) {
		get().setPercentageHeight(Integer.valueOf(percentageHeight));
		return (T) this;
	}
	
	@Override
	@SuppressWarnings("unchecked")
	public @Nonnull T minPixelHeight(int minPixelHeight) {
		get().setMinPixelHeight(Integer.valueOf(minPixelHeight));
		return (T) this;
	}
	
	@Override
	@SuppressWarnings("unchecked")
	public @Nonnull T maxPixelHeight(int maxPixelHeight) {
		get().setMaxPixelHeight(Integer.valueOf(maxPixelHeight));
		return (T) this;
	}
	
	/**
	 * Sets the {@code invisible} condition as authored; the opposite sense of {@link #visibleCondition(String)}.
	 *
	 * @param invisibleCondition
	 *            a condition name, an inline condition expression or a boolean literal
	 * @return this builder
	 */
	@SuppressWarnings("unchecked")
	public @Nonnull T invisibleCondition(@Nullable String invisibleCondition) {
		get().setXmlInvisibleCondition(invisibleCondition);
		return (T) this;
	}

	/**
	 * Sets the {@code visible} condition as authored; the opposite sense of {@link #invisibleCondition(String)}.
	 *
	 * @param visibleCondition
	 *            a condition name, an inline condition expression or a boolean literal
	 * @return this builder
	 */
	@SuppressWarnings("unchecked")
	public @Nonnull T visibleCondition(@Nullable String visibleCondition) {
		get().setXmlVisibleCondition(visibleCondition);
		return (T) this;
	}

	@Override
	public abstract @Nonnull AbstractDataWidget get();
}
