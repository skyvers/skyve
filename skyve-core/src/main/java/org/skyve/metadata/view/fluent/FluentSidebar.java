package org.skyve.metadata.view.fluent;

import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.view.container.Sidebar;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Fluent builder for sidebar container metadata.
 */
public class FluentSidebar extends FluentContainer<FluentSidebar> implements FluentRelativeWidth<FluentSidebar> {
	private Sidebar sidebar = null;
	
	/**
	 * Creates a builder backed by a new {@link Sidebar} metadata instance.
	 */
	public FluentSidebar() {
		this.sidebar = new Sidebar();
	}
	
	/**
	 * Creates a builder backed by the supplied sidebar metadata instance.
	 *
	 * @param sidebar
	 *            the sidebar metadata to mutate
	 */
	public FluentSidebar(@Nonnull Sidebar sidebar) {
		this.sidebar = sidebar;
	}
	
	/**
	 * Copies sidebar metadata into this builder.
	 *
	 * <p>Side effects: replaces sizing, floating-width, visibility condition, and contained widgets.
	 *
	 * @param sidebar
	 *            the source sidebar metadata
	 * @return this builder
	 */
	public @Nonnull FluentSidebar from(@SuppressWarnings("hiding") @Nonnull Sidebar sidebar) {
		widgetId(sidebar.getWidgetId());
		relativeWidth(sidebar, this);

		Integer i = sidebar.getFloatingPixelWidthBreakpoint();
		this.sidebar.setFloatingPixelWidthBreakpoint(i);
		i = sidebar.getFloatingPixelWidth();
		if (i != null) {
			this.sidebar.setFloatingPixelWidth(i);
		}
		
		BindUtil.copyAuthoredCondition(sidebar.getXmlInvisibleCondition(), sidebar.getXmlVisibleCondition(), this::invisibleCondition, this::visibleCondition);
		
		super.from(sidebar);
		
		sidebar.getProperties().entrySet().forEach(p -> putProperty(p.getKey(), p.getValue()));

		return this;
	}
	
	/**
	 * Sets the widget id for this sidebar.
	 *
	 * @param widgetId
	 *            the widget identifier
	 * @return this builder
	 */
	public @Nonnull FluentSidebar widgetId(@Nullable String widgetId) {
		sidebar.setWidgetId(widgetId);
		return this;
	}
	
	/**
	 * Sets sidebar width in pixels.
	 *
	 * @param pixelWidth
	 *            the width in pixels
	 * @return this builder
	 */
	@Override
	public @Nonnull FluentSidebar pixelWidth(int pixelWidth) {
		sidebar.setPixelWidth(Integer.valueOf(pixelWidth));
		return this;
	}

	/**
	 * Sets sidebar responsive width.
	 *
	 * @param responsiveWidth
	 *            the responsive width value
	 * @return this builder
	 */
	@Override
	public @Nonnull FluentSidebar responsiveWidth(int responsiveWidth) {
		sidebar.setResponsiveWidth(Integer.valueOf(responsiveWidth));
		return this;
	}

	/**
	 * Sets sidebar width as a percentage.
	 *
	 * @param percentageWidth
	 *            the width percentage
	 * @return this builder
	 */
	@Override
	public @Nonnull FluentSidebar percentageWidth(int percentageWidth) {
		sidebar.setPercentageWidth(Integer.valueOf(percentageWidth));
		return this;
	}

	/**
	 * Sets the viewport breakpoint below which the sidebar floats.
	 *
	 * @param pixelWidth
	 *            the breakpoint width in pixels
	 * @return this builder
	 */
	public @Nonnull FluentSidebar floatingPixelWidthBreakpoint(int pixelWidth) {
		sidebar.setFloatingPixelWidthBreakpoint(Integer.valueOf(pixelWidth));
		return this;
	}

	/**
	 * Sets the pixel width used when the sidebar is floating.
	 *
	 * @param pixelWidth
	 *            the floating width in pixels
	 * @return this builder
	 */
	public @Nonnull FluentSidebar floatingPixelWidth(int pixelWidth) {
		sidebar.setFloatingPixelWidth(Integer.valueOf(pixelWidth));
		return this;
	}

	/**
	 * Sets the {@code invisible} condition as authored; the opposite sense of {@link #visibleCondition(String)}.
	 *
	 * @param invisibleCondition
	 *            a condition name, an inline condition expression or a boolean literal
	 * @return this builder
	 */
	public @Nonnull FluentSidebar invisibleCondition(@Nullable String invisibleCondition) {
		sidebar.setXmlInvisibleCondition(invisibleCondition);
		return this;
	}

	/**
	 * Sets the {@code visible} condition as authored; the opposite sense of {@link #invisibleCondition(String)}.
	 *
	 * @param visibleCondition
	 *            a condition name, an inline condition expression or a boolean literal
	 * @return this builder
	 */
	public @Nonnull FluentSidebar visibleCondition(@Nullable String visibleCondition) {
		sidebar.setXmlVisibleCondition(visibleCondition);
		return this;
	}

	/**
	 * Adds or replaces a custom property on the wrapped metadata.
	 * Properties keep the order in which they were first added.
	 *
	 * @param k the property name
	 * @param v the property value
	 * @return this builder
	 */
	public @Nonnull FluentSidebar putProperty(@Nonnull String k, @Nonnull String v) {
		sidebar.getProperties().put(k, v);
		return this;
	}

	/**
	 * Returns the backing metadata instance.
	 *
	 * @return the mutable sidebar metadata
	 */
	@Override
	public @Nonnull Sidebar get() {
		return sidebar;
	}
}
