package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.container.Sidebar;

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
	public FluentSidebar(Sidebar sidebar) {
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
	public FluentSidebar from(@SuppressWarnings("hiding") Sidebar sidebar) {
		widgetId(sidebar.getWidgetId());
		relativeWidth(sidebar, this);

		Integer i = sidebar.getFloatingPixelWidthBreakpoint();
		if (i != null) {
			this.sidebar.setFloatingPixelWidthBreakpoint(i);
		}
		i = sidebar.getFloatingPixelWidth();
		if (i != null) {
			this.sidebar.setFloatingPixelWidth(i);
		}
		
		invisibleConditionName(sidebar.getInvisibleConditionName());
		
		super.from(sidebar);
		
		return this;
	}
	
	/**
	 * Sets the widget id for this sidebar.
	 *
	 * @param widgetId
	 *            the widget identifier
	 * @return this builder
	 */
	public FluentSidebar widgetId(String widgetId) {
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
	public FluentSidebar pixelWidth(int pixelWidth) {
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
	public FluentSidebar responsiveWidth(int responsiveWidth) {
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
	public FluentSidebar percentageWidth(int percentageWidth) {
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
	public FluentSidebar floatingPixelWidthBreakpoint(int pixelWidth) {
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
	public FluentSidebar floatingPixelWidth(int pixelWidth) {
		sidebar.setFloatingPixelWidth(Integer.valueOf(pixelWidth));
		return this;
	}

	/**
	 * Sets the condition name that hides this sidebar when it evaluates to true.
	 *
	 * @param invisibleConditionName
	 *            the invisibility condition identifier
	 * @return this builder
	 */
	public FluentSidebar invisibleConditionName(String invisibleConditionName) {
		sidebar.setInvisibleConditionName(invisibleConditionName);
		return this;
	}

	/**
	 * Returns the backing metadata instance.
	 *
	 * @return the mutable sidebar metadata
	 */
	@Override
	public Sidebar get() {
		return sidebar;
	}
}
