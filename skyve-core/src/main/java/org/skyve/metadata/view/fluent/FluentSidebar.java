package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.container.Sidebar;

public class FluentSidebar extends FluentContainer<FluentSidebar> implements FluentRelativeWidth<FluentSidebar> {
	private Sidebar sidebar = null;
	
	public FluentSidebar() {
		this.sidebar = new Sidebar();
	}
	
	public FluentSidebar(Sidebar sidebar) {
		this.sidebar = sidebar;
	}
	
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
	
	public FluentSidebar widgetId(String widgetId) {
		sidebar.setWidgetId(widgetId);
		return this;
	}
	
	@Override
	public FluentSidebar pixelWidth(int pixelWidth) {
		sidebar.setPixelWidth(Integer.valueOf(pixelWidth));
		return this;
	}

	@Override
	public FluentSidebar responsiveWidth(int responsiveWidth) {
		sidebar.setResponsiveWidth(Integer.valueOf(responsiveWidth));
		return this;
	}

	@Override
	public FluentSidebar percentageWidth(int percentageWidth) {
		sidebar.setPercentageWidth(Integer.valueOf(percentageWidth));
		return this;
	}

	public FluentSidebar floatingPixelWidthBreakpoint(int pixelWidth) {
		sidebar.setFloatingPixelWidthBreakpoint(Integer.valueOf(pixelWidth));
		return this;
	}

	public FluentSidebar floatingPixelWidth(int pixelWidth) {
		sidebar.setFloatingPixelWidth(Integer.valueOf(pixelWidth));
		return this;
	}

	public FluentSidebar invisibleConditionName(String invisibleConditionName) {
		sidebar.setInvisibleConditionName(invisibleConditionName);
		return this;
	}

	@Override
	public Sidebar get() {
		return sidebar;
	}
}
