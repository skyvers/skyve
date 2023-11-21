package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.HorizontalAlignment;
import org.skyve.impl.metadata.view.VerticalAlignment;
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
		i = sidebar.getFloatingPercentageWidth();
		if (i != null) {
			this.sidebar.setFloatingPercentageWidth(i);
		}
		i = sidebar.getFloatingResponsiveWidth();
		if (i != null) {
			this.sidebar.setFloatingResponsiveWidth(i);
		}

		this.sidebar.setTitle(sidebar.getTitle());
		
		horizontalAlignment(sidebar.getHorizontalAlignment());
		verticalAlignment(sidebar.getVerticalAlignment());

		invisibleConditionName(sidebar.getInvisibleConditionName());
		
		super.from(sidebar);
		
		return this;
	}
	
	public FluentSidebar widgetId(String widgetId) {
		sidebar.setWidgetId(widgetId);
		return this;
	}
	
	public FluentSidebar title(String title) {
		sidebar.setTitle(title);
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

	public FluentSidebar floatingResponsiveWidth(int responsiveWidth) {
		sidebar.setFloatingResponsiveWidth(Integer.valueOf(responsiveWidth));
		return this;
	}

	public FluentSidebar floatingPercentageWidth(int percentageWidth) {
		sidebar.setFloatingPercentageWidth(Integer.valueOf(percentageWidth));
		return this;
	}

	public FluentSidebar horizontalAlignment(HorizontalAlignment horizontalAlignment) {
		sidebar.setHorizontalAlignment(horizontalAlignment);
		return this;
	}

	public FluentSidebar verticalAlignment(VerticalAlignment verticalAlignment) {
		sidebar.setVerticalAlignment(verticalAlignment);
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
