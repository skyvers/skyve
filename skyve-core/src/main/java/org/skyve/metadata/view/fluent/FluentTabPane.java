package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.container.TabPane;

public class FluentTabPane extends FluentWidget implements FluentRelativeSize<FluentTabPane> {
	private TabPane pane = null;
	
	public FluentTabPane() {
		this.pane = new TabPane();
	}
	
	public FluentTabPane(TabPane tabPane) {
		pane = tabPane;
	}
	
	public FluentTabPane from(TabPane tabPane) {
		widgetId(tabPane.getWidgetId());
		
		relativeSize(tabPane, this);
		
		disabledConditionName(tabPane.getDisabledConditionName());
		invisibleConditionName(tabPane.getInvisibleConditionName());
		
		selectedTabIndexBinding(tabPane.getSelectedTabIndexBinding());
		
		tabPane.getTabs().forEach(t -> addTab(new FluentTab().from(t)));
		
		return this;
	}
	
	public FluentTabPane widgetId(String widgetId) {
		pane.setWidgetId(widgetId);
		return this;
	}
	
	@Override
	public FluentTabPane pixelWidth(int pixelWidth) {
		pane.setPixelWidth(Integer.valueOf(pixelWidth));
		return this;
	}

	@Override
	public FluentTabPane responsiveWidth(int responsiveWidth) {
		pane.setResponsiveWidth(Integer.valueOf(responsiveWidth));
		return this;
	}

	@Override
	public FluentTabPane sm(int sm) {
		pane.setSm(Integer.valueOf(sm));
		return this;
	}
	
	@Override
	public FluentTabPane md(int md) {
		pane.setMd(Integer.valueOf(md));
		return this;
	}
	
	@Override
	public FluentTabPane lg(int lg) {
		pane.setLg(Integer.valueOf(lg));
		return this;
	}

	@Override
	public FluentTabPane xl(int xl) {
		pane.setXl(Integer.valueOf(xl));
		return this;
	}

	@Override
	public FluentTabPane percentageWidth(int percentageWidth) {
		pane.setPercentageWidth(Integer.valueOf(percentageWidth));
		return this;
	}

	@Override
	public FluentTabPane minPixelWidth(int minPixelWidth) {
		pane.setMinPixelWidth(Integer.valueOf(minPixelWidth));
		return this;
	}

	@Override
	public FluentTabPane maxPixelWidth(int maxPixelWidth) {
		pane.setMaxPixelWidth(Integer.valueOf(maxPixelWidth));
		return this;
	}

	@Override
	public FluentTabPane pixelHeight(int pixelHeight) {
		pane.setPixelHeight(Integer.valueOf(pixelHeight));
		return this;
	}

	@Override
	public FluentTabPane percentageHeight(int percentageHeight) {
		pane.setPercentageHeight(Integer.valueOf(percentageHeight));
		return this;
	}

	@Override
	public FluentTabPane minPixelHeight(int minPixelHeight) {
		pane.setMinPixelHeight(Integer.valueOf(minPixelHeight));
		return this;
	}

	@Override
	public FluentTabPane maxPixelHeight(int maxPixelHeight) {
		pane.setMaxPixelHeight(Integer.valueOf(maxPixelHeight));
		return this;
	}
	
	public FluentTabPane disabledConditionName(String disabledConditionName) {
		pane.setDisabledConditionName(disabledConditionName);
		return this;
	}

	public FluentTabPane invisibleConditionName(String invisibleConditionName) {
		pane.setInvisibleConditionName(invisibleConditionName);
		return this;
	}
	
	public FluentTabPane selectedTabIndexBinding(String selectedTabIndexBinding) {
		pane.setSelectedTabIndexBinding(selectedTabIndexBinding);
		return this;
	}
	
	public FluentTabPane addTab(FluentTab tab) {
		pane.getTabs().add(tab.get());
		return this;
	}

	public FluentTabPane addTab(int index, FluentTab tab) {
		pane.getTabs().add(index, tab.get());
		return this;
	}
	
	public FluentTab getTab(int index) {
		return new FluentTab(pane.getTabs().get(index));
	}

	public FluentTabPane removeTab(int index) {
		pane.getTabs().remove(index);
		return this;
	}
	
	public FluentTabPane clearTabs() {
		pane.getTabs().clear();
		return this;
	}
	
	@Override
	public TabPane get() {
		return pane;
	}
}
