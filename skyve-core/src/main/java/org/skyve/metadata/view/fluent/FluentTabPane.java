package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.container.TabPane;

/**
 * Fluent builder for tab-pane metadata.
 */
public class FluentTabPane extends FluentWidget implements FluentRelativeSize<FluentTabPane> {
	private TabPane pane = null;
	
	/**
	 * Creates a builder backed by a new {@link TabPane} metadata instance.
	 */
	public FluentTabPane() {
		this.pane = new TabPane();
	}
	
	/**
	 * Creates a builder backed by the supplied tab-pane metadata instance.
	 *
	 * @param tabPane
	 *            the tab-pane metadata to mutate
	 */
	public FluentTabPane(TabPane tabPane) {
		pane = tabPane;
	}
	
	/**
	 * Copies tab-pane metadata into this builder.
	 *
	 * <p>Side effects: replaces size settings, tab selection binding, visibility conditions,
	 * and the tab list.
	 *
	 * @param tabPane
	 *            the source tab-pane metadata
	 * @return this builder
	 */
	public FluentTabPane from(TabPane tabPane) {
		widgetId(tabPane.getWidgetId());
		
		relativeSize(tabPane, this);
		
		disabledConditionName(tabPane.getDisabledConditionName());
		invisibleConditionName(tabPane.getInvisibleConditionName());
		
		selectedTabIndexBinding(tabPane.getSelectedTabIndexBinding());
		
		tabPane.getTabs().forEach(t -> addTab(new FluentTab().from(t)));
		
		return this;
	}
	
	/**
	 * Sets the widget id for this tab pane.
	 *
	 * @param widgetId
	 *            the widget identifier
	 * @return this builder
	 */
	public FluentTabPane widgetId(String widgetId) {
		pane.setWidgetId(widgetId);
		return this;
	}
	
	/**
	 * Sets tab-pane width in pixels.
	 *
	 * @param pixelWidth
	 *            width in pixels
	 * @return this builder
	 */
	@Override
	public FluentTabPane pixelWidth(int pixelWidth) {
		pane.setPixelWidth(Integer.valueOf(pixelWidth));
		return this;
	}

	/**
	 * Sets responsive width for this tab pane.
	 *
	 * @param responsiveWidth
	 *            responsive width value
	 * @return this builder
	 */
	@Override
	public FluentTabPane responsiveWidth(int responsiveWidth) {
		pane.setResponsiveWidth(Integer.valueOf(responsiveWidth));
		return this;
	}

	/**
	 * Sets the small breakpoint width.
	 *
	 * @param sm
	 *            small breakpoint width value
	 * @return this builder
	 */
	@Override
	public FluentTabPane sm(int sm) {
		pane.setSm(Integer.valueOf(sm));
		return this;
	}
	
	/**
	 * Sets the medium breakpoint width.
	 *
	 * @param md
	 *            medium breakpoint width value
	 * @return this builder
	 */
	@Override
	public FluentTabPane md(int md) {
		pane.setMd(Integer.valueOf(md));
		return this;
	}
	
	/**
	 * Sets the large breakpoint width.
	 *
	 * @param lg
	 *            large breakpoint width value
	 * @return this builder
	 */
	@Override
	public FluentTabPane lg(int lg) {
		pane.setLg(Integer.valueOf(lg));
		return this;
	}

	/**
	 * Sets the extra-large breakpoint width.
	 *
	 * @param xl
	 *            extra-large breakpoint width value
	 * @return this builder
	 */
	@Override
	public FluentTabPane xl(int xl) {
		pane.setXl(Integer.valueOf(xl));
		return this;
	}

	/**
	 * Sets tab-pane width as a percentage.
	 *
	 * @param percentageWidth
	 *            width percentage
	 * @return this builder
	 */
	@Override
	public FluentTabPane percentageWidth(int percentageWidth) {
		pane.setPercentageWidth(Integer.valueOf(percentageWidth));
		return this;
	}

	/**
	 * Sets the minimum tab-pane width in pixels.
	 *
	 * @param minPixelWidth
	 *            minimum width in pixels
	 * @return this builder
	 */
	@Override
	public FluentTabPane minPixelWidth(int minPixelWidth) {
		pane.setMinPixelWidth(Integer.valueOf(minPixelWidth));
		return this;
	}

	/**
	 * Sets the maximum tab-pane width in pixels.
	 *
	 * @param maxPixelWidth
	 *            maximum width in pixels
	 * @return this builder
	 */
	@Override
	public FluentTabPane maxPixelWidth(int maxPixelWidth) {
		pane.setMaxPixelWidth(Integer.valueOf(maxPixelWidth));
		return this;
	}

	/**
	 * Sets tab-pane height in pixels.
	 *
	 * @param pixelHeight
	 *            height in pixels
	 * @return this builder
	 */
	@Override
	public FluentTabPane pixelHeight(int pixelHeight) {
		pane.setPixelHeight(Integer.valueOf(pixelHeight));
		return this;
	}

	/**
	 * Sets tab-pane height as a percentage.
	 *
	 * @param percentageHeight
	 *            height percentage
	 * @return this builder
	 */
	@Override
	public FluentTabPane percentageHeight(int percentageHeight) {
		pane.setPercentageHeight(Integer.valueOf(percentageHeight));
		return this;
	}

	/**
	 * Sets the minimum tab-pane height in pixels.
	 *
	 * @param minPixelHeight
	 *            minimum height in pixels
	 * @return this builder
	 */
	@Override
	public FluentTabPane minPixelHeight(int minPixelHeight) {
		pane.setMinPixelHeight(Integer.valueOf(minPixelHeight));
		return this;
	}

	/**
	 * Sets the maximum tab-pane height in pixels.
	 *
	 * @param maxPixelHeight
	 *            maximum height in pixels
	 * @return this builder
	 */
	@Override
	public FluentTabPane maxPixelHeight(int maxPixelHeight) {
		pane.setMaxPixelHeight(Integer.valueOf(maxPixelHeight));
		return this;
	}
	
	/**
	 * Sets the condition name that disables this tab pane when true.
	 *
	 * @param disabledConditionName
	 *            disabled condition identifier
	 * @return this builder
	 */
	public FluentTabPane disabledConditionName(String disabledConditionName) {
		pane.setDisabledConditionName(disabledConditionName);
		return this;
	}

	/**
	 * Sets the condition name that hides this tab pane when true.
	 *
	 * @param invisibleConditionName
	 *            invisibility condition identifier
	 * @return this builder
	 */
	public FluentTabPane invisibleConditionName(String invisibleConditionName) {
		pane.setInvisibleConditionName(invisibleConditionName);
		return this;
	}
	
	/**
	 * Sets the binding that stores the selected tab index.
	 *
	 * @param selectedTabIndexBinding
	 *            binding expression for the selected tab index
	 * @return this builder
	 */
	public FluentTabPane selectedTabIndexBinding(String selectedTabIndexBinding) {
		pane.setSelectedTabIndexBinding(selectedTabIndexBinding);
		return this;
	}
	
	/**
	 * Appends a tab to this tab pane.
	 *
	 * @param tab
	 *            tab builder to append
	 * @return this builder
	 */
	public FluentTabPane addTab(FluentTab tab) {
		pane.getTabs().add(tab.get());
		return this;
	}

	/**
	 * Inserts a tab at the supplied index.
	 *
	 * @param index
	 *            zero-based insertion index
	 * @param tab
	 *            tab builder to insert
	 * @return this builder
	 */
	public FluentTabPane addTab(int index, FluentTab tab) {
		pane.getTabs().add(index, tab.get());
		return this;
	}
	
	/**
	 * Returns the tab at the supplied index.
	 *
	 * @param index
	 *            zero-based tab index
	 * @return the tab wrapper at {@code index}
	 */
	public FluentTab getTab(int index) {
		return new FluentTab(pane.getTabs().get(index));
	}

	/**
	 * Removes the tab at the supplied index.
	 *
	 * @param index
	 *            zero-based tab index
	 * @return this builder
	 */
	public FluentTabPane removeTab(int index) {
		pane.getTabs().remove(index);
		return this;
	}
	
	/**
	 * Removes all tabs from this tab pane.
	 *
	 * @return this builder
	 */
	public FluentTabPane clearTabs() {
		pane.getTabs().clear();
		return this;
	}
	
	/**
	 * Returns the backing metadata instance.
	 *
	 * @return the mutable tab-pane metadata
	 */
	@Override
	public TabPane get() {
		return pane;
	}
}
