package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.container.Tab;

public class FluentTab extends FluentContainer<FluentTab> {
	private Tab tab = null;

	public FluentTab() {
		this.tab = new Tab();
	}
	
	public FluentTab(Tab tab) {
		this.tab = tab;
	}

	public FluentTab from(@SuppressWarnings("hiding") Tab tab) {
		title(tab.getTitle());
		icon16x16RelativeFileName(tab.getIcon16x16RelativeFileName());
		iconStyleClass(tab.getIconStyleClass());
		disabledConditionName(tab.getDisabledConditionName());
		invisibleConditionName(tab.getInvisibleConditionName());
		
		super.from(tab);

		return this;
	}
	
	public FluentTab title(String title) {
		tab.setTitle(title);
		return this;
	}

	public FluentTab icon16x16RelativeFileName(String icon16x16RelativeFileName) {
		tab.setIcon16x16RelativeFileName(icon16x16RelativeFileName);
		return this;
	}

	public FluentTab iconStyleClass(String iconStyleClass) {
		tab.setIconStyleClass(iconStyleClass);
		return this;
	}

	public FluentTab disabledConditionName(String disabledConditionName) {
		tab.setDisabledConditionName(disabledConditionName);
		return this;
	}

	public FluentTab invisibleConditionName(String invisibleConditionName) {
		tab.setInvisibleConditionName(invisibleConditionName);
		return this;
	}

	@Override
	public Tab get() {
		return tab;
	}
}
