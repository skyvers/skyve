package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.container.Tab;

/**
 * Fluent builder for tab container metadata.
 */
public class FluentTab extends FluentContainer<FluentTab> {
	private Tab tab = null;

	/**
	 * Creates a builder backed by a new {@link Tab} metadata instance.
	 */
	public FluentTab() {
		this.tab = new Tab();
	}
	
	/**
	 * Creates a builder backed by the supplied tab metadata instance.
	 *
	 * @param tab
	 *            the tab metadata to mutate
	 */
	public FluentTab(Tab tab) {
		this.tab = tab;
	}

	/**
	 * Copies tab metadata into this builder.
	 *
	 * <p>Side effects: replaces tab caption, icon, conditions, and contained widgets.
	 *
	 * @param tab
	 *            the source tab metadata
	 * @return this builder
	 */
	public FluentTab from(@SuppressWarnings("hiding") Tab tab) {
		title(tab.getTitle());
		icon16x16RelativeFileName(tab.getIcon16x16RelativeFileName());
		iconStyleClass(tab.getIconStyleClass());
		disabledConditionName(tab.getDisabledConditionName());
		invisibleConditionName(tab.getInvisibleConditionName());
		
		super.from(tab);

		return this;
	}
	
	/**
	 * Sets the tab title.
	 *
	 * @param title
	 *            the caption displayed for this tab
	 * @return this builder
	 */
	public FluentTab title(String title) {
		tab.setTitle(title);
		return this;
	}

	/**
	 * Sets the relative path of the 16x16 icon used for this tab.
	 *
	 * @param icon16x16RelativeFileName
	 *            the icon resource path
	 * @return this builder
	 */
	public FluentTab icon16x16RelativeFileName(String icon16x16RelativeFileName) {
		tab.setIcon16x16RelativeFileName(icon16x16RelativeFileName);
		return this;
	}

	/**
	 * Sets the CSS class used to style the tab icon.
	 *
	 * @param iconStyleClass
	 *            the icon style class
	 * @return this builder
	 */
	public FluentTab iconStyleClass(String iconStyleClass) {
		tab.setIconStyleClass(iconStyleClass);
		return this;
	}

	/**
	 * Sets the condition name that disables this tab when it evaluates to true.
	 *
	 * @param disabledConditionName
	 *            the disabled condition identifier
	 * @return this builder
	 */
	public FluentTab disabledConditionName(String disabledConditionName) {
		tab.setDisabledConditionName(disabledConditionName);
		return this;
	}

	/**
	 * Sets the condition name that hides this tab when it evaluates to true.
	 *
	 * @param invisibleConditionName
	 *            the invisibility condition identifier
	 * @return this builder
	 */
	public FluentTab invisibleConditionName(String invisibleConditionName) {
		tab.setInvisibleConditionName(invisibleConditionName);
		return this;
	}

	/**
	 * Returns the backing metadata instance.
	 *
	 * @return the mutable tab metadata
	 */
	@Override
	public Tab get() {
		return tab;
	}
}
