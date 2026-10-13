package org.skyve.metadata.view.fluent;

import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.view.container.Tab;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

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
		escapeTitle(tab.getEscapeTitle());
		icon16x16RelativeFileName(tab.getIcon16x16RelativeFileName());
		iconStyleClass(tab.getIconStyleClass());
		BindUtil.copyAuthoredCondition(tab.getXmlDisabledCondition(), tab.getXmlEnabledCondition(), this::disabledCondition, this::enabledCondition);
		BindUtil.copyAuthoredCondition(tab.getXmlInvisibleCondition(), tab.getXmlVisibleCondition(), this::invisibleCondition, this::visibleCondition);
		super.from(tab);
		tab.getProperties().entrySet().forEach(p -> putProperty(p.getKey(), p.getValue()));

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
	 * Sets whether the tab title should be escaped before rendering.
	 *
	 * @param escapeTitle {@code false} to allow trusted markup; {@code true} to escape at the renderer boundary
	 * @return this builder
	 */
	public FluentTab escapeTitle(boolean escapeTitle) {
		return escapeTitle(escapeTitle ? Boolean.TRUE : Boolean.FALSE);
	}

	/**
	 * Sets whether the tab title should be escaped before rendering.
	 *
	 * @param escapeTitle {@code Boolean.FALSE} to allow trusted markup; {@code null} or {@code Boolean.TRUE} to escape at the renderer boundary
	 * @return this builder
	 */
	public FluentTab escapeTitle(Boolean escapeTitle) {
		tab.setEscapeTitle(escapeTitle);
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
	 * Sets the {@code disabled} condition as authored; the opposite sense of {@link #enabledCondition(String)}.
	 *
	 * @param disabledCondition
	 *            a condition name, an inline condition expression or a boolean literal
	 * @return this builder
	 */
	public @Nonnull FluentTab disabledCondition(@Nullable String disabledCondition) {
		tab.setXmlDisabledCondition(disabledCondition);
		return this;
	}

	/**
	 * Sets the {@code enabled} condition as authored; the opposite sense of {@link #disabledCondition(String)}.
	 *
	 * @param enabledCondition
	 *            a condition name, an inline condition expression or a boolean literal
	 * @return this builder
	 */
	public @Nonnull FluentTab enabledCondition(@Nullable String enabledCondition) {
		tab.setXmlEnabledCondition(enabledCondition);
		return this;
	}

	/**
	 * Sets the {@code invisible} condition as authored; the opposite sense of {@link #visibleCondition(String)}.
	 *
	 * @param invisibleCondition
	 *            a condition name, an inline condition expression or a boolean literal
	 * @return this builder
	 */
	public @Nonnull FluentTab invisibleCondition(@Nullable String invisibleCondition) {
		tab.setXmlInvisibleCondition(invisibleCondition);
		return this;
	}

	/**
	 * Sets the {@code visible} condition as authored; the opposite sense of {@link #invisibleCondition(String)}.
	 *
	 * @param visibleCondition
	 *            a condition name, an inline condition expression or a boolean literal
	 * @return this builder
	 */
	public @Nonnull FluentTab visibleCondition(@Nullable String visibleCondition) {
		tab.setXmlVisibleCondition(visibleCondition);
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
	public @Nonnull FluentTab putProperty(@Nonnull String k, @Nonnull String v) {
		tab.getProperties().put(k, v);
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
