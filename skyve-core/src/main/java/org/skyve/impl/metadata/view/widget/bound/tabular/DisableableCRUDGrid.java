package org.skyve.impl.metadata.view.widget.bound.tabular;

import jakarta.annotation.Nullable;

/**
 * Mixin interface for CRUD grid widgets that support condition-based disabling
 * of the add, edit, zoom-in, and remove operations.
 *
 * <p>Each CRUD operation can be independently disabled by a condition: a condition name,
 * an inline condition expression, or a boolean literal. A view may author either the
 * {@code disable} or the {@code enable} attribute of each operation.
 *
 * <p>Naming pattern: {@code getXml...} and {@code setXml...} are the attributes exactly as authored,
 * which is what is marshalled; setting one attribute of a pair clears the other.
 * The un-prefixed {@code getDisable...Condition()} methods are the derived conditions that renderers
 * and other consumers read; each negates the {@code enable} attribute when that is the one authored.
 */
public interface DisableableCRUDGrid {

	/**
	 * Returns the derived {@code disableAdd} condition that renderers and other consumers read.
	 *
	 * @return the {@code disableAdd} attribute as authored, or else the negation of the authored
	 * 			{@code enableAdd} attribute; {@code null} when neither was authored
	 */
	public @Nullable String getDisableAddCondition();

	/**
	 * Returns the {@code disableAdd} attribute as authored.
	 *
	 * @return the authored condition, or {@code null} when {@code enableAdd} or neither was authored
	 */
	public @Nullable String getXmlDisableAddCondition();

	/**
	 * Sets the {@code disableAdd} attribute as authored.
	 *
	 * <p>Side effects: clears any authored {@code enableAdd} attribute.
	 *
	 * @param disableAddCondition a condition name, an inline condition expression or a boolean literal; may be {@code null}
	 */
	public void setXmlDisableAddCondition(@Nullable String disableAddCondition);

	/**
	 * Returns the {@code enableAdd} attribute as authored.
	 *
	 * @return the authored condition, or {@code null} when {@code disableAdd} or neither was authored
	 */
	public @Nullable String getXmlEnableAddCondition();

	/**
	 * Sets the {@code enableAdd} attribute as authored.
	 *
	 * <p>Side effects: clears any authored {@code disableAdd} attribute, so that
	 * {@link #getDisableAddCondition()} returns the negation of this condition.
	 *
	 * @param enableAddCondition a condition name, an inline condition expression or a boolean literal; may be {@code null}
	 */
	public void setXmlEnableAddCondition(@Nullable String enableAddCondition);

	/**
	 * Returns the derived {@code disableZoom} condition that renderers and other consumers read.
	 *
	 * @return the {@code disableZoom} attribute as authored, or else the negation of the authored
	 * 			{@code enableZoom} attribute; {@code null} when neither was authored
	 */
	public @Nullable String getDisableZoomCondition();

	/**
	 * Returns the {@code disableZoom} attribute as authored.
	 *
	 * @return the authored condition, or {@code null} when {@code enableZoom} or neither was authored
	 */
	public @Nullable String getXmlDisableZoomCondition();

	/**
	 * Sets the {@code disableZoom} attribute as authored.
	 *
	 * <p>Side effects: clears any authored {@code enableZoom} attribute.
	 *
	 * @param disableZoomCondition a condition name, an inline condition expression or a boolean literal; may be {@code null}
	 */
	public void setXmlDisableZoomCondition(@Nullable String disableZoomCondition);

	/**
	 * Returns the {@code enableZoom} attribute as authored.
	 *
	 * @return the authored condition, or {@code null} when {@code disableZoom} or neither was authored
	 */
	public @Nullable String getXmlEnableZoomCondition();

	/**
	 * Sets the {@code enableZoom} attribute as authored.
	 *
	 * <p>Side effects: clears any authored {@code disableZoom} attribute, so that
	 * {@link #getDisableZoomCondition()} returns the negation of this condition.
	 *
	 * @param enableZoomCondition a condition name, an inline condition expression or a boolean literal; may be {@code null}
	 */
	public void setXmlEnableZoomCondition(@Nullable String enableZoomCondition);

	/**
	 * Returns the derived {@code disableEdit} condition that renderers and other consumers read.
	 *
	 * @return the {@code disableEdit} attribute as authored, or else the negation of the authored
	 * 			{@code enableEdit} attribute; {@code null} when neither was authored
	 */
	public @Nullable String getDisableEditCondition();

	/**
	 * Returns the {@code disableEdit} attribute as authored.
	 *
	 * @return the authored condition, or {@code null} when {@code enableEdit} or neither was authored
	 */
	public @Nullable String getXmlDisableEditCondition();

	/**
	 * Sets the {@code disableEdit} attribute as authored.
	 *
	 * <p>Side effects: clears any authored {@code enableEdit} attribute.
	 *
	 * @param disableEditCondition a condition name, an inline condition expression or a boolean literal; may be {@code null}
	 */
	public void setXmlDisableEditCondition(@Nullable String disableEditCondition);

	/**
	 * Returns the {@code enableEdit} attribute as authored.
	 *
	 * @return the authored condition, or {@code null} when {@code disableEdit} or neither was authored
	 */
	public @Nullable String getXmlEnableEditCondition();

	/**
	 * Sets the {@code enableEdit} attribute as authored.
	 *
	 * <p>Side effects: clears any authored {@code disableEdit} attribute, so that
	 * {@link #getDisableEditCondition()} returns the negation of this condition.
	 *
	 * @param enableEditCondition a condition name, an inline condition expression or a boolean literal; may be {@code null}
	 */
	public void setXmlEnableEditCondition(@Nullable String enableEditCondition);

	/**
	 * Returns the derived {@code disableRemove} condition that renderers and other consumers read.
	 *
	 * @return the {@code disableRemove} attribute as authored, or else the negation of the authored
	 * 			{@code enableRemove} attribute; {@code null} when neither was authored
	 */
	public @Nullable String getDisableRemoveCondition();

	/**
	 * Returns the {@code disableRemove} attribute as authored.
	 *
	 * @return the authored condition, or {@code null} when {@code enableRemove} or neither was authored
	 */
	public @Nullable String getXmlDisableRemoveCondition();

	/**
	 * Sets the {@code disableRemove} attribute as authored.
	 *
	 * <p>Side effects: clears any authored {@code enableRemove} attribute.
	 *
	 * @param disableRemoveCondition a condition name, an inline condition expression or a boolean literal; may be {@code null}
	 */
	public void setXmlDisableRemoveCondition(@Nullable String disableRemoveCondition);

	/**
	 * Returns the {@code enableRemove} attribute as authored.
	 *
	 * @return the authored condition, or {@code null} when {@code disableRemove} or neither was authored
	 */
	public @Nullable String getXmlEnableRemoveCondition();

	/**
	 * Sets the {@code enableRemove} attribute as authored.
	 *
	 * <p>Side effects: clears any authored {@code disableRemove} attribute, so that
	 * {@link #getDisableRemoveCondition()} returns the negation of this condition.
	 *
	 * @param enableRemoveCondition a condition name, an inline condition expression or a boolean literal; may be {@code null}
	 */
	public void setXmlEnableRemoveCondition(@Nullable String enableRemoveCondition);
}
