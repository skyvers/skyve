package org.skyve.metadata.view;

import org.skyve.metadata.SerializableMetaData;

import jakarta.annotation.Nullable;

/**
 * Mixin interface for view elements that can be conditionally disabled.
 *
 * <p>The state is driven by a condition evaluated on the current document bean: the name of a
 * {@link org.skyve.metadata.model.document.Condition}, an inline condition expression, or a boolean literal.
 * A view may author either the {@code disabled} or the {@code enabled} attribute.
 *
 * <p>Naming pattern: {@code getXml...} and {@code setXml...} are the attributes
 * exactly as authored, which is what is marshalled. {@link #getDisabledCondition()} is the derived condition
 * that renderers and other consumers read; it negates the {@code enabled} attribute when that is the one authored.
 *
 * @see Invisible
 * @see Editable
 */
public interface Disableable extends SerializableMetaData {
	/**
	 * Returns the derived {@code disabled} condition that renderers and other consumers read.
	 *
	 * @return the {@code disabled} attribute as authored, or else the negation of the authored
	 * 			{@code enabled} attribute; {@code null} when neither was authored
	 */
	public @Nullable String getDisabledCondition();
	
	/**
	 * Returns the {@code disabled} attribute as authored.
	 *
	 * @return the authored condition, or {@code null} when {@code enabled} or neither was authored
	 */
	public @Nullable String getXmlDisabledCondition();

	/**
	 * Returns the {@code enabled} attribute as authored.
	 *
	 * @return the authored condition, or {@code null} when {@code disabled} or neither was authored
	 */
	public @Nullable String getXmlEnabledCondition();

	/**
	 * Sets the {@code disabled} attribute as authored.
	 *
	 * <p>Side effects: clears any authored {@code enabled} attribute.
	 *
	 * @param disabledCondition a condition name, an inline condition expression or a boolean literal; may be {@code null}
	 */
	public void setXmlDisabledCondition(@Nullable String disabledCondition);

	/**
	 * Sets the {@code enabled} attribute as authored.
	 *
	 * <p>Side effects: clears any authored {@code disabled} attribute, so that
	 * {@link #getDisabledCondition()} returns the negation of this condition.
	 *
	 * @param enabledCondition a condition name, an inline condition expression or a boolean literal; may be {@code null}
	 */
	public void setXmlEnabledCondition(@Nullable String enabledCondition);
}
