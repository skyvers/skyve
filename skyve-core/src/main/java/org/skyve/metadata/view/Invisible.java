package org.skyve.metadata.view;

import org.skyve.metadata.SerializableMetaData;

import jakarta.annotation.Nullable;

/**
 * Mixin interface for view elements that can be conditionally hidden.
 *
 * <p>The state is driven by a condition evaluated on the current document bean: the name of a
 * {@link org.skyve.metadata.model.document.Condition}, an inline condition expression, or a boolean literal.
 * A view may author either the {@code invisible} or the {@code visible} attribute.
 *
 * <p>Naming pattern: {@code getXml...} and {@code setXml...} are the attributes
 * exactly as authored, which is what is marshalled. {@link #getInvisibleCondition()} is the derived condition
 * that renderers and other consumers read; it negates the {@code visible} attribute when that is the one authored.
 *
 * @see Disableable
 */
public interface Invisible extends SerializableMetaData {
	/**
	 * Returns the derived {@code invisible} condition that renderers and other consumers read.
	 *
	 * @return the {@code invisible} attribute as authored, or else the negation of the authored
	 * 			{@code visible} attribute; {@code null} when neither was authored
	 */
	public @Nullable String getInvisibleCondition();
	
	/**
	 * Returns the {@code invisible} attribute as authored.
	 *
	 * @return the authored condition, or {@code null} when {@code visible} or neither was authored
	 */
	public @Nullable String getXmlInvisibleCondition();

	/**
	 * Returns the {@code visible} attribute as authored.
	 *
	 * @return the authored condition, or {@code null} when {@code invisible} or neither was authored
	 */
	public @Nullable String getXmlVisibleCondition();

	/**
	 * Sets the {@code invisible} attribute as authored.
	 *
	 * <p>Side effects: clears any authored {@code visible} attribute.
	 *
	 * @param invisibleCondition a condition name, an inline condition expression or a boolean literal; may be {@code null}
	 */
	public void setXmlInvisibleCondition(@Nullable String invisibleCondition);

	/**
	 * Sets the {@code visible} attribute as authored.
	 *
	 * <p>Side effects: clears any authored {@code invisible} attribute, so that
	 * {@link #getInvisibleCondition()} returns the negation of this condition.
	 *
	 * @param visibleCondition a condition name, an inline condition expression or a boolean literal; may be {@code null}
	 */
	public void setXmlVisibleCondition(@Nullable String visibleCondition);
}
