package org.skyve.impl.metadata.view;

import org.skyve.metadata.SerializableMetaData;
import org.skyve.util.Util;

/**
 * Mixin interface for view widgets that support a visible border.
 *
 * <p>Implemented by JAXB widget descriptors that expose a {@code border}
 * boolean attribute controlling whether a visual border is rendered around
 * the widget.
 */
public interface Bordered extends SerializableMetaData {
	/**
	 * Returns whether a border is rendered around this metadata element.
	 *
	 * @return {@code Boolean.TRUE} when a border is explicitly enabled,
	 *         {@code Boolean.FALSE} when explicitly disabled, or {@code null}
	 *         when the renderer default applies
	 */
	public Boolean getBorder();

	/**
	 * Sets whether a border is rendered around this metadata element.
	 *
	 * @param border {@code Boolean.TRUE} to enable a border,
	 *        {@code Boolean.FALSE} to disable it, or {@code null} to use the
	 *        renderer default
	 */
	public void setBorder(Boolean border);

	/**
	 * Returns the raw border title text.
	 *
	 * @return border title text, or {@code null} when no title is configured
	 */
	public String getBorderTitle();

	/**
	 * Returns the border title translated for the current locale.
	 *
	 * @return localised border title text, or {@code null} when no title is configured
	 */
	public default String getLocalisedBorderTitle() {
		return Util.i18n(getBorderTitle());
	}

	/**
	 * Sets the raw border title text.
	 *
	 * @param borderTitle border title text; may be {@code null}
	 */
	public void setBorderTitle(String borderTitle);

	/**
	 * Returns whether the border title text should be escaped before rendering.
	 *
	 * @return {@code Boolean.FALSE} to allow trusted markup; {@code null} or
	 *         {@code Boolean.TRUE} to escape at the renderer boundary
	 */
	public Boolean getEscapeBorderTitle();

	/**
	 * Sets whether the border title text should be escaped before rendering.
	 *
	 * @param escapeBorderTitle {@code Boolean.FALSE} to allow trusted markup;
	 *        {@code null} or {@code Boolean.TRUE} to escape at the renderer boundary
	 */
	public void setEscapeBorderTitle(Boolean escapeBorderTitle);
}
