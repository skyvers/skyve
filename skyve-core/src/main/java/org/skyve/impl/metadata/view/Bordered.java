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
	public Boolean getBorder();
	public void setBorder(Boolean border);
	public String getBorderTitle();
	public default String getLocalisedBorderTitle() {
		return Util.i18n(getBorderTitle());
	}
	public void setBorderTitle(String borderTitle);
}
