package org.skyve.impl.metadata.view.widget.bound.tabular;

import org.skyve.impl.metadata.view.HorizontalAlignment;
import org.skyve.metadata.SerializableMetaData;
import org.skyve.util.Util;

/**
 * Base interface for all column descriptors in tabular view widgets.
 *
 * <p>Declares the minimal column contract (title, alignment, width) shared by
 * {@link DataGridColumn} and list-grid column types.
 */
public interface TabularColumn extends SerializableMetaData {
	public String getTitle();
	public default String getLocalisedTitle() {
		return Util.i18n(getTitle());
	}
	public void setTitle(String title);
	public HorizontalAlignment getAlignment();
	public void setAlignment(HorizontalAlignment alignment);
}
