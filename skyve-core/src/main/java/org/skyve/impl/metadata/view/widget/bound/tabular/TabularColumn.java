package org.skyve.impl.metadata.view.widget.bound.tabular;

import org.skyve.impl.metadata.view.HorizontalAlignment;
import org.skyve.metadata.SerializableMetaData;
import org.skyve.util.Util;

import jakarta.annotation.Nullable;

/**
 * Base interface for all column descriptors in tabular view widgets.
 *
 * <p>Declares the minimal column contract (title, alignment, width) shared by
 * {@link DataGridColumn} and list-grid column types.
 */
public interface TabularColumn extends SerializableMetaData {
	public @Nullable String getTitle();
	public default @Nullable String getLocalisedTitle() {
		return Util.i18n(getTitle());
	}
	public void setTitle(@Nullable String title);
	public @Nullable HorizontalAlignment getAlignment();
	public void setAlignment(@Nullable HorizontalAlignment alignment);
}
