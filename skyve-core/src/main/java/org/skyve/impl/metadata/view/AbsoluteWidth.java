package org.skyve.impl.metadata.view;

import org.skyve.metadata.SerializableMetaData;

import jakarta.annotation.Nullable;

/**
 * Mixin interface for view widgets that declare an absolute pixel width.
 *
 * <p>Implemented by JAXB widget descriptors that expose a {@code pixelWidth}
 * attribute.  Pairs with {@link AbsoluteSize} (adds height) and
 * {@link RelativeWidth} (adds responsive column width).
 */
public interface AbsoluteWidth extends SerializableMetaData {
	public @Nullable Integer getPixelWidth();
	public void setPixelWidth(@Nullable Integer pixelWidth);
}
