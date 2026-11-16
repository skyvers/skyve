package org.skyve.impl.metadata.view;

import org.skyve.metadata.SerializableMetaData;

import jakarta.annotation.Nullable;

/**
 * Mixin interface for view widgets that declare a minimum pixel height.
 *
 * <p>Implemented by JAXB widget descriptors that expose a {@code minPixelHeight}
 * attribute.  Extended by {@link ConstrainableHeight} (adds maximum height).
 */
public interface MinimumHeight extends SerializableMetaData {
	public @Nullable Integer getMinPixelHeight();
	public void setMinPixelHeight(@Nullable Integer minPixelHeight);
}
