package org.skyve.impl.metadata.view;

import jakarta.annotation.Nullable;

/**
 * Mixin interface for view widgets that declare both an absolute pixel width
 * and an absolute pixel height.
 *
 * <p>Extends {@link AbsoluteWidth} with a {@code pixelHeight} attribute.
 */
public interface AbsoluteSize extends AbsoluteWidth {
	public @Nullable Integer getPixelHeight();
	public void setPixelHeight(@Nullable Integer pixelHeight);
}
