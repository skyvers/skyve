package org.skyve.impl.metadata.view;

import jakarta.annotation.Nullable;

/**
 * Mixin interface for view widgets that support the shrink-wrap layout mode.
 *
 * <p>Adds the {@link ShrinkWrap} attribute to {@link RelativeSize}, allowing the
 * widget to size itself to its content rather than filling its container.
 */
public interface ShrinkWrapper extends RelativeSize {
	public @Nullable ShrinkWrap getShrinkWrap();
}
