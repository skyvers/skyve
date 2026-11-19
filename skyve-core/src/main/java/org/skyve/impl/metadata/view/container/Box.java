package org.skyve.impl.metadata.view.container;

import org.skyve.impl.metadata.view.ShrinkWrapper;
import org.skyve.metadata.DecoratedMetaData;
import org.skyve.metadata.view.Invisible;

import jakarta.annotation.Nullable;

/**
 * Mixin interface for box-layout container widgets ({@link HBox}, {@link VBox}).
 *
 * <p>Combines shrink-wrap sizing ({@link ShrinkWrapper}), visibility conditions
 * ({@link Invisible}), and decorator properties ({@link DecoratedMetaData}) into
 * the base contract for horizontal and vertical box layouts.
 */
public interface Box extends ShrinkWrapper, Invisible, DecoratedMetaData {
	public @Nullable Integer getPixelPadding();
	public void setPixelPadding(@Nullable Integer pixelPadding);
	public @Nullable Integer getPixelMemberPadding();
	public void setPixelMemberPadding(@Nullable Integer pixelMemberPadding);
	
	/**
	 * Returns the collapse behavior for this box container.
	 *
	 * @return the collapse mode, or {@code null} when collapsibility is not configured
	 */
	public @Nullable Collapsible getCollapsible();
}
