package org.skyve.impl.metadata.view.container;

import org.skyve.impl.metadata.view.ShrinkWrapper;
import org.skyve.metadata.DecoratedMetaData;
import org.skyve.metadata.view.Invisible;

/**
 * Mixin interface for box-layout container widgets ({@link HBox}, {@link VBox}).
 *
 * <p>Combines shrink-wrap sizing ({@link ShrinkWrapper}), visibility conditions
 * ({@link Invisible}), and decorator properties ({@link DecoratedMetaData}) into
 * the base contract for horizontal and vertical box layouts.
 */
public interface Box extends ShrinkWrapper, Invisible, DecoratedMetaData {
	public Integer getPixelPadding();
	public void setPixelPadding(Integer pixelPadding);
	public Integer getPixelMemberPadding();
	public void setPixelMemberPadding(Integer pixelMemberPadding);
	
	public Collapsible getCollapsible();
}
