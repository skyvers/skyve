package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.ShrinkWrap;

interface FluentBox<T extends FluentBox<T>> extends FluentRelativeSize<T> {
	T shrinkWrap(ShrinkWrap shrinkWrap);
	T pixelPadding(int pixelPadding);
	T pixelMemberPadding(int pixelMemberPadding);
}
