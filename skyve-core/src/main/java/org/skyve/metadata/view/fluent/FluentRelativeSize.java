package org.skyve.metadata.view.fluent;

interface FluentRelativeSize<T extends FluentRelativeSize<T>> extends FluentAbsoluteSize<T>, FluentResponsiveWidth<T>, FluentConstrainableSize<T> {
	T percentageHeight(int percentageHeight);
}
