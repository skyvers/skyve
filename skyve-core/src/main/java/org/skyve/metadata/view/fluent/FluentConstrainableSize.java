package org.skyve.metadata.view.fluent;

interface FluentConstrainableSize<T extends FluentConstrainableSize<T>> extends FluentConstrainableHeight<T> {
	T minPixelWidth(int minPixelWidth);
	T maxPixelWidth(int maxPixelWidth);
}
