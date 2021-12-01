package org.skyve.metadata.view.fluent;

interface FluentConstrainableHeight<T extends FluentConstrainableHeight<T>> extends FluentMinimumHeight<T> {
	T maxPixelHeight(int maxPixelHeight);
}
