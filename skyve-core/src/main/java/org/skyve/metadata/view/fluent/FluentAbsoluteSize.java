package org.skyve.metadata.view.fluent;

interface FluentAbsoluteSize<T extends FluentAbsoluteSize<T>> extends FluentAbsoluteWidth<T> {
	T pixelHeight(int height);
}
