package org.skyve.metadata.view.fluent;

interface FluentRelativeWidth<T extends FluentRelativeWidth<T>> extends FluentAbsoluteWidth<T> {
	T percentageWidth(int width);
	T responsiveWidth(int width);
}
