package org.skyve.metadata.view.fluent;

interface FluentRelativeSize<T extends FluentRelativeSize<T>> extends FluentAbsoluteSize<T>, FluentConstrainableSize<T> {
	T percentageWidth(int percentageWidth);
	T responsiveWidth(int responsiveWidth);
	T sm(int sm);
	T md(int md);
	T lg(int lg);
	T xl(int xl);
	T percentageHeight(int percentageHeight);
}
