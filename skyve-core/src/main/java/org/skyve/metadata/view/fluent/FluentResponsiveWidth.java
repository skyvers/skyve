package org.skyve.metadata.view.fluent;

interface FluentResponsiveWidth<T extends FluentResponsiveWidth<T>> extends FluentRelativeWidth<T> {
	T sm(int sm);
	T md(int md);
	T lg(int lg);
	T xl(int xl);
}
