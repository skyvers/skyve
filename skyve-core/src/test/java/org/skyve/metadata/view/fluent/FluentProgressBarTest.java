package org.skyve.metadata.view.fluent;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.view.widget.bound.ProgressBar;

class FluentProgressBarTest {

	@Test
	@SuppressWarnings("static-method")
	void defaultConstructorCreatesProgressBar() {
		FluentProgressBar pb = new FluentProgressBar();
		assertNotNull(pb.get());
	}

	@Test
	@SuppressWarnings("static-method")
	void wrappingConstructorUsesProvided() {
		ProgressBar bar = new ProgressBar();
		FluentProgressBar pb = new FluentProgressBar(bar);
		assertSame(bar, pb.get());
	}

	@Test
	@SuppressWarnings("static-method")
	void invisibleConditionNameReturnsSelf() {
		FluentProgressBar pb = new FluentProgressBar();
		FluentProgressBar result = pb.invisibleConditionName("hiddenCond");
		assertSame(pb, result);
	}

	@Test
	@SuppressWarnings("static-method")
	void pixelWidthReturnsSelf() {
		FluentProgressBar pb = new FluentProgressBar();
		FluentProgressBar result = pb.pixelWidth(300);
		assertSame(pb, result);
	}

	@Test
	@SuppressWarnings("static-method")
	void pixelHeightReturnsSelf() {
		FluentProgressBar pb = new FluentProgressBar();
		FluentProgressBar result = pb.pixelHeight(20);
		assertSame(pb, result);
	}

	@Test
	@SuppressWarnings("static-method")
	void minPixelHeightReturnsSelf() {
		FluentProgressBar pb = new FluentProgressBar();
		FluentProgressBar result = pb.minPixelHeight(10);
		assertSame(pb, result);
	}

	@Test
	@SuppressWarnings("static-method")
	void maxPixelHeightReturnsSelf() {
		FluentProgressBar pb = new FluentProgressBar();
		FluentProgressBar result = pb.maxPixelHeight(50);
		assertSame(pb, result);
	}
}
