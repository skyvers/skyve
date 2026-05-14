package org.skyve.metadata.view.fluent;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.view.widget.bound.ZoomIn;

public class FluentZoomInTest {

	@Test
	@SuppressWarnings("static-method")
	void defaultConstructorCreatesZoomIn() {
		FluentZoomIn z = new FluentZoomIn();
		assertNotNull(z.get());
	}

	@Test
	@SuppressWarnings("static-method")
	void wrappingConstructorUsesProvided() {
		ZoomIn zoom = new ZoomIn();
		FluentZoomIn z = new FluentZoomIn(zoom);
		assertSame(zoom, z.get());
	}

	@Test
	@SuppressWarnings("static-method")
	void displayNameReturnsSelf() {
		FluentZoomIn z = new FluentZoomIn();
		FluentZoomIn result = z.displayName("Zoom");
		assertSame(z, result);
	}

	@Test
	@SuppressWarnings("static-method")
	void relativeIconFileNameReturnsSelf() {
		FluentZoomIn z = new FluentZoomIn();
		FluentZoomIn result = z.relativeIconFileName("icon.png");
		assertSame(z, result);
	}

	@Test
	@SuppressWarnings("static-method")
	void iconStyleClassReturnsSelf() {
		FluentZoomIn z = new FluentZoomIn();
		FluentZoomIn result = z.iconStyleClass("fa fa-search");
		assertSame(z, result);
	}

	@Test
	@SuppressWarnings("static-method")
	void toolTipReturnsSelf() {
		FluentZoomIn z = new FluentZoomIn();
		FluentZoomIn result = z.toolTip("Click to zoom");
		assertSame(z, result);
	}

	@Test
	@SuppressWarnings("static-method")
	void disabledConditionNameReturnsSelf() {
		FluentZoomIn z = new FluentZoomIn();
		FluentZoomIn result = z.disabledConditionName("isReadOnly");
		assertSame(z, result);
	}

	@Test
	@SuppressWarnings("static-method")
	void invisibleConditionNameReturnsSelf() {
		FluentZoomIn z = new FluentZoomIn();
		FluentZoomIn result = z.invisibleConditionName("hiddenCond");
		assertSame(z, result);
	}
}
