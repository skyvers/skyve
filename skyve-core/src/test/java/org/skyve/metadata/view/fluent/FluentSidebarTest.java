package org.skyve.metadata.view.fluent;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.view.container.Sidebar;

class FluentSidebarTest {

	@Test
	@SuppressWarnings("static-method")
	void defaultConstructorCreatesSidebar() {
		FluentSidebar s = new FluentSidebar();
		assertNotNull(s.get());
	}

	@Test
	@SuppressWarnings("static-method")
	void wrappingConstructorUsesProvided() {
		Sidebar sidebar = new Sidebar();
		FluentSidebar s = new FluentSidebar(sidebar);
		assertSame(sidebar, s.get());
	}

	@Test
	@SuppressWarnings("static-method")
	void widgetIdReturnsSelf() {
		FluentSidebar s = new FluentSidebar();
		FluentSidebar result = s.widgetId("sb1");
		assertSame(s, result);
	}

	@Test
	@SuppressWarnings("static-method")
	void pixelWidthReturnsSelf() {
		FluentSidebar s = new FluentSidebar();
		FluentSidebar result = s.pixelWidth(250);
		assertSame(s, result);
	}

	@Test
	@SuppressWarnings("static-method")
	void responsiveWidthReturnsSelf() {
		FluentSidebar s = new FluentSidebar();
		FluentSidebar result = s.responsiveWidth(3);
		assertSame(s, result);
	}

	@Test
	@SuppressWarnings("static-method")
	void percentageWidthReturnsSelf() {
		FluentSidebar s = new FluentSidebar();
		FluentSidebar result = s.percentageWidth(20);
		assertSame(s, result);
	}
}
