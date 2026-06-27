package org.skyve.metadata.view.fluent;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.view.container.Tab;

class FluentTabTest {

	@Test
	@SuppressWarnings("static-method")
	void defaultConstructorCreatesTab() {
		FluentTab t = new FluentTab();
		assertNotNull(t.get());
	}

	@Test
	@SuppressWarnings("static-method")
	void wrappingConstructorUsesProvided() {
		Tab tab = new Tab();
		FluentTab t = new FluentTab(tab);
		assertSame(tab, t.get());
	}

	@Test
	@SuppressWarnings("static-method")
	void titleReturnsSelf() {
		FluentTab t = new FluentTab();
		FluentTab result = t.title("My Tab");
		assertSame(t, result);
	}

	@Test
	@SuppressWarnings("static-method")
	void iconStyleClassReturnsSelf() {
		FluentTab t = new FluentTab();
		FluentTab result = t.iconStyleClass("fa fa-home");
		assertSame(t, result);
	}

	@Test
	@SuppressWarnings("static-method")
	void icon16x16RelativeFileNameReturnsSelf() {
		FluentTab t = new FluentTab();
		FluentTab result = t.icon16x16RelativeFileName("img/icon.png");
		assertSame(t, result);
	}

	@Test
	@SuppressWarnings("static-method")
	void disabledConditionNameReturnsSelf() {
		FluentTab t = new FluentTab();
		FluentTab result = t.disabledConditionName("isReadOnly");
		assertSame(t, result);
	}

	@Test
	@SuppressWarnings("static-method")
	void invisibleConditionNameReturnsSelf() {
		FluentTab t = new FluentTab();
		FluentTab result = t.invisibleConditionName("hiddenCond");
		assertSame(t, result);
	}
}
