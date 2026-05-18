package org.skyve.metadata.view.fluent;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.view.widget.Button;

class FluentButtonTest {

	@Test
	@SuppressWarnings("static-method")
	void defaultConstructorCreatesButton() {
		FluentButton b = new FluentButton();
		assertNotNull(b.get());
	}

	@Test
	@SuppressWarnings("static-method")
	void wrappingConstructorUsesProvided() {
		Button button = new Button();
		FluentButton b = new FluentButton(button);
		assertSame(button, b.get());
	}

	@Test
	@SuppressWarnings("static-method")
	void actionNameReturnsSelf() {
		FluentButton b = new FluentButton();
		FluentButton result = b.actionName("Save");
		assertSame(b, result);
	}

	@Test
	@SuppressWarnings("static-method")
	void pixelWidthReturnsSelf() {
		FluentButton b = new FluentButton();
		FluentButton result = b.pixelWidth(120);
		assertSame(b, result);
	}

	@Test
	@SuppressWarnings("static-method")
	void pixelHeightReturnsSelf() {
		FluentButton b = new FluentButton();
		FluentButton result = b.pixelHeight(40);
		assertSame(b, result);
	}

	@Test
	@SuppressWarnings("static-method")
	void minPixelHeightReturnsSelf() {
		FluentButton b = new FluentButton();
		FluentButton result = b.minPixelHeight(30);
		assertSame(b, result);
	}

	@Test
	@SuppressWarnings("static-method")
	void maxPixelHeightReturnsSelf() {
		FluentButton b = new FluentButton();
		FluentButton result = b.maxPixelHeight(60);
		assertSame(b, result);
	}
}
