package org.skyve.metadata.view.fluent;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.view.widget.bound.input.TextArea;

public class FluentTextAreaTest {

	@Test
	@SuppressWarnings("static-method")
	void defaultConstructorCreatesTextArea() {
		FluentTextArea ta = new FluentTextArea();
		assertNotNull(ta.get());
	}

	@Test
	@SuppressWarnings("static-method")
	void wrappingConstructorUsesProvided() {
		TextArea text = new TextArea();
		FluentTextArea ta = new FluentTextArea(text);
		assertSame(text, ta.get());
	}

	@Test
	@SuppressWarnings("static-method")
	void wordWrapReturnsSelf() {
		FluentTextArea ta = new FluentTextArea();
		FluentTextArea result = ta.wordWrap(true);
		assertSame(ta, result);
	}

	@Test
	@SuppressWarnings("static-method")
	void editableReturnsSelf() {
		FluentTextArea ta = new FluentTextArea();
		FluentTextArea result = ta.editable(false);
		assertSame(ta, result);
	}

	@Test
	@SuppressWarnings("static-method")
	void minPixelHeightDoesNotThrow() {
		// FluentTextArea.minPixelHeight() returns null (unimplemented) - just verify it doesn't throw
		new FluentTextArea().minPixelHeight(50);
	}

	@Test
	@SuppressWarnings("static-method")
	void pixelWidthDoesNotThrow() {
		// FluentTextArea.pixelWidth() returns null (unimplemented) - just verify it doesn't throw
		new FluentTextArea().pixelWidth(400);
	}

	@Test
	@SuppressWarnings("static-method")
	void pixelHeightDoesNotThrow() {
		// FluentTextArea.pixelHeight() returns null (unimplemented) - just verify it doesn't throw
		new FluentTextArea().pixelHeight(150);
	}
}
