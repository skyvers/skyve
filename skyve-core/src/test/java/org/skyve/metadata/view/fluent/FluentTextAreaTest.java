package org.skyve.metadata.view.fluent;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.view.widget.bound.input.KeyboardType;
import org.skyve.impl.metadata.view.widget.bound.input.TextArea;

class FluentTextAreaTest {

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
	void minPixelHeightReturnsSelfAndSetsMetadata() {
		FluentTextArea ta = new FluentTextArea();
		assertSame(ta, ta.minPixelHeight(50));
		assertEquals(Integer.valueOf(50), ta.get().getMinPixelHeight());
	}

	@Test
	@SuppressWarnings("static-method")
	void keyboardTypeReturnsSelfAndSetsMetadata() {
		FluentTextArea ta = new FluentTextArea();
		assertSame(ta, ta.keyboardType(KeyboardType.email));
		assertEquals(KeyboardType.email, ta.get().getKeyboardType());
	}

	@Test
	@SuppressWarnings("static-method")
	void pixelWidthReturnsSelfAndSetsMetadata() {
		FluentTextArea ta = new FluentTextArea();
		assertSame(ta, ta.pixelWidth(400));
		assertEquals(Integer.valueOf(400), ta.get().getPixelWidth());
	}

	@Test
	@SuppressWarnings("static-method")
	void pixelHeightReturnsSelfAndSetsMetadata() {
		FluentTextArea ta = new FluentTextArea();
		assertSame(ta, ta.pixelHeight(150));
		assertEquals(Integer.valueOf(150), ta.get().getPixelHeight());
	}
}
