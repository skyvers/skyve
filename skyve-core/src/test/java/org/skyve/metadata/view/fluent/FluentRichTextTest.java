package org.skyve.metadata.view.fluent;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.view.widget.bound.input.RichText;

class FluentRichTextTest {

	@Test
	@SuppressWarnings("static-method")
	void defaultConstructorCreatesRichText() {
		FluentRichText rt = new FluentRichText();
		assertNotNull(rt.get());
	}

	@Test
	@SuppressWarnings("static-method")
	void wrappingConstructorUsesProvided() {
		RichText text = new RichText();
		FluentRichText rt = new FluentRichText(text);
		assertSame(text, rt.get());
	}

	@Test
	@SuppressWarnings("static-method")
	void pixelWidthReturnsSelf() {
		FluentRichText rt = new FluentRichText();
		FluentRichText result = rt.pixelWidth(500);
		assertSame(rt, result);
	}

	@Test
	@SuppressWarnings("static-method")
	void pixelHeightReturnsSelf() {
		FluentRichText rt = new FluentRichText();
		FluentRichText result = rt.pixelHeight(300);
		assertSame(rt, result);
	}

	@Test
	@SuppressWarnings("static-method")
	void minPixelHeightReturnsSelf() {
		FluentRichText rt = new FluentRichText();
		FluentRichText result = rt.minPixelHeight(100);
		assertSame(rt, result);
	}

	@Test
	@SuppressWarnings("static-method")
	void maxPixelHeightReturnsSelf() {
		FluentRichText rt = new FluentRichText();
		FluentRichText result = rt.maxPixelHeight(600);
		assertSame(rt, result);
	}
}
