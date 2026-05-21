package org.skyve.metadata.view.fluent;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.view.widget.Blurb;

class FluentBlurbTest {

	@Test
	@SuppressWarnings("static-method")
	void defaultConstructorCreatesBlurb() {
		FluentBlurb b = new FluentBlurb();
		assertNotNull(b.get());
	}

	@Test
	@SuppressWarnings("static-method")
	void wrappingConstructorUsesProvided() {
		Blurb blurb = new Blurb();
		FluentBlurb b = new FluentBlurb(blurb);
		assertSame(blurb, b.get());
	}

	@Test
	@SuppressWarnings("static-method")
	void markupReturnsSelf() {
		FluentBlurb b = new FluentBlurb();
		FluentBlurb result = b.markup("<b>Hello</b>");
		assertSame(b, result);
	}

	@Test
	@SuppressWarnings("static-method")
	void pixelWidthReturnsSelf() {
		FluentBlurb b = new FluentBlurb();
		FluentBlurb result = b.pixelWidth(300);
		assertSame(b, result);
	}

	@Test
	@SuppressWarnings("static-method")
	void pixelHeightReturnsSelf() {
		FluentBlurb b = new FluentBlurb();
		FluentBlurb result = b.pixelHeight(200);
		assertSame(b, result);
	}

	@Test
	@SuppressWarnings("static-method")
	void invisibleConditionNameReturnsSelf() {
		FluentBlurb b = new FluentBlurb();
		FluentBlurb result = b.invisibleConditionName("hiddenCond");
		assertSame(b, result);
	}

	@Test
	@SuppressWarnings("static-method")
	void escapeReturnsSelf() {
		FluentBlurb b = new FluentBlurb();
		FluentBlurb result = b.escape(true);
		assertSame(b, result);
	}

	@Test
	@SuppressWarnings("static-method")
	void escapeWithFalseSetsEscapeToFalse() {
		FluentBlurb b = new FluentBlurb();
		b.escape(false);
		assertEquals(Boolean.FALSE, b.get().getEscape());
	}

	@Test
	@SuppressWarnings("static-method")
	void putPropertyReturnsSelf() {
		FluentBlurb b = new FluentBlurb();
		FluentBlurb result = b.putProperty("key", "value");
		assertSame(b, result);
	}

	@Test
	@SuppressWarnings("static-method")
	void fromWithEscapeNonNullCopiesEscape() {
		Blurb blurb = new Blurb();
		blurb.setEscape(Boolean.TRUE);
		FluentBlurb fb = new FluentBlurb();
		FluentBlurb result = fb.from(blurb);
		assertSame(fb, result);
	}

	@Test
	@SuppressWarnings("static-method")
	void fromWithNonEmptyPropertiesCopiesPropertiesViaLambda() {
		// Exercises the lambda in: blurb.getProperties().entrySet().forEach(p -> putProperty(...))
		Blurb blurb = new Blurb();
		blurb.getProperties().put("colour", "red");
		FluentBlurb fb = new FluentBlurb();
		fb.from(blurb);
		assertEquals("red", fb.get().getProperties().get("colour"));
	}
}
