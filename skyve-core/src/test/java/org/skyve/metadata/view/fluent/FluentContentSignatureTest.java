package org.skyve.metadata.view.fluent;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.view.widget.bound.input.ContentSignature;

public class FluentContentSignatureTest {

	@Test
	@SuppressWarnings("static-method")
	void defaultConstructorCreatesContentSignature() {
		FluentContentSignature cs = new FluentContentSignature();
		assertNotNull(cs.get());
	}

	@Test
	@SuppressWarnings("static-method")
	void wrappingConstructorUsesProvided() {
		ContentSignature sig = new ContentSignature();
		FluentContentSignature cs = new FluentContentSignature(sig);
		assertSame(sig, cs.get());
	}

	@Test
	@SuppressWarnings("static-method")
	void rgbHexBackgroundColourReturnsSelf() {
		FluentContentSignature cs = new FluentContentSignature();
		FluentContentSignature result = cs.rgbHexBackgroundColour("FFFFFF");
		assertSame(cs, result);
	}

	@Test
	@SuppressWarnings("static-method")
	void rgbHexForegroundColourReturnsSelf() {
		FluentContentSignature cs = new FluentContentSignature();
		FluentContentSignature result = cs.rgbHexForegroundColour("000000");
		assertSame(cs, result);
	}

	@Test
	@SuppressWarnings("static-method")
	void pixelWidthReturnsSelf() {
		FluentContentSignature cs = new FluentContentSignature();
		FluentContentSignature result = cs.pixelWidth(400);
		assertSame(cs, result);
	}

	@Test
	@SuppressWarnings("static-method")
	void pixelHeightReturnsSelf() {
		FluentContentSignature cs = new FluentContentSignature();
		FluentContentSignature result = cs.pixelHeight(200);
		assertSame(cs, result);
	}
}
