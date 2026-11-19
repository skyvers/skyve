package org.skyve.impl.util;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.nio.charset.StandardCharsets;
import java.util.Base64;
import java.util.Objects;

import org.junit.jupiter.api.Test;

import jakarta.annotation.Nonnull;

@SuppressWarnings("static-method")
class SvgMarkupSanitiserTest {
	private static final String SVG_OPEN = "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"300\" height=\"200\" viewBox=\"0 0 300 200\">";
	// The serialiser writes attributes in alphabetical order
	private static final String SANITISED_SVG_OPEN = "<svg xmlns=\"http://www.w3.org/2000/svg\" height=\"200\" viewBox=\"0 0 300 200\" width=\"300\">";

	@Test
	void blankMarkupIsNull() {
		assertNull(SvgMarkupSanitiser.sanitise(null));
		assertNull(SvgMarkupSanitiser.sanitise("  \n "));
	}

	@Test
	void keepsFreehandStrokes() {
		String path = "<path d=\"M10 20 L30 40 L50 45\" fill=\"none\" stroke=\"#e53935\" stroke-width=\"6\" "
				+ "stroke-linecap=\"round\" stroke-linejoin=\"round\"/>";

		String result = sanitised(SVG_OPEN + path + "</svg>");

		assertEquals(SANITISED_SVG_OPEN + "<path d=\"M10 20 L30 40 L50 45\" fill=\"none\" stroke=\"#e53935\" "
				+ "stroke-linecap=\"round\" stroke-linejoin=\"round\" stroke-width=\"6\"/></svg>", result);
	}

	@Test
	void keepsLabelsFromTheUploadEditor() {
		String transform = " transform=\"rotate(90 150 100)\"";
		String svg = SVG_OPEN
				+ "<rect data-skyve-label=\"true\" x=\"40\" y=\"85\" width=\"220\" height=\"30\" rx=\"7.5\"" + transform
				+ " fill=\"#e53935\" fill-opacity=\"0.75\"/>"
				+ "<text data-skyve-text=\"true\" x=\"150\" y=\"107\"" + transform + " font-family=\"Helvetica, Arial, sans-serif\" "
				+ "font-size=\"20\" text-anchor=\"middle\" fill=\"#ffffff\">Crack &amp; damp &lt;here&gt;</text>"
				+ "</svg>";

		String result = sanitised(svg);

		assertTrue(result.contains("data-skyve-label=\"true\""), result);
		assertTrue(result.contains("rx=\"7.5\""), result);
		assertTrue(result.contains("fill-opacity=\"0.75\""), result);
		assertTrue(result.contains("fill=\"#e53935\""), result);
		assertTrue(result.contains("data-skyve-text=\"true\""), result);
		assertTrue(result.contains("font-family=\"Helvetica, Arial, sans-serif\""), result);
		assertEquals(2, result.split("rotate\\(90 150 100\\)", -1).length - 1, result);
		assertTrue(result.contains("fill=\"#ffffff\""), result);
		assertTrue(result.contains(">Crack &amp; damp &lt;here&gt;</text>"), result);
		assertTrue(result.indexOf("<rect") < result.indexOf("<text"), result);
	}

	@Test
	void keepsExcalidrawSceneCommentAndStructure() {
		String svg = SVG_OPEN
				+ "<!-- svg-source:excalidraw --><metadata><!-- payload-type:application/vnd.excalidraw+json -->"
				+ "<!-- payload-start -->eyJ2ZXJzaW9uIjoiMSJ9<!-- payload-end --></metadata>"
				+ "<defs><style class=\"style-fonts\">@font-face { font-family: \"Virgil\"; src: url(\"https://example.com/Virgil.woff2\"); }</style>"
				+ "<mask id=\"m1\"><rect x=\"0\" y=\"0\" width=\"10\" height=\"10\" fill=\"#fff\"/></mask></defs>"
				+ "<g stroke-linecap=\"round\" transform=\"translate(10 10) rotate(0 5 5)\" mask=\"url(#m1)\">"
				+ "<path d=\"M0 0 L5 5\" stroke=\"#1e1e1e\" stroke-width=\"1\" fill=\"none\"/></g>"
				+ "<text x=\"5\" y=\"5\" font-family=\"Virgil, Segoe UI Emoji\" font-size=\"20px\" fill=\"#1e1e1e\" "
				+ "text-anchor=\"start\" style=\"white-space: pre;\" direction=\"ltr\" dominant-baseline=\"alphabetic\">Crack</text>"
				+ "</svg>";

		String result = sanitised(svg);

		assertTrue(result.contains("<!-- payload-start -->eyJ2ZXJzaW9uIjoiMSJ9<!-- payload-end -->"), result);
		assertTrue(result.contains("<!-- svg-source:excalidraw -->"), result);
		assertTrue(result.contains("mask=\"url(#m1)\""), result);
		assertTrue(result.contains("style=\"white-space: pre;\""), result);
		assertTrue(result.contains(">Crack</text>"), result);
		assertFalse(result.contains("<style"), result);
		assertFalse(result.contains("example.com"), result);
	}

	@Test
	void removesScriptsHandlersAndForeignContent() {
		String svg = SVG_OPEN
				+ "<script>alert(1)</script>"
				+ "<foreignObject width=\"10\" height=\"10\"><div xmlns=\"http://www.w3.org/1999/xhtml\">x</div></foreignObject>"
				+ "<rect width=\"10\" height=\"10\" onclick=\"alert(2)\" onload=\"alert(3)\" fill=\"red\"/>"
				+ "<animate attributeName=\"href\" to=\"javascript:alert(4)\"/>"
				+ "<?xml-stylesheet href=\"https://example.com/x.css\"?>"
				+ "</svg>";

		String result = sanitised(svg);

		assertEquals(SANITISED_SVG_OPEN + "<rect fill=\"red\" height=\"10\" width=\"10\"/></svg>", result);
	}

	@Test
	void unwrapsLinksAndKeepsTheirDrawing() {
		String svg = SVG_OPEN + "<a href=\"javascript:alert(1)\" target=\"_blank\"><path d=\"M1 1 L2 2\" stroke=\"blue\"/></a></svg>";

		String result = sanitised(svg);

		assertEquals(SANITISED_SVG_OPEN + "<path d=\"M1 1 L2 2\" stroke=\"blue\"/></svg>", result);
	}

	@Test
	void allowsOnlyLocalReferencesAndEmbeddedImages() {
		String png = "data:image/png;base64,iVBORw0KGgo=";
		String svg = SVG_OPEN
				+ "<use href=\"#symbol1\"/>"
				+ "<use href=\"https://example.com/sprite.svg#a\"/>"
				+ "<image href=\"" + png + "\" width=\"5\" height=\"5\"/>"
				+ "<image href=\"https://example.com/tracker.png\" width=\"5\" height=\"5\"/>"
				+ "<image xmlns:xlink=\"http://www.w3.org/1999/xlink\" xlink:href=\"javascript:alert(1)\" width=\"5\" height=\"5\"/>"
				+ "</svg>";

		String result = sanitised(svg);

		assertTrue(result.contains("<use href=\"#symbol1\"/>"), result);
		assertTrue(result.contains("href=\"" + png + "\""), result);
		assertFalse(result.contains("example.com"), result);
		assertFalse(result.contains("javascript"), result);
	}

	@Test
	void sanitisesEmbeddedSvgImages() {
		String inner = SVG_OPEN + "<script>alert(1)</script><path d=\"M1 1 L2 2\" stroke=\"red\"/></svg>";
		String dataUrl = "data:image/svg+xml;base64," + Base64.getEncoder().encodeToString(inner.getBytes(StandardCharsets.UTF_8));
		String svg = SVG_OPEN + "<image href=\"" + dataUrl + "\" width=\"300\" height=\"200\"/></svg>";

		String result = sanitised(svg);

		String prefix = "href=\"data:image/svg+xml;base64,";
		int start = result.indexOf(prefix) + prefix.length();
		String embedded = new String(Base64.getDecoder().decode(result.substring(start, result.indexOf('"', start))), StandardCharsets.UTF_8);
		assertEquals(SANITISED_SVG_OPEN + "<path d=\"M1 1 L2 2\" stroke=\"red\"/></svg>", embedded);
	}

	@Test
	void removesUnsafeStylesAndUrlReferences() {
		String svg = SVG_OPEN
				+ "<rect width=\"1\" height=\"1\" style=\"fill: url(https://example.com/a.png)\"/>"
				+ "<rect width=\"2\" height=\"2\" fill=\"url( 'https://example.com/b.svg#g' )\"/>"
				+ "<rect width=\"3\" height=\"3\" style=\"behavior: url(#x)\"/>"
				+ "<rect width=\"4\" height=\"4\" fill=\"url(#gradient)\" style=\"opacity: 0.5\"/>"
				+ "</svg>";

		String result = sanitised(svg);

		assertFalse(result.contains("example.com"), result);
		assertFalse(result.contains("behavior"), result);
		assertTrue(result.contains("fill=\"url(#gradient)\""), result);
		assertTrue(result.contains("style=\"opacity: 0.5\""), result);
	}

	@Test
	void refusesDocumentTypeDeclarations() {
		String svg = "<?xml version=\"1.0\"?><!DOCTYPE svg [<!ENTITY xxe SYSTEM \"file:///etc/passwd\">]>"
				+ SVG_OPEN + "<text>&xxe;</text></svg>";

		assertThrows(IllegalArgumentException.class, () -> SvgMarkupSanitiser.sanitise(svg));
	}

	@Test
	void refusesMarkupThatIsNotSvg() {
		assertThrows(IllegalArgumentException.class, () -> SvgMarkupSanitiser.sanitise("<html><body/></html>"));
		assertThrows(IllegalArgumentException.class, () -> SvgMarkupSanitiser.sanitise("<svg><path d=\"M0 0\"/></svg>"));
		assertThrows(IllegalArgumentException.class, () -> SvgMarkupSanitiser.sanitise("not xml"));
	}

	@Test
	void refusesMarkupThatIsTooLong() {
		String svg = SVG_OPEN + "<desc>" + "x".repeat(SvgMarkupSanitiser.MAXIMUM_LENGTH) + "</desc></svg>";

		assertThrows(IllegalArgumentException.class, () -> SvgMarkupSanitiser.sanitise(svg));
	}

	private static @Nonnull String sanitised(@Nonnull String svg) {
		return Objects.requireNonNull(SvgMarkupSanitiser.sanitise(svg), "sanitised markup");
	}
}
