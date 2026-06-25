package org.skyve.impl.web.faces.pipeline;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;
import org.skyve.util.OWASP;

@SuppressWarnings("static-method")
class AbstractFacesBuilderTest {
	private static final String UNSAFE_TEXT = "<img src=x onerror=alert(1)> & \"quoted\" 'single'";

	@Test
	void escapeFacesTextReturnsNullForNullValue() {
		assertNull(AbstractFacesBuilder.escapeFacesText(null, true));
		assertNull(AbstractFacesBuilder.escapeFacesText(null, true));
		assertNull(AbstractFacesBuilder.escapeFacesText(null, false));
	}

	@Test
	void escapeFacesTextEscapesHtmlByDefault() {
		assertEquals(OWASP.escapeHtml(UNSAFE_TEXT), AbstractFacesBuilder.escapeFacesText(UNSAFE_TEXT, true));
	}

	@Test
	void escapeFacesTextEscapesHtmlWhenExplicitlyTrue() {
		assertEquals(OWASP.escapeHtml(UNSAFE_TEXT), AbstractFacesBuilder.escapeFacesText(UNSAFE_TEXT, true));
	}

	@Test
	void escapeFacesTextLeavesTrustedHtmlRawWhenFalse() {
		assertEquals(UNSAFE_TEXT, AbstractFacesBuilder.escapeFacesText(UNSAFE_TEXT, false));
	}

	@Test
	void escapeFacesAttributeReturnsNullForNullValue() {
		assertNull(AbstractFacesBuilder.escapeFacesAttribute(null, true));
		assertNull(AbstractFacesBuilder.escapeFacesAttribute(null, true));
		assertNull(AbstractFacesBuilder.escapeFacesAttribute(null, false));
	}

	@Test
	void escapeFacesAttributeEscapesHtmlByDefaultThenAttributeSyntax() {
		String result = AbstractFacesBuilder.escapeFacesAttribute(UNSAFE_TEXT, true);

		assertEquals("&amp;lt;img src=x onerror=alert(1)&amp;gt; &amp;amp; &amp;#34;quoted&amp;#34; &amp;#39;single&amp;#39;", result);
		assertFalse(result.contains("\""));
		assertFalse(result.contains("'"));
		assertFalse(result.contains("<"));
		assertFalse(result.contains(">"));
	}

	@Test
	void escapeFacesAttributeEscapesHtmlWhenExplicitlyTrueThenAttributeSyntax() {
		String result = AbstractFacesBuilder.escapeFacesAttribute(UNSAFE_TEXT, true);

		assertEquals("&amp;lt;img src=x onerror=alert(1)&amp;gt; &amp;amp; &amp;#34;quoted&amp;#34; &amp;#39;single&amp;#39;", result);
		assertFalse(result.contains("\""));
		assertFalse(result.contains("'"));
		assertFalse(result.contains("<"));
		assertFalse(result.contains(">"));
	}

	@Test
	void escapeFacesAttributeLeavesTrustedHtmlRawWhenFalseButEscapesAttributeSyntax() {
		String result = AbstractFacesBuilder.escapeFacesAttribute(UNSAFE_TEXT, false);

		assertEquals("&lt;img src=x onerror=alert(1)> &amp; &#34;quoted&#34; &#39;single&#39;", result);
		assertFalse(result.contains("\""));
		assertFalse(result.contains("'"));
		assertFalse(result.contains("<"));
	}

	@Test
	void sanitiseFacesTextReturnsNullForNullValue() {
		assertNull(AbstractFacesBuilder.sanitiseFacesText(null));
	}

	@Test
	void sanitiseFacesTextStripsMarkupAndKeepsText() {
		assertEquals("Name required", AbstractFacesBuilder.sanitiseFacesText("<b>Name required</b>"));
	}
}
