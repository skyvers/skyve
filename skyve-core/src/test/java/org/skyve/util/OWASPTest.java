package org.skyve.util;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.skyve.metadata.view.TextOutput.Sanitisation;

public class OWASPTest {

	@Test
	@SuppressWarnings("static-method")
	public void testSanitiseRelaxedAllowsImageSrc() {
		// setup the test data
		String html = "<img src=\"http://localhost:8080/skyve/images/test.gif\" alt=\"Larry\" />";

		// call the method under test
		String result = OWASP.sanitise(Sanitisation.relaxed, html);

		// verify the result
		assertThat("Relaxied sanitiser should not modify the html", result, is(html));
	}

	@Test
	@Disabled("Relaxed sanitiser currently strips data urls, to be enabled once updated")
	@SuppressWarnings("static-method")
	public void testSanitiseRelaxedAllowsImageSrcDataUrl() {
		// setup the test data
		String html = "<img src=\"data:image/gif;base64,R0lGODdhMAAwAPAAAAAAAP///ywAAAAAMAAw AAAC8IyPqcvt3wCcDkiLc7C0qwyGHhSWpjQu5yqmCYsapyuvUUlvONmOZtfzgFz ByTB10QgxOR0TqBQejhRNzOfkVJ+5YiUqrXF5Y5lKh/DeuNcP5yLWGsEbtLiOSp a/TPg7JpJHxyendzWTBfX0cxOnKPjgBzi4diinWGdkF8kjdfnycQZXZeYGejmJl ZeGl9i2icVqaNVailT6F5iJ90m6mvuTS4OK05M0vDk0Q4XUtwvKOzrcd3iq9uis F81M1OIcR7lEewwcLp7tuNNkM3uNna3F2JQFo97Vriy/Xl4/f1cf5VWzXyym7PH hhx4dbgYKAAA7\" alt=\"Larry\">";

		// call the method under test
		String result = OWASP.sanitise(Sanitisation.relaxed, html);

		// verify the result
		assertThat("Relaxied sanitiser should not modify the html", result, is(html));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testSanitiseTextLevel() {
		String html = "<p>Hello <script>alert('xss')</script> World</p>";
		String result = OWASP.sanitise(Sanitisation.text, html);
		assertThat("Text sanitizer should strip all HTML", result, is("Hello  World"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testSanitiseBasicLevel() {
		String html = "<b>Hello</b> <script>alert('xss')</script> <i>World</i>";
		String result = OWASP.sanitise(Sanitisation.basic, html);
		assertThat("Basic sanitizer should allow basic formatting", result, is("<b>Hello</b>  <i>World</i>"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testSanitiseSimpleLevel() {
		String html = "<div><p>Hello</p><script>alert('xss')</script><p>World</p></div>";
		String result = OWASP.sanitise(Sanitisation.simple, html);
		assertThat("Simple sanitizer should allow basic blocks", result, is("<div><p>Hello</p><p>World</p></div>"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testSanitiseNullInput() {
		String result = OWASP.sanitise(Sanitisation.relaxed, null);
		assertThat("Null input should return null", result, is((String)null));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testSanitiseNullSanitisation() {
		String html = "<p>Hello World</p>";
		String result = OWASP.sanitise(null, html);
		assertThat("Null sanitisation should return original input", result, is(html));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testEscapeHtml() {
		String html = "<div>Hello & World</div>";
		String result = OWASP.escapeHtml(html);
		assertThat("HTML should be properly escaped", result, is("&lt;div&gt;Hello &amp; World&lt;/div&gt;"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testEscapeJsonString() {
		String json = "Hello \"World\"\nNew Line";
		String result = OWASP.escapeJsonString(json);
		assertThat("JSON string should be properly escaped", result, is("Hello \\\"World\\\"\\nNew Line"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testEscapeJsString() {
		String js = "Hello 'World'\nNew Line";
		String result = OWASP.escapeJsString(js);
		assertThat("JS string should be properly escaped", result, is("Hello \\'World\\'<br/>New Line"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testEscapeJsStringWithoutNewlines() {
		String js = "Hello 'World'\nNew Line";
		String result = OWASP.escapeJsString(js, true, false);
		assertThat("JS string should be escaped without newlines", result, is("Hello \\'World\\'New Line"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testEscapeJsStringWithoutQuotes() {
		String js = "Hello \"World\"\nNew Line";
		String result = OWASP.escapeJsString(js, false, true);
		assertThat("JS string should be escaped without quotes", result, is("Hello \"World\"<br/>New Line"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testUnescapeHtmlChars() {
		String html = "&lt;div&gt;Hello &amp; World&lt;/div&gt;";
		String result = OWASP.unescapeHtmlChars(html);
		assertThat("HTML entities should be unescaped", result, is("<div>Hello & World</div>"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testSanitiseAndEscapeHtml() {
		String html = "<p>Hello <script>alert('xss')</script> & World</p>";
		String result = OWASP.sanitiseAndEscapeHtml(Sanitisation.basic, html);
		assertThat("HTML should be sanitized and escaped", result, is("Hello  &amp; World"));
	}
}
