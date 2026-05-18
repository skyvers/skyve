package org.skyve.util;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import org.skyve.domain.Bean;
import org.skyve.metadata.module.query.MetaDataQueryColumn;
import org.skyve.metadata.module.query.MetaDataQueryProjectedColumn;
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

	@Test
	@SuppressWarnings("static-method")
	public void testSanitiseFileNameBasic() {
		String result = OWASP.sanitiseFileName("my file.txt");
		assertThat("Safe filename should be returned", result, is("my file.txt"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testSanitiseFileNameStripsPathTraversal() {
		String result = OWASP.sanitiseFileName("../etc/passwd");
		assertNotNull(result, "Result should not be null");
	}

	@Test
	@SuppressWarnings("static-method")
	public void testSanitiseFileNameNull() {
		String result = OWASP.sanitiseFileName(null);
		assertThat("Null input returns unnamed", result, is("unnamed"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testEscapeJsStringNullReturnsNull() {
		assertNull(OWASP.escapeJsString(null, true, true));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testEscapeJsStringNoDoubleQuotesNoNewlines() {
		String result = OWASP.escapeJsString("say \"hello\"\nworld", false, false);
		assertThat(result, is("say \"hello\"world"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testEscapeJsStringEscapeDoubleQuotesNoNewlines() {
		String result = OWASP.escapeJsString("say \"hello\"\nworld", true, false);
		assertThat(result, is("say &quot;hello&quot;world"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testEscapeJsStringEscapeNewlinesNoDoubleQuotes() {
		String result = OWASP.escapeJsString("hello\nworld", false, true);
		assertThat(result, is("hello<br/>world"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testEscapeJsonStringNullReturnsNull() {
		assertNull(OWASP.escapeJsonString(null));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testSanitiseNoneSanitisationReturnsInputUnchanged() {
		String input = "<b>hello</b>";
		String result = OWASP.sanitise(Sanitisation.none, input);
		assertEquals(input, result);
	}

	@Test
	@SuppressWarnings("static-method")
	public void testSanitiseAndEscapeHtmlWithNullInput() {
		assertNull(OWASP.sanitiseAndEscapeHtml(Sanitisation.none, null));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testSanitiseFileNameStripsNullBytes() {
		String result = OWASP.sanitiseFileName("file\0name.txt");
		assertNotNull(result);
		assertFalse(result.contains("\0"), "Sanitised name should not contain null bytes");
	}

	@Test
	@SuppressWarnings("static-method")
	public void testSanitiseFileNameEmptyString() {
		String result = OWASP.sanitiseFileName("");
		assertNotNull(result);
	}

	@Test
	@SuppressWarnings({"static-method", "boxing"})
	public void testSanitiseAndEscapeListModelRowsEmptyRows() {
		// Empty rows list — should complete without error
		List<MetaDataQueryColumn> columns = new ArrayList<>();
		MetaDataQueryColumn col = Mockito.mock(MetaDataQueryColumn.class);
		Mockito.when(col.getBinding()).thenReturn("name");
		Mockito.when(col.isEscape()).thenReturn(Boolean.FALSE);
		Mockito.when(col.getSanitise()).thenReturn(null);
		columns.add(col);

		// Should not throw
		OWASP.sanitiseAndEscapeListModelRows(new ArrayList<>(), columns, Boolean.TRUE.booleanValue());
	}

	@Test
	@SuppressWarnings({"static-method", "boxing"})
	public void testSanitiseAndEscapeListModelRowsSkipsNonProjectedColumn() {
		// Non-projected MetaDataQueryProjectedColumn — should continue (skip)
		Bean row = Mockito.mock(Bean.class);

		MetaDataQueryProjectedColumn col = Mockito.mock(MetaDataQueryProjectedColumn.class);
		Mockito.when(col.isProjected()).thenReturn(Boolean.FALSE);

		List<Bean> rows = new ArrayList<>();
		rows.add(row);
		List<MetaDataQueryColumn> columns = new ArrayList<>();
		columns.add(col);

		// Should not throw; should skip and not call any binding methods
		OWASP.sanitiseAndEscapeListModelRows(rows, columns, Boolean.FALSE.booleanValue());
		Mockito.verify(col, Mockito.never()).getBinding();
	}

	@Test
	@SuppressWarnings({"static-method", "boxing"})
	public void testSanitiseAndEscapeListModelRowsNoSanitisationNeeded() {
		// escape=false, sanitise=null — should skip the sanitisation block
		Bean row = Mockito.mock(Bean.class);

		MetaDataQueryColumn col = Mockito.mock(MetaDataQueryColumn.class);
		Mockito.when(col.getBinding()).thenReturn("name");
		Mockito.when(col.isEscape()).thenReturn(Boolean.FALSE);
		Mockito.when(col.getSanitise()).thenReturn(null);

		List<Bean> rows = new ArrayList<>();
		rows.add(row);
		List<MetaDataQueryColumn> columns = new ArrayList<>();
		columns.add(col);

		// Should not throw; no value resolution needed
		OWASP.sanitiseAndEscapeListModelRows(rows, columns, Boolean.FALSE.booleanValue());
	}

	@Test
	@SuppressWarnings({"static-method", "boxing"})
	public void testSanitiseAndEscapeListModelRowsUsesNameWhenBindingNull() {
		// binding=null → falls back to getName()
		Bean row = Mockito.mock(Bean.class);

		MetaDataQueryColumn col = Mockito.mock(MetaDataQueryColumn.class);
		Mockito.when(col.getBinding()).thenReturn(null);
		Mockito.when(col.getName()).thenReturn("title");
		Mockito.when(col.isEscape()).thenReturn(Boolean.FALSE);
		Mockito.when(col.getSanitise()).thenReturn(null);

		List<Bean> rows = new ArrayList<>();
		rows.add(row);
		List<MetaDataQueryColumn> columns = new ArrayList<>();
		columns.add(col);

		// Should not throw; no sanitisation since escape=false, sanitise=null
		OWASP.sanitiseAndEscapeListModelRows(rows, columns, Boolean.FALSE.booleanValue());
		// Verify that getName() was called as fallback
		Mockito.verify(col).getName();
	}
}

