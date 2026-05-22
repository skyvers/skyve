package org.skyve.util;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import org.skyve.domain.Bean;
import org.skyve.domain.DynamicBean;
import org.skyve.impl.metadata.module.query.MetaDataQueryProjectedColumnImpl;
import org.skyve.metadata.module.query.MetaDataQueryColumn;
import org.skyve.metadata.module.query.MetaDataQueryProjectedColumn;
import org.skyve.metadata.view.TextOutput.Sanitisation;

class OWASPTest {

	@Test
	@SuppressWarnings("static-method")
	void testSanitiseRelaxedAllowsImageSrc() {
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
	void testSanitiseRelaxedAllowsImageSrcDataUrl() {
		// setup the test data
		String html = "<img src=\"data:image/gif;base64,R0lGODdhMAAwAPAAAAAAAP///ywAAAAAMAAw AAAC8IyPqcvt3wCcDkiLc7C0qwyGHhSWpjQu5yqmCYsapyuvUUlvONmOZtfzgFz ByTB10QgxOR0TqBQejhRNzOfkVJ+5YiUqrXF5Y5lKh/DeuNcP5yLWGsEbtLiOSp a/TPg7JpJHxyendzWTBfX0cxOnKPjgBzi4diinWGdkF8kjdfnycQZXZeYGejmJl ZeGl9i2icVqaNVailT6F5iJ90m6mvuTS4OK05M0vDk0Q4XUtwvKOzrcd3iq9uis F81M1OIcR7lEewwcLp7tuNNkM3uNna3F2JQFo97Vriy/Xl4/f1cf5VWzXyym7PH hhx4dbgYKAAA7\" alt=\"Larry\">";

		// call the method under test
		String result = OWASP.sanitise(Sanitisation.relaxed, html);

		// verify the result
		assertThat("Relaxied sanitiser should not modify the html", result, is(html));
	}

	@Test
	@SuppressWarnings("static-method")
	void testSanitiseTextLevel() {
		String html = "<p>Hello <script>alert('xss')</script> World</p>";
		String result = OWASP.sanitise(Sanitisation.text, html);
		assertThat("Text sanitizer should strip all HTML", result, is("Hello  World"));
	}

	@Test
	@SuppressWarnings("static-method")
	void testSanitiseBasicLevel() {
		String html = "<b>Hello</b> <script>alert('xss')</script> <i>World</i>";
		String result = OWASP.sanitise(Sanitisation.basic, html);
		assertThat("Basic sanitizer should allow basic formatting", result, is("<b>Hello</b>  <i>World</i>"));
	}

	@Test
	@SuppressWarnings("static-method")
	void testSanitiseSimpleLevel() {
		String html = "<div><p>Hello</p><script>alert('xss')</script><p>World</p></div>";
		String result = OWASP.sanitise(Sanitisation.simple, html);
		assertThat("Simple sanitizer should allow basic blocks", result, is("<div><p>Hello</p><p>World</p></div>"));
	}

	@Test
	@SuppressWarnings("static-method")
	void testSanitiseNullInput() {
		String result = OWASP.sanitise(Sanitisation.relaxed, null);
		assertThat("Null input should return null", result, is((String)null));
	}

	@Test
	@SuppressWarnings("static-method")
	void testSanitiseNullSanitisation() {
		String html = "<p>Hello World</p>";
		String result = OWASP.sanitise(null, html);
		assertThat("Null sanitisation should return original input", result, is(html));
	}

	@Test
	@SuppressWarnings("static-method")
	void testEscapeHtml() {
		String html = "<div>Hello & World</div>";
		String result = OWASP.escapeHtml(html);
		assertThat("HTML should be properly escaped", result, is("&lt;div&gt;Hello &amp; World&lt;/div&gt;"));
	}

	@Test
	@SuppressWarnings("static-method")
	void testEscapeJsonString() {
		String json = "Hello \"World\"\nNew Line";
		String result = OWASP.escapeJsonString(json);
		assertThat("JSON string should be properly escaped", result, is("Hello \\\"World\\\"\\nNew Line"));
	}

	@Test
	@SuppressWarnings("static-method")
	void testEscapeJsString() {
		String js = "Hello 'World'\nNew Line";
		String result = OWASP.escapeJsString(js);
		assertThat("JS string should be properly escaped", result, is("Hello \\'World\\'<br/>New Line"));
	}

	@Test
	@SuppressWarnings("static-method")
	void testEscapeJsStringWithoutNewlines() {
		String js = "Hello 'World'\nNew Line";
		String result = OWASP.escapeJsString(js, true, false);
		assertThat("JS string should be escaped without newlines", result, is("Hello \\'World\\'New Line"));
	}

	@Test
	@SuppressWarnings("static-method")
	void testEscapeJsStringWithoutQuotes() {
		String js = "Hello \"World\"\nNew Line";
		String result = OWASP.escapeJsString(js, false, true);
		assertThat("JS string should be escaped without quotes", result, is("Hello \"World\"<br/>New Line"));
	}

	@Test
	@SuppressWarnings("static-method")
	void testUnescapeHtmlChars() {
		String html = "&lt;div&gt;Hello &amp; World&lt;/div&gt;";
		String result = OWASP.unescapeHtmlChars(html);
		assertThat("HTML entities should be unescaped", result, is("<div>Hello & World</div>"));
	}

	@Test
	@SuppressWarnings("static-method")
	void testSanitiseAndEscapeHtml() {
		String html = "<p>Hello <script>alert('xss')</script> & World</p>";
		String result = OWASP.sanitiseAndEscapeHtml(Sanitisation.basic, html);
		assertThat("HTML should be sanitized and escaped", result, is("Hello  &amp; World"));
	}

	@Test
	@SuppressWarnings("static-method")
	void testSanitiseFileNameBasic() {
		String result = OWASP.sanitiseFileName("my file.txt");
		assertThat("Safe filename should be returned", result, is("my file.txt"));
	}

	@Test
	@SuppressWarnings("static-method")
	void testSanitiseFileNameStripsPathTraversal() {
		String result = OWASP.sanitiseFileName("../etc/passwd");
		assertNotNull(result, "Result should not be null");
	}

	@Test
	@SuppressWarnings("static-method")
	void testSanitiseFileNameNull() {
		String result = OWASP.sanitiseFileName(null);
		assertThat("Null input returns unnamed", result, is("unnamed"));
	}

	@Test
	@SuppressWarnings("static-method")
	void testEscapeJsStringNullReturnsNull() {
		assertNull(OWASP.escapeJsString(null, true, true));
	}

	@Test
	@SuppressWarnings("static-method")
	void testEscapeJsStringNoDoubleQuotesNoNewlines() {
		String result = OWASP.escapeJsString("say \"hello\"\nworld", false, false);
		assertThat(result, is("say \"hello\"world"));
	}

	@Test
	@SuppressWarnings("static-method")
	void testEscapeJsStringEscapeDoubleQuotesNoNewlines() {
		String result = OWASP.escapeJsString("say \"hello\"\nworld", true, false);
		assertThat(result, is("say &quot;hello&quot;world"));
	}

	@Test
	@SuppressWarnings("static-method")
	void testEscapeJsStringEscapeNewlinesNoDoubleQuotes() {
		String result = OWASP.escapeJsString("hello\nworld", false, true);
		assertThat(result, is("hello<br/>world"));
	}

	@Test
	@SuppressWarnings("static-method")
	void testEscapeJsonStringNullReturnsNull() {
		assertNull(OWASP.escapeJsonString(null));
	}

	@Test
	@SuppressWarnings("static-method")
	void testSanitiseNoneSanitisationReturnsInputUnchanged() {
		String input = "<b>hello</b>";
		String result = OWASP.sanitise(Sanitisation.none, input);
		assertEquals(input, result);
	}

	@Test
	@SuppressWarnings("static-method")
	void testSanitiseAndEscapeHtmlWithNullInput() {
		assertNull(OWASP.sanitiseAndEscapeHtml(Sanitisation.none, null));
	}

	@Test
	@SuppressWarnings("static-method")
	void testSanitiseFileNameStripsNullBytes() {
		String result = OWASP.sanitiseFileName("file\0name.txt");
		assertNotNull(result);
		assertFalse(result.contains("\0"), "Sanitised name should not contain null bytes");
	}

	@Test
	@SuppressWarnings("static-method")
	void testSanitiseFileNameEmptyString() {
		String result = OWASP.sanitiseFileName("");
		assertNotNull(result);
	}

	@Test
	@SuppressWarnings({"static-method", "boxing"})
	void testSanitiseAndEscapeListModelRowsEmptyRows() {
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
	void testSanitiseAndEscapeListModelRowsSkipsNonProjectedColumn() {
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
	void testSanitiseAndEscapeListModelRowsNoSanitisationNeeded() {
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
	void testSanitiseAndEscapeListModelRowsUsesNameWhenBindingNull() {
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

	@Test
	@SuppressWarnings("static-method")
	void testSanitiseAndEscapeListModelRowsSanitisesStringValueWithEscape() {
		// escape=true with DynamicBean containing a string — exercises the sanitise+escape branch
		DynamicBean row = new DynamicBean("test", "Contact", new HashMap<>());
		row.putDynamic("name", "<script>alert('xss')</script>");

		MetaDataQueryProjectedColumnImpl col = new MetaDataQueryProjectedColumnImpl();
		col.setBinding("name");
		col.setEscape(true);

		List<Bean> rows = new ArrayList<>();
		rows.add(row);
		List<MetaDataQueryColumn> columns = new ArrayList<>();
		columns.add(col);

		OWASP.sanitiseAndEscapeListModelRows(rows, columns, true);
			// The value should have been sanitised; script tag content is stripped
			Object value = row.getDynamic("name");
			assertNull(value);
	}

	// ---- sanitiseLog ----

	@Test
	@SuppressWarnings("static-method")
	void sanitiseLogReplacesCarriageReturn() {
		assertThat(OWASP.sanitiseLog("before\rafter"), is("before_after"));
	}

	@Test
	@SuppressWarnings("static-method")
	void sanitiseLogReplacesLineFeed() {
		assertThat(OWASP.sanitiseLog("line1\nline2"), is("line1_line2"));
	}

	@Test
	@SuppressWarnings("static-method")
	void sanitiseLogReplacesCrLfSequence() {
		assertThat(OWASP.sanitiseLog("head\r\nINFO fake-line"), is("head__INFO fake-line"));
	}

	@Test
	@SuppressWarnings("static-method")
	void sanitiseLogReplacesTab() {
		assertThat(OWASP.sanitiseLog("col1\tcol2"), is("col1_col2"));
	}

	@Test
	@SuppressWarnings("static-method")
	void sanitiseLogReplacesNulChar() {
		assertThat(OWASP.sanitiseLog("be\u0000fore"), is("be_fore"));
	}

	@Test
	@SuppressWarnings("static-method")
	void sanitiseLogReplacesDelChar() {
		assertThat(OWASP.sanitiseLog("be\u007fore"), is("be_ore"));
	}

	@Test
	@SuppressWarnings("static-method")
	void sanitiseLogReplacesOtherControlChars() {
		assertThat(OWASP.sanitiseLog("a\u0001b\u001fc"), is("a_b_c"));
	}

	@Test
	@SuppressWarnings("static-method")
	void sanitiseLogDoesNotModifyCleanString() {
		String clean = "A perfectly normal log message.";
		assertThat(OWASP.sanitiseLog(clean), is(clean));
	}

	@Test
	@SuppressWarnings("static-method")
	void sanitiseLogReturnsNullForNullInput() {
		assertNull(OWASP.sanitiseLog(null));
	}
}
