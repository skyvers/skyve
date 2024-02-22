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
}
