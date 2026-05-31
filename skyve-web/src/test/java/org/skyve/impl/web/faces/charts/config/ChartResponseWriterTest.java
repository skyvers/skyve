package org.skyve.impl.web.faces.charts.config;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.io.StringWriter;

import org.junit.jupiter.api.Test;

@SuppressWarnings({"static-method", "resource"})
class ChartResponseWriterTest {

	@Test
	void writerReportsJsonMetadataAndBuffersText() throws Exception {
		try (ChartResponseWriter writer = new ChartResponseWriter(); StringWriter target = new StringWriter()) {
			assertEquals("application/json", writer.getContentType());
			assertEquals("UTF-8", writer.getCharacterEncoding());
			assertNull(writer.cloneWithWriter(target));

			writer.writeText(",\"series\":[]", null);
			writer.write(new char[] {',', '"', 'x', '"', ':', '1'}, 0, 6);
			writer.flush();

			assertEquals(",\"series\":[],\"x\":1", writer.getStringWriter().toString());
		}
	}

	@Test
	void spruceConvertsLeadingCommaToJsonObjectBraces() throws Exception {
		try (ChartResponseWriter writer = new ChartResponseWriter()) {
			writer.writeText(",\"labels\":[\"A\"]", null);

			assertEquals("{\"labels\":[\"A\"]}", ChartConfigRenderer.spruce(writer));
		}
	}

	@Test
	void writeTextNullThrowsNullPointerException() throws Exception {
		try (ChartResponseWriter writer = new ChartResponseWriter()) {
			assertThrows(NullPointerException.class, () -> writer.writeText(null, null));
		}
	}
}