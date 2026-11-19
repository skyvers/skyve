package org.skyve.impl.util;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.mockito.Mockito.mock;

import java.util.HashMap;
import java.util.Map;

import org.junit.jupiter.api.Test;
import org.skyve.domain.Bean;
import org.skyve.metadata.model.document.Document;

/**
 * Tests for {@link ReportParameters}.
 */
@SuppressWarnings("static-method")
class ReportParametersTest {

	@Test
	void threeArgConstructorStoresValues() {
		Document doc = mock(Document.class);
		Map<String, Object> params = new HashMap<>();
		params.put("key", "value");

		ReportParameters rp = new ReportParameters(doc, "myReport", params);

		assertSame(doc, rp.getDocument());
		assertEquals("myReport", rp.getReportName());
		assertSame(params, rp.getParameters());
		assertNull(rp.getBean());
	}

	@Test
	void fourArgConstructorStoresBean() {
		Document doc = mock(Document.class);
		Bean bean = mock(Bean.class);
		Map<String, Object> params = new HashMap<>();

		ReportParameters rp = new ReportParameters(doc, "report2", params, bean);

		assertSame(doc, rp.getDocument());
		assertEquals("report2", rp.getReportName());
		assertSame(params, rp.getParameters());
		assertSame(bean, rp.getBean());
	}

	@SuppressWarnings({"null", "unused"}) // checks values the annotations say are non-null
	@Test
	void threeArgConstructorWithNullDocumentAndEmptyParams() {
		Map<String, Object> params = new HashMap<>();
		ReportParameters rp = new ReportParameters(null, "nullDocReport", params);

		assertNull(rp.getDocument());
		assertEquals("nullDocReport", rp.getReportName());
		assertSame(params, rp.getParameters());
		assertNull(rp.getBean());
	}

	@Test
	void reportNameIsPreserved() {
		@SuppressWarnings("null") // passes null on purpose
		ReportParameters rp = new ReportParameters(null, "specificReportName", new HashMap<>());
		assertEquals("specificReportName", rp.getReportName());
	}
}
