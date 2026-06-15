package org.skyve.bizport;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.Test;
import org.skyve.bizport.BizPortWorkbook.BizPortFormat;

class BizPortFormatTest {

	@Test
	@SuppressWarnings("static-method")
	void valuesContainsTwoFormats() {
		assertEquals(2, BizPortFormat.values().length);
	}

	@Test
	@SuppressWarnings("static-method")
	void valueOfXls() {
		assertNotNull(BizPortFormat.valueOf("xls"));
	}

	@Test
	@SuppressWarnings("static-method")
	void valueOfXlsx() {
		assertNotNull(BizPortFormat.valueOf("xlsx"));
	}
}
