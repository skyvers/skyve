package org.skyve.bizport;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.Test;
import org.skyve.bizport.BizPortWorkbook.BizPortFormat;

public class BizPortFormatTest {

	@Test
	@SuppressWarnings("static-method")
	public void valuesContainsTwoFormats() {
		assertEquals(2, BizPortFormat.values().length);
	}

	@Test
	@SuppressWarnings("static-method")
	public void valueOfXls() {
		assertNotNull(BizPortFormat.valueOf("xls"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void valueOfXlsx() {
		assertNotNull(BizPortFormat.valueOf("xlsx"));
	}
}
