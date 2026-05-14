package org.skyve.impl.generate.jasperreports;

import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
public class ListReportDesignGeneratorTest {

	@Test
	public void getSubreportGeneratorThrowsUnsupportedOperationException() {
		assertThrows(UnsupportedOperationException.class, new ListReportDesignGenerator()::getSubreportGenerator);
	}
}
