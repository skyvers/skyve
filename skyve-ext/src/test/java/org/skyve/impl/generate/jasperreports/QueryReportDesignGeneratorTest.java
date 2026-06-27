package org.skyve.impl.generate.jasperreports;

import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class QueryReportDesignGeneratorTest {

	@Test
	void getSubreportGeneratorThrowsUnsupportedOperationException() {
		QueryReportDesignGenerator generator = new QueryReportDesignGenerator();
		assertThrows(UnsupportedOperationException.class, generator::getSubreportGenerator);
	}
}
