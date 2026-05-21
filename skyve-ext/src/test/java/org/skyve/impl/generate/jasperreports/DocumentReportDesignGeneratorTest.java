package org.skyve.impl.generate.jasperreports;

import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class DocumentReportDesignGeneratorTest {

	@Test
	void defaultConstructorCreatesInstance() {
		assertThat(new DocumentReportDesignGenerator(), notNullValue());
	}

	@Test
	void getSubreportGeneratorReturnsDocumentReportDesignGeneratorInstance() {
		DocumentReportDesignGenerator gen = new DocumentReportDesignGenerator();
		assertThat(gen.getSubreportGenerator(), instanceOf(DocumentReportDesignGenerator.class));
	}
}
