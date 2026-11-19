package org.skyve.impl.generate.jasperreports;

import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class DocumentReportDesignGeneratorTest {

	@Test
	void defaultConstructorCreatesInstance() {
		assertThat(new DocumentReportDesignGenerator(), notNullValue());
	}

	@Test
	void addBandsWithASubreportWithoutACollectionFieldThrowsIllegalState() {
		DesignSpecification design = new DesignSpecification();
		design.setName("Report");
		DesignSpecification sub = new DesignSpecification();
		sub.setName("Subreport");
		design.getSubReports().add(sub);
		DocumentReportDesignGenerator gen = new DocumentReportDesignGenerator();

		assertThrows(IllegalStateException.class, () -> gen.addBands(design));
	}

	@Test
	void getSubreportGeneratorReturnsDocumentReportDesignGeneratorInstance() {
		DocumentReportDesignGenerator gen = new DocumentReportDesignGenerator();
		assertThat(gen.getSubreportGenerator(), instanceOf(DocumentReportDesignGenerator.class));
	}
}
