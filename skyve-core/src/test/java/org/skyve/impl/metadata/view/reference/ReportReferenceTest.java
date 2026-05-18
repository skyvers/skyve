package org.skyve.impl.metadata.view.reference;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;
import org.skyve.report.ReportFormat;

class ReportReferenceTest {

	@Test
	@SuppressWarnings("static-method")
	void setModuleNameRoundtrip() {
		ReportReference ref = new ReportReference();
		ref.setModuleName("admin");
		assertThat(ref.getModuleName(), is("admin"));
	}

	@Test
	@SuppressWarnings("static-method")
	void setDocumentNameRoundtrip() {
		ReportReference ref = new ReportReference();
		ref.setDocumentName("Contact");
		assertThat(ref.getDocumentName(), is("Contact"));
	}

	@Test
	@SuppressWarnings("static-method")
	void setReportNameRoundtrip() {
		ReportReference ref = new ReportReference();
		ref.setReportName("ContactDetail");
		assertThat(ref.getReportName(), is("ContactDetail"));
	}

	@Test
	@SuppressWarnings("static-method")
	void setFormatRoundtrip() {
		ReportReference ref = new ReportReference();
		ref.setFormat(ReportFormat.pdf);
		assertThat(ref.getFormat(), is(ReportFormat.pdf));
	}

	@Test
	@SuppressWarnings("static-method")
	void defaultFormatIsNull() {
		ReportReference ref = new ReportReference();
		assertNull(ref.getFormat());
	}

	@Test
	@SuppressWarnings("static-method")
	void parametersListIsInitiallyEmpty() {
		ReportReference ref = new ReportReference();
		assertTrue(ref.getParameters().isEmpty());
	}

	@Test
	@SuppressWarnings("static-method")
	void blankModuleNameBecomesNull() {
		ReportReference ref = new ReportReference();
		ref.setModuleName("   ");
		assertNull(ref.getModuleName());
	}
}
