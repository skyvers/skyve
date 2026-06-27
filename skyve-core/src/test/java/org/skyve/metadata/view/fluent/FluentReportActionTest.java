package org.skyve.metadata.view.fluent;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.repository.view.actions.ReportAction;
import org.skyve.impl.metadata.view.widget.bound.ParameterImpl;
import org.skyve.report.ReportFormat;

/**
 * Tests for {@link FluentReportAction}: constructors and setters.
 */
@SuppressWarnings("static-method")
class FluentReportActionTest {

	@Test
	void defaultConstructorCreatesInstance() {
		assertNotNull(new FluentReportAction().get());
	}

	@Test
	void wrappingConstructorPreservesInstance() {
		ReportAction action = new ReportAction();
		FluentReportAction fra = new FluentReportAction(action);
		assertEquals(action, fra.get());
	}

	@Test
	void moduleNameSetsValue() {
		FluentReportAction fra = new FluentReportAction().moduleName("admin");
		assertEquals("admin", fra.get().getModuleName());
	}

	@Test
	void documentNameSetsValue() {
		FluentReportAction fra = new FluentReportAction().documentName("Contact");
		assertEquals("Contact", fra.get().getDocumentName());
	}

	@Test
	void reportNameSetsValue() {
		FluentReportAction fra = new FluentReportAction().reportName("ContactReport");
		assertEquals("ContactReport", fra.get().getReportName());
	}

	@Test
	void reportFormatSetsValue() {
		FluentReportAction fra = new FluentReportAction().reportFormat(ReportFormat.pdf);
		assertEquals(ReportFormat.pdf, fra.get().getReportFormat());
	}

	@Test
	void reportFormatCsvSetsValue() {
		FluentReportAction fra = new FluentReportAction().reportFormat(ReportFormat.csv);
		assertEquals(ReportFormat.csv, fra.get().getReportFormat());
	}

	@Test
	void listReportTrueSetsFlag() {
		FluentReportAction fra = new FluentReportAction().listReport(true);
		assertThat(fra.get().isListReport(), is(Boolean.TRUE));
	}

	@Test
	void listReportFalseSetsFlag() {
		FluentReportAction fra = new FluentReportAction().listReport(false);
		assertThat(fra.get().isListReport(), is(Boolean.FALSE));
	}

	@Test
	void queryNameSetsValue() {
		FluentReportAction fra = new FluentReportAction().queryName("qContacts");
		assertEquals("qContacts", fra.get().getQueryName());
	}

	@Test
	void modelNameSetsValue() {
		FluentReportAction fra = new FluentReportAction().modelName("ContactsModel");
		assertEquals("ContactsModel", fra.get().getModelName());
	}

	@Test
	void fromCallsParentFromAndReturnsSelf() {
		ReportAction source = new ReportAction();
		source.setDisplayName("Print Report");

		FluentReportAction fra = new FluentReportAction().from(source);

		// from() delegates to FluentParameterizableAction.from() which copies
		// base action properties such as displayName
		assertNotNull(fra);
		assertEquals("Print Report", fra.get().getDisplayName());
	}

	@Test
	void fromCopiesParametersFromSource() {
		ReportAction source = new ReportAction();
		ParameterImpl param = new ParameterImpl();
		param.setName("reportParam");
		param.setValue("value1");
		source.getParameters().add(param);

		FluentReportAction fra = new FluentReportAction().from(source);
		assertEquals(1, fra.get().getParameters().size());
		assertEquals("reportParam", fra.get().getParameters().get(0).getName());
	}

	@Test
	void addParameterAddsToAction() {
		FluentParameter fp = new FluentParameter().name("p1").value("v1");
		FluentReportAction fra = new FluentReportAction().addParameter(fp);
		assertEquals(1, fra.get().getParameters().size());
		assertEquals("p1", fra.get().getParameters().get(0).getName());
	}
}
