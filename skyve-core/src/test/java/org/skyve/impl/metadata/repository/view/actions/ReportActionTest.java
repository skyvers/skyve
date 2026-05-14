package org.skyve.impl.metadata.repository.view.actions;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.report.ReportFormat;

public class ReportActionTest {

	@Test
	@SuppressWarnings("static-method")
	public void defaultConstructorSetsImplicitName() {
		ReportAction action = new ReportAction();
		assertThat(action.getImplicitName(), is(ImplicitActionName.Report));
	}

	@Test
	@SuppressWarnings("static-method")
	public void setModuleNameRoundtrip() {
		ReportAction action = new ReportAction();
		assertNull(action.getModuleName());
		action.setModuleName("admin");
		assertThat(action.getModuleName(), is("admin"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void setDocumentNameRoundtrip() {
		ReportAction action = new ReportAction();
		action.setDocumentName("User");
		assertThat(action.getDocumentName(), is("User"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void setReportNameRoundtrip() {
		ReportAction action = new ReportAction();
		action.setReportName("UserReport");
		assertThat(action.getReportName(), is("UserReport"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void setReportFormatRoundtrip() {
		ReportAction action = new ReportAction();
		assertNull(action.getReportFormat());
		action.setReportFormat(ReportFormat.pdf);
		assertThat(action.getReportFormat(), is(ReportFormat.pdf));
	}

	@Test
	@SuppressWarnings("static-method")
	public void setListReportRoundtrip() {
		ReportAction action = new ReportAction();
		assertNull(action.isListReport());
		action.setListReport(Boolean.TRUE);
		assertThat(action.isListReport(), is(Boolean.TRUE));
	}

	@Test
	@SuppressWarnings("static-method")
	public void setQueryNameRoundtrip() {
		ReportAction action = new ReportAction();
		assertNull(action.getQueryName());
		action.setQueryName("myQuery");
		assertThat(action.getQueryName(), is("myQuery"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void setModelNameRoundtrip() {
		ReportAction action = new ReportAction();
		assertNull(action.getModelName());
		action.setModelName("myModel");
		assertThat(action.getModelName(), is("myModel"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void toMetaDataActionBuildsParameters() {
		ReportAction action = new ReportAction();
		action.setModuleName("admin");
		action.setDocumentName("User");
		action.setReportName("userReport");
		action.setReportFormat(ReportFormat.xls);
		action.setQueryName("qUser");
		action.setModelName("mUser");
		assertNotNull(action.toMetaDataAction());
		assertNotNull(action.toMetaDataAction().getParameters());
	}
}
