package modules.admin.ReportTemplate.jobs;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.Test;
import org.skyve.domain.messages.DomainException;

import modules.admin.domain.ReportTemplate;
import util.AbstractH2Test;

@SuppressWarnings("static-method")
class ReportJobTest extends AbstractH2Test {
	@Test
	void executeReportThrowsWhenTemplateNoLongerExists() {
		ReportTemplate missing = ReportTemplate.newInstance();
		missing.setBizId("missing-report-template");
		ReportJob job = new ReportJob();
		job.setBean(missing);

		DomainException thrown = assertThrows(DomainException.class, job::executeReport);

		assertThat(thrown.getMessage(), containsString("Report template missing-report-template does not exist"));
	}
}
