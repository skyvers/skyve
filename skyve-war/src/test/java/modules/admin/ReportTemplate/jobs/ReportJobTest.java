package modules.admin.ReportTemplate.jobs;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.io.StringReader;
import java.nio.charset.StandardCharsets;

import org.junit.jupiter.api.Test;
import org.skyve.content.MimeType;
import org.skyve.domain.app.admin.ReportDataset.DatasetType;
import org.skyve.domain.messages.DomainException;
import org.skyve.util.MailAttachment;

import freemarker.template.Configuration;
import freemarker.template.Template;
import modules.admin.Contact.ContactExtension;
import modules.admin.ReportDataset.ReportDatasetExtension;
import modules.admin.UserProxy.UserProxyExtension;
import modules.admin.domain.Contact;
import modules.admin.domain.ReportTemplate;
import modules.admin.domain.UserProxy;
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

	@Test
	void executeDelegatesToExecuteReport() {
		ReportTemplate missing = ReportTemplate.newInstance();
		missing.setBizId("missing-report-template");
		ReportJob job = new ReportJob();
		job.setBean(missing);

		assertThrows(DomainException.class, job::execute);
	}

	@Test
	void executeReportSendsCsvAttachmentForConstantDataset() throws Exception {
		ReportTemplate report = report(ReportTemplate.OutputFormat.CSV);
		ReportDatasetExtension dataset = new ReportDatasetExtension();
		dataset.setDatasetType(DatasetType.constant);
		dataset.setDatasetName("rows");
		dataset.setQuery("Alpha,3");
		report.getDatasets().add(dataset);
		TestableReportJob job = new TestableReportJob(report, "name,count\n${rows}");
		job.setBean(report);

		job.executeReport();

		assertEquals(1, job.sentCount);
		assertEquals("Usage Summary.csv", job.attachment.getAttachmentFileName());
		assertEquals(MimeType.csv.toString(), job.attachment.getAttachmentContentType());
		assertEquals("name,count\nAlpha,3", new String(job.attachment.getAttachment(), StandardCharsets.UTF_8));
		assertThat(job.getLog().get(0), containsString("Successfully emailed report to person@example.com"));
	}

	@Test
	void executeReportGeneratesPdfAttachment() throws Exception {
		ReportTemplate report = report(ReportTemplate.OutputFormat.PDF);
		TestableReportJob job = new TestableReportJob(report, "<html>Report</html>");
		job.setBean(report);

		job.executeReport();

		assertEquals("Usage Summary.pdf", job.attachment.getAttachmentFileName());
		assertEquals(MimeType.pdf.toString(), job.attachment.getAttachmentContentType());
		assertArrayEquals("pdf:<html>Report</html>".getBytes(StandardCharsets.UTF_8), job.attachment.getAttachment());
	}

	@Test
	void executeReportThrowsWhenOutputFormatUnsupported() {
		ReportTemplate report = report(null);
		TestableReportJob job = new TestableReportJob(report, "content");
		job.setBean(report);

		RuntimeException thrown = assertThrows(RuntimeException.class, job::executeReport);

		assertEquals("Unsupported format.", thrown.getMessage());
	}

	@Test
	void executeReportLogsRecipientFailureAndThrowsAfterSendingAttempts() {
		ReportTemplate report = report(ReportTemplate.OutputFormat.CSV);
		TestableReportJob job = new TestableReportJob(report, "content");
		job.failSend = true;
		job.setBean(report);

		RuntimeException thrown = assertThrows(RuntimeException.class, job::executeReport);

		assertEquals("Failed to send report to some users.", thrown.getMessage());
		assertEquals(1, job.sentCount);
		assertThat(job.getLog().get(0), containsString("Failed to email report to person@example.com"));
	}

	private static ReportTemplate report(ReportTemplate.OutputFormat outputFormat) {
		ReportTemplate report = ReportTemplate.newInstance();
		report.setBizId("report-id");
		report.setName("Usage Summary");
		report.setTemplateName("usage.ftl");
		report.setOutputFormat(outputFormat);
		report.getUsersToEmail().add(user("person@example.com"));
		return report;
	}

	private static UserProxyExtension user(String email) {
		UserProxyExtension user = UserProxy.newInstance();
		ContactExtension contact = Contact.newInstance();
		contact.setEmail1(email);
		user.setContact(contact);
		return user;
	}

	private static class TestableReportJob extends ReportJob {
		private final ReportTemplate report;
		private final String templateText;
		private MailAttachment attachment;
		private int sentCount;
		private boolean failSend;

		private TestableReportJob(ReportTemplate report, String templateText) {
			this.report = report;
			this.templateText = templateText;
		}

		@Override
		protected void injectDependencies() {
			// Dependencies are supplied by this test subclass.
		}

		@Override
		protected ReportTemplate retrieveReport(String bizId) {
			return report;
		}

		@Override
		protected Template getFreemarkerTemplate(String templateName) throws Exception {
			return new Template(templateName, new StringReader(templateText), new Configuration(Configuration.VERSION_2_3_32));
		}

		@Override
		protected void generateFreemarkerPDFFromHTML(InputStream in, ByteArrayOutputStream out) throws Exception {
			out.write("pdf:".getBytes(StandardCharsets.UTF_8));
			in.transferTo(out);
		}

		@Override
		protected void sendReport(MailAttachment reportAttachment, UserProxy userToEmail, ReportTemplate reportToSend)
				throws Exception {
			sentCount++;
			attachment = reportAttachment;
			if (failSend) {
				throw new IllegalStateException("mail unavailable");
			}
		}
	}
}
