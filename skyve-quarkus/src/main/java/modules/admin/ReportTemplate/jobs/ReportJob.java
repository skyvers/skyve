package modules.admin.ReportTemplate.jobs;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.io.StringWriter;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.beanutils.DynaBean;
import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.content.MimeType;
import org.skyve.domain.Bean;
import org.skyve.domain.app.admin.ReportDataset.DatasetType;
import org.skyve.impl.util.UtilImpl;
import org.skyve.job.Job;
import org.skyve.util.Binder;
import org.skyve.util.CommunicationUtil;
import org.skyve.util.MailAttachment;

import freemarker.template.Template;
import modules.admin.ReportDataset.ReportDatasetExtension;
import modules.admin.domain.Contact;
import modules.admin.domain.ReportTemplate;
import modules.admin.domain.User;
import modules.admin.domain.UserProxy;

public class ReportJob extends Job {
	public static final String SYSTEM_SCHEDULED_REPORT_EMAIL = "SYSTEM Scheduled Report";
	public static final String SYSTEM_SCHEDULED_REPORT_EMAIL_DEFAULT_SUBJECT = String.format("Scheduled Report {%s}",
			ReportTemplate.namePropertyName);
	public static final String SYSTEM_SCHEDULED_REPORT_EMAIL_DEFAULT_BODY = String.format("Hi {%s}, <br /><br />" +
					"Please find attached your copy of a scheduled report.",
			Binder.createCompoundBinding(User.contactPropertyName, Contact.namePropertyName));

	@Override
	public void execute() throws Exception {
		executeReport();
	}

	public void executeReport() throws Exception {
		UtilImpl.inject(this);

		final ReportTemplate report = CORE.getPersistence().retrieve(ReportTemplate.MODULE_NAME, ReportTemplate.DOCUMENT_NAME, getBean().getBizId());

		// TODO: Diverge from Jasper
		Template template = EXT.getReporting().getFreemarkerTemplate(report.getTemplateName());

		Map<String, Object> root = new HashMap<>();

		// put the parameters into the root
		root.put("reportParameters", report.getParameters());

		// put all the datasets into the root
		for (ReportDatasetExtension dataset : report.getDatasets()) {
			DatasetType type = dataset.getDatasetType();
			if (type != null) {
				switch (type) {
					case bizQL:
						List<Bean> results = dataset.executeQuery();
						if (results != null) {
							root.put(dataset.getDatasetName(), results);
						}
						break;
					case SQL:
						List<DynaBean> sqlResults = dataset.executeSQLQuery();
						if (sqlResults != null) {
							root.put(dataset.getDatasetName(), sqlResults);
						}
						break;
					case constant:
						root.put(dataset.getDatasetName(), dataset.getQuery());
						break;
					case classValue:
						List<DynaBean> beanResults = dataset.executeClass();
						if (beanResults != null) {
							root.put(dataset.getDatasetName(), beanResults);
						}
						break;
					default:
						throw new IllegalStateException(type + " is not catered for");
				}
			}
		}

		try (StringWriter sw = new StringWriter()) {
			template.process(root, sw);

			// write the output string to an input stream
			final byte[] reportBytes = sw.toString().getBytes(StandardCharsets.UTF_8);

			final MailAttachment reportAttachment;
			// return the correct report type
			if (ReportTemplate.OutputFormat.CSV == report.getOutputFormat()) {
				reportAttachment = new MailAttachment(String.format("%s.csv", report.getName()), reportBytes, MimeType.csv);
			} else if (ReportTemplate.OutputFormat.PDF == report.getOutputFormat()) {
				try (final ByteArrayOutputStream out = new ByteArrayOutputStream()) {
					try (final InputStream in = new ByteArrayInputStream(reportBytes)) {
						EXT.getReporting().generateFreemarkerPDFFromHTML(in, out);
						reportAttachment = new MailAttachment(String.format("%s.pdf", report.getName()), out.toByteArray(), MimeType.pdf);
					}
				}
			} else {
				throw new RuntimeException("Unsupported format.");
			}

			final List<Exception> exceptions = new ArrayList<>();
			for (UserProxy userToEmail : report.getUsersToEmail()) {
				try {
					CommunicationUtil.sendFailSafeSystemCommunication(SYSTEM_SCHEDULED_REPORT_EMAIL,
							SYSTEM_SCHEDULED_REPORT_EMAIL_DEFAULT_SUBJECT,
							SYSTEM_SCHEDULED_REPORT_EMAIL_DEFAULT_BODY,
							CommunicationUtil.ResponseMode.EXPLICIT, new MailAttachment[] { reportAttachment }, userToEmail,
							report);
					getLog().add(String.format("Successfully emailed report to %s.", userToEmail.getContact().getEmail1()));
				} catch (Exception e) {
					getLog().add(String.format("Failed to email report to %s.", userToEmail.getContact().getEmail1()));
					exceptions.add(e);
				}
			}

			if (!exceptions.isEmpty()) {
				throw new RuntimeException("Failed to send report to some users.");
			}
		}
	}
}
