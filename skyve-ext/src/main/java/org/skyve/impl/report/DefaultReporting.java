package org.skyve.impl.report;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.List;
import java.util.Map;

import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.content.MimeType;
import org.skyve.domain.Bean;
import org.skyve.impl.report.freemarker.FreemarkerReportUtil;
import org.skyve.impl.report.jasperreports.JasperReportUtil;
import org.skyve.impl.util.ReportParameters;
import org.skyve.metadata.controller.DownloadAction.Download;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.user.User;
import org.skyve.persistence.Persistence;
import org.skyve.report.ReportFormat;
import org.skyve.report.Reporting;
import org.skyve.util.MailAttachment;

import freemarker.template.Template;
import net.sf.jasperreports.engine.JasperPrint;

public class DefaultReporting implements Reporting {
	private static final DefaultReporting INSTANCE = new DefaultReporting();
	
	public static DefaultReporting get() {
		return INSTANCE;
	}
	
	/**
	 * Disallow external instantiation
	 */
	private DefaultReporting() {
		// nothing to do here
	}

	@Override
	public void startup() {
		FreemarkerReportUtil.init();
	}

	@Override
	public void shutdown() {
		// nothing to do
	}
	
	@Override
	public JasperPrint runJasperBeanReport(User user, Document document, String reportName, Map<String, Object> parameters, Bean bean, ReportFormat format, OutputStream out)
	throws Exception {
		return JasperReportUtil.runBeanReport(user, document, reportName, parameters, bean, format, out);
	}

	@Override
	public JasperPrint runJasperSQLReport(User user, Document document, String reportName, Map<String, Object> parameters, ReportFormat format, OutputStream out)
	throws Exception {
		return JasperReportUtil.runSQLReport(user, document, reportName, parameters, format, out);
	}

	@Override
	public JasperPrint runJasperReport(User user, Document document, String reportName, Map<String, Object> parameters, Bean bean, ReportFormat format, OutputStream out)
	throws Exception {
		return JasperReportUtil.runReport(user, document, reportName, parameters, bean, format, out);
	}

	@Override
	public List<JasperPrint> runJasperReport(User user, List<ReportParameters> reportParameters, ReportFormat format, OutputStream out)
	throws Exception {
		return JasperReportUtil.runReport(user, reportParameters, format, out);
	}

	@Override
	public void runJasperReport(JasperPrint jasperPrint, ReportFormat format, OutputStream out)
	throws Exception {
		JasperReportUtil.runReport(jasperPrint, format, out);
	}

	@Override
	public void runJasperReport(List<JasperPrint> jasperPrintList, ReportFormat format, OutputStream out)
	throws Exception {
		JasperReportUtil.runReport(jasperPrintList, format, out);
	}

	@Override
	public Template getFreemarkerTemplate(String templateName) throws Exception {
		return FreemarkerReportUtil.getTemplate(templateName);
	}
	
	@Override
	public void generateFreemarkerPDFFromHTML(InputStream in, File outputFile) throws Exception {
		FreemarkerReportUtil.generatePDFFromHTML(in, outputFile);
	}
	
	@Override
	public void generateFreemarkerPDFFromHTML(String url, File outputFile) throws Exception {
		FreemarkerReportUtil.generatePDFFromHTML(url, outputFile);
	}

	@Override
	public final void generateFreemarkerPDFFromHTML(InputStream in, OutputStream out) throws Exception {
		FreemarkerReportUtil.generatePDFFromHTML(in, out);
	}

	@Override
	public String createFreemarkerBeanReport(final Bean bean, final String reportName, final Map<String, Object> reportParameters)
	throws Exception {
		return FreemarkerReportUtil.createBeanReport(bean, reportName, reportParameters);
	}
	
	@Override
	public File createFreemarkerBeanReportPDF(final Bean bean,
												final String reportName,
												final Map<String, Object> reportParameters,
												final String reportFilename)
	throws Exception {
		return FreemarkerReportUtil.createBeanReportPDF(bean, reportName, reportParameters, reportFilename);
	}
	
	@Override
	public String createFreemarkerReport(final String templateName, final Map<String, Object> reportParameters)
	throws Exception {
		return FreemarkerReportUtil.createReport(templateName, reportParameters);
	}

	@Override
	public File createFreemarkerReportPDF(final String templateName, final Map<String, Object> reportParameters, final String reportFilename)
	throws Exception {
		return FreemarkerReportUtil.createReportPDF(templateName, reportParameters, reportFilename);
	}
	
	@Override
	public Download downloadFreemarkerReport(final String reportName,
												final Map<String, Object> reportParameters,
												final ReportFormat format,
												final String downloadFilename)
	throws Exception {
		return FreemarkerReportUtil.downloadReport(reportName, reportParameters, format, downloadFilename);
	}

	@Override
	public String runFreemarkerReport(String reportName, Map<String, Object> reportParameters) throws Exception {
		return FreemarkerReportUtil.runReport(reportName, reportParameters);
	}
	
	@Override
	public Template getBeanReport(final Bean bean, final String reportName)
	throws Exception {
		return FreemarkerReportUtil.getBeanReport(bean, reportName);
	}
	
	@Override
	public MailAttachment getMailAttachmentFromJasperReport(String reportModuleName,
																String reportDocumentName,
																String reportName,
																Map<String, Object> parameters)
	throws Exception {
		MailAttachment result = new MailAttachment();

		Persistence persistence = CORE.getPersistence();
		User user = persistence.getUser();
		Customer customer = user.getCustomer();
		Document document = customer.getModule(reportModuleName).getDocument(customer, reportDocumentName);

		byte[] reportBytes = null;
		try (ByteArrayOutputStream out = new ByteArrayOutputStream()) {
			EXT.getReporting().runJasperSQLReport(user, document, reportName, parameters, ReportFormat.pdf, out);
			reportBytes = out.toByteArray();
		}

		result.setAttachmentFileName(reportName);
		result.setAttachment(reportBytes);
		result.setAttachmentMimeType(MimeType.pdf);

		return result;
	}

	@Override
	public MailAttachment getMailAttachmentFromJasperReport(List<ReportParameters> reportParameters)
	throws Exception {
		if (reportParameters.isEmpty()) {
			throw new IllegalArgumentException("There must be at least 1 report to generate.");
		}

		MailAttachment result = new MailAttachment();

		Persistence persistence = CORE.getPersistence();
		User user = persistence.getUser();

		byte[] reportBytes = null;
		try (ByteArrayOutputStream out = new ByteArrayOutputStream()) {
			EXT.getReporting().runJasperReport(user, reportParameters, ReportFormat.pdf, out);
			reportBytes = out.toByteArray();
		}
		
		result.setAttachmentFileName(reportParameters.get(0).getReportName());
		result.setAttachment(reportBytes);
		result.setAttachmentMimeType(MimeType.pdf);

		return result;
	}
}
