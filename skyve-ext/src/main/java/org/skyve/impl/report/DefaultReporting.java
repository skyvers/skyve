package org.skyve.impl.report;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.Map;

import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.content.MimeType;
import org.skyve.domain.Bean;
import org.skyve.impl.report.freemarker.FreemarkerReportUtil;
import org.skyve.impl.report.jasperreports.JasperReportUtil;
import org.skyve.impl.util.ReportParameters;
import org.skyve.metadata.controller.Download;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.user.User;
import org.skyve.persistence.Persistence;
import org.skyve.report.ReportFormat;
import org.skyve.report.Reporting;
import org.skyve.util.MailAttachment;

import freemarker.template.Template;
import net.sf.jasperreports.engine.JasperPrint;

/**
 * Default implementation of Skyve reporting facade backed by JasperReports and FreeMarker utilities.
 */
public class DefaultReporting implements Reporting {
	private static final DefaultReporting INSTANCE = new DefaultReporting();
	
	public static DefaultReporting get() {
		return INSTANCE;
	}
	
	/**
	 * Disallows external instantiation.
	 */
	private DefaultReporting() {
		// nothing to do here
	}

	/**
	 * Initializes report engines required by this reporting facade.
	 */
	@Override
	public void startup() {
		FreemarkerReportUtil.init();
	}

	/**
	 * Shuts down reporting resources for this implementation.
	 */
	@Override
	public void shutdown() {
		// nothing to do
	}
	
	/**
	 * Runs a Jasper bean report for a single bean instance.
	 */
	@Override
	public JasperPrint runJasperBeanReport(User user, Document document, String reportName, Map<String, Object> parameters, Bean bean, ReportFormat format, OutputStream out)
	throws Exception {
		return JasperReportUtil.runBeanReport(user, document, reportName, parameters, bean, format, out);
	}

	/**
	 * Runs a Jasper SQL report and writes the rendered output.
	 */
	@Override
	public JasperPrint runJasperSQLReport(User user, Document document, String reportName, Map<String, Object> parameters, ReportFormat format, OutputStream out)
	throws Exception {
		return JasperReportUtil.runSQLReport(user, document, reportName, parameters, format, out);
	}

	/**
	 * Runs a Jasper report using the supplied document context and bean.
	 */
	@Override
	public JasperPrint runJasperReport(User user, Document document, String reportName, Map<String, Object> parameters, Bean bean, ReportFormat format, OutputStream out)
	throws Exception {
		return JasperReportUtil.runReport(user, document, reportName, parameters, bean, format, out);
	}

	/**
	 * Runs and merges multiple Jasper report parameter sets.
	 */
	@Override
	public List<JasperPrint> runJasperReport(User user, List<ReportParameters> reportParameters, ReportFormat format, OutputStream out)
	throws Exception {
		return JasperReportUtil.runReport(user, reportParameters, format, out);
	}

	/**
	 * Renders a previously generated Jasper print to the requested format.
	 */
	@Override
	public void runJasperReport(JasperPrint jasperPrint, ReportFormat format, OutputStream out)
	throws Exception {
		JasperReportUtil.runReport(jasperPrint, format, out);
	}

	/**
	 * Renders a list of Jasper prints to the requested format.
	 */
	@Override
	public void runJasperReport(List<JasperPrint> jasperPrintList, ReportFormat format, OutputStream out)
	throws Exception {
		JasperReportUtil.runReport(jasperPrintList, format, out);
	}

	/**
	 * Registers an in-memory FreeMarker template.
	 */
	@Override
	public void addTemplate(String templateName, String templateMarkup) {
		FreemarkerReportUtil.addTemplate(templateName, templateMarkup);
	}

	/**
	 * Resolves a FreeMarker template by name.
	 */
	@Override
	public Template getFreemarkerTemplate(String templateName) throws Exception {
		return FreemarkerReportUtil.getTemplate(templateName);
	}
	
	/**
	 * Generates a PDF from HTML content read from an input stream.
	 */
	@Override
	public void generateFreemarkerPDFFromHTML(InputStream in, File outputFile) throws Exception {
		FreemarkerReportUtil.generatePDFFromHTML(in, outputFile);
	}

	/**
	 * Generates a PDF from an HTML string and writes it to a file.
	 */
	@Override
	public void generateFreemarkerPDFFromHTML(String html, File outputFile) throws Exception {
		try (ByteArrayInputStream in = new ByteArrayInputStream(html.getBytes(StandardCharsets.UTF_8))) {
			FreemarkerReportUtil.generatePDFFromHTML(in, outputFile);
		}
	}

	/**
	 * Generates a PDF from HTML located at a URL.
	 */
	@Override
	public void generateFreemarkerPDFFromHTMLURL(String url, File outputFile) throws Exception {
		FreemarkerReportUtil.generatePDFFromHTML(url, outputFile);
	}

	/**
	 * Generates a PDF from stream-based HTML and writes to an output stream.
	 */
	@Override
	public final void generateFreemarkerPDFFromHTML(InputStream in, OutputStream out) throws Exception {
		FreemarkerReportUtil.generatePDFFromHTML(in, out);
	}

	/**
	 * Generates a PDF from an HTML string and writes to an output stream.
	 */
	@Override
	public final void generateFreemarkerPDFFromHTML(String html, OutputStream out) throws Exception {
		try (ByteArrayInputStream in = new ByteArrayInputStream(html.getBytes(StandardCharsets.UTF_8))) {
			FreemarkerReportUtil.generatePDFFromHTML(in, out);
		}
	}

	/**
	 * Creates a FreeMarker bean report and returns rendered markup.
	 */
	@Override
	public String createFreemarkerBeanReport(final Bean bean, final String reportName, final Map<String, Object> reportParameters)
	throws Exception {
		return FreemarkerReportUtil.createBeanReport(bean, reportName, reportParameters);
	}
	
	/**
	 * Creates and saves a FreeMarker bean report as a PDF file.
	 */
	@Override
	public File createFreemarkerBeanReportPDF(final Bean bean,
												final String reportName,
												final Map<String, Object> reportParameters,
												final String reportFilename)
	throws Exception {
		return FreemarkerReportUtil.createBeanReportPDF(bean, reportName, reportParameters, reportFilename);
	}
	
	/**
	 * Creates a FreeMarker report from a named template.
	 */
	@Override
	public String createFreemarkerReport(final String templateName, final Map<String, Object> reportParameters)
	throws Exception {
		return FreemarkerReportUtil.createReport(templateName, reportParameters);
	}

	/**
	 * Creates and saves a FreeMarker template report as PDF.
	 */
	@Override
	public File createFreemarkerReportPDF(final String templateName, final Map<String, Object> reportParameters, final String reportFilename)
	throws Exception {
		return FreemarkerReportUtil.createReportPDF(templateName, reportParameters, reportFilename);
	}
	
	/**
	 * Produces a downloadable report from a FreeMarker template.
	 */
	@Override
	public Download downloadFreemarkerReport(final String reportName,
												final Map<String, Object> reportParameters,
												final ReportFormat format,
												final String downloadFilename)
	throws Exception {
		return FreemarkerReportUtil.downloadReport(reportName, reportParameters, format, downloadFilename);
	}

	/**
	 * Runs a FreeMarker report and returns the rendered output.
	 */
	@Override
	public String runFreemarkerReport(String reportName, Map<String, Object> reportParameters) throws Exception {
		return FreemarkerReportUtil.runReport(reportName, reportParameters);
	}
	
	/**
	 * Resolves the bean-specific FreeMarker template for the given report name.
	 */
	@Override
	public Template getBeanReport(final Bean bean, final String reportName)
	throws Exception {
		return FreemarkerReportUtil.getBeanReport(bean, reportName);
	}
	
	/**
	 * Generates a PDF attachment from a Jasper SQL report.
	 */
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

	/**
	 * Generates a PDF attachment by running a sequence of Jasper reports.
	 */
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
