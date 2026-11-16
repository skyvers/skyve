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

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Default implementation of Skyve reporting facade backed by JasperReports and FreeMarker utilities.
 */
public class DefaultReporting implements Reporting {
	private static final DefaultReporting INSTANCE = new DefaultReporting();
	
	public static @Nonnull DefaultReporting get() {
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
	public @Nullable JasperPrint runJasperBeanReport(@Nonnull User user, @Nonnull Document document, @Nonnull String reportName, @Nonnull Map<String, Object> parameters, @Nullable Bean bean, @Nonnull ReportFormat format, @Nonnull OutputStream out)
	throws Exception {
		return JasperReportUtil.runBeanReport(user, document, reportName, parameters, bean, format, out);
	}

	/**
	 * Runs a Jasper SQL report and writes the rendered output.
	 */
	@Override
	public @Nullable JasperPrint runJasperSQLReport(@Nonnull User user, @Nonnull Document document, @Nonnull String reportName, @Nonnull Map<String, Object> parameters, @Nonnull ReportFormat format, @Nonnull OutputStream out)
	throws Exception {
		return JasperReportUtil.runSQLReport(user, document, reportName, parameters, format, out);
	}

	/**
	 * Runs a Jasper report using the supplied document context and bean.
	 */
	@Override
	public @Nullable JasperPrint runJasperReport(@Nonnull User user, @Nonnull Document document, @Nonnull String reportName, @Nonnull Map<String, Object> parameters, @Nullable Bean bean, @Nonnull ReportFormat format, @Nonnull OutputStream out)
	throws Exception {
		return JasperReportUtil.runReport(user, document, reportName, parameters, bean, format, out);
	}

	/**
	 * Runs and merges multiple Jasper report parameter sets.
	 */
	@Override
	public @Nonnull List<JasperPrint> runJasperReport(@Nonnull User user, @Nonnull List<ReportParameters> reportParameters, @Nonnull ReportFormat format, @Nonnull OutputStream out)
	throws Exception {
		return JasperReportUtil.runReport(user, reportParameters, format, out);
	}

	/**
	 * Renders a previously generated Jasper print to the requested format.
	 */
	@Override
	public void runJasperReport(@Nonnull JasperPrint jasperPrint, @Nonnull ReportFormat format, @Nonnull OutputStream out)
	throws Exception {
		JasperReportUtil.runReport(jasperPrint, format, out);
	}

	/**
	 * Renders a list of Jasper prints to the requested format.
	 */
	@Override
	public void runJasperReport(@Nonnull List<JasperPrint> jasperPrintList, @Nonnull ReportFormat format, @Nonnull OutputStream out)
	throws Exception {
		JasperReportUtil.runReport(jasperPrintList, format, out);
	}

	/**
	 * Registers an in-memory FreeMarker template.
	 */
	@Override
	public void addTemplate(@Nonnull String templateName, @Nonnull String templateMarkup) {
		FreemarkerReportUtil.addTemplate(templateName, templateMarkup);
	}

	/**
	 * Resolves a FreeMarker template by name.
	 */
	@Override
	public @Nonnull Template getFreemarkerTemplate(@Nonnull String templateName) throws Exception {
		return FreemarkerReportUtil.getTemplate(templateName);
	}
	
	/**
	 * Generates a PDF from HTML content read from an input stream.
	 */
	@Override
	public void generateFreemarkerPDFFromHTML(@Nonnull InputStream in, @Nonnull File outputFile) throws Exception {
		FreemarkerReportUtil.generatePDFFromHTML(in, outputFile);
	}

	/**
	 * Generates a PDF from an HTML string and writes it to a file.
	 */
	@Override
	public void generateFreemarkerPDFFromHTML(@Nonnull String html, @Nonnull File outputFile) throws Exception {
		try (ByteArrayInputStream in = new ByteArrayInputStream(html.getBytes(StandardCharsets.UTF_8))) {
			FreemarkerReportUtil.generatePDFFromHTML(in, outputFile);
		}
	}

	/**
	 * Generates a PDF from HTML located at a URL.
	 */
	@Override
	public void generateFreemarkerPDFFromHTMLURL(@Nonnull String url, @Nonnull File outputFile) throws Exception {
		FreemarkerReportUtil.generatePDFFromHTML(url, outputFile);
	}

	/**
	 * Generates a PDF from stream-based HTML and writes to an output stream.
	 */
	@Override
	public final void generateFreemarkerPDFFromHTML(@Nonnull InputStream in, @Nonnull OutputStream out) throws Exception {
		FreemarkerReportUtil.generatePDFFromHTML(in, out);
	}

	/**
	 * Generates a PDF from an HTML string and writes to an output stream.
	 */
	@Override
	public final void generateFreemarkerPDFFromHTML(@Nonnull String html, @Nonnull OutputStream out) throws Exception {
		try (ByteArrayInputStream in = new ByteArrayInputStream(html.getBytes(StandardCharsets.UTF_8))) {
			FreemarkerReportUtil.generatePDFFromHTML(in, out);
		}
	}

	/**
	 * Creates a FreeMarker bean report and returns rendered markup.
	 */
	@Override
	public @Nonnull String createFreemarkerBeanReport(final @Nonnull Bean bean, final @Nonnull String reportName, final @Nullable Map<String, Object> reportParameters)
	throws Exception {
		return FreemarkerReportUtil.createBeanReport(bean, reportName, reportParameters);
	}
	
	/**
	 * Creates and saves a FreeMarker bean report as a PDF file.
	 */
	@Override
	public @Nonnull File createFreemarkerBeanReportPDF(final @Nonnull Bean bean,
														final @Nonnull String reportName,
														final @Nullable Map<String, Object> reportParameters,
														final @Nonnull String reportFilename)
	throws Exception {
		return FreemarkerReportUtil.createBeanReportPDF(bean, reportName, reportParameters, reportFilename);
	}
	
	/**
	 * Creates a FreeMarker report from a named template.
	 */
	@Override
	public @Nonnull String createFreemarkerReport(final @Nonnull String templateName, final @Nullable Map<String, Object> reportParameters)
	throws Exception {
		return FreemarkerReportUtil.createReport(templateName, reportParameters);
	}

	/**
	 * Creates and saves a FreeMarker template report as PDF.
	 */
	@Override
	public @Nonnull File createFreemarkerReportPDF(final @Nonnull String templateName, final @Nullable Map<String, Object> reportParameters, final @Nonnull String reportFilename)
	throws Exception {
		return FreemarkerReportUtil.createReportPDF(templateName, reportParameters, reportFilename);
	}
	
	/**
	 * Produces a downloadable report from a FreeMarker template.
	 */
	@Override
	public @Nonnull Download downloadFreemarkerReport(final @Nonnull String reportName,
														final @Nullable Map<String, Object> reportParameters,
														final @Nonnull ReportFormat format,
														final @Nonnull String downloadFilename)
	throws Exception {
		return FreemarkerReportUtil.downloadReport(reportName, reportParameters, format, downloadFilename);
	}

	/**
	 * Runs a FreeMarker report and returns the rendered output.
	 */
	@Override
	public @Nonnull String runFreemarkerReport(@Nonnull String reportName, @Nullable Map<String, Object> reportParameters) throws Exception {
		return FreemarkerReportUtil.runReport(reportName, reportParameters);
	}
	
	/**
	 * Resolves the bean-specific FreeMarker template for the given report name.
	 */
	@Override
	public @Nonnull Template getBeanReport(final @Nonnull Bean bean, final @Nonnull String reportName)
	throws Exception {
		return FreemarkerReportUtil.getBeanReport(bean, reportName);
	}
	
	/**
	 * Generates a PDF attachment from a Jasper SQL report.
	 */
	@Override
	public @Nonnull MailAttachment getMailAttachmentFromJasperReport(@Nonnull String reportModuleName,
																		@Nonnull String reportDocumentName,
																		@Nonnull String reportName,
																		@Nonnull Map<String, Object> parameters)
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
	public @Nonnull MailAttachment getMailAttachmentFromJasperReport(@Nonnull List<ReportParameters> reportParameters)
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
