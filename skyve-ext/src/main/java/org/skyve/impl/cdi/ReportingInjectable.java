package org.skyve.impl.cdi;

import java.io.File;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.Serializable;
import java.util.List;
import java.util.Map;

import org.skyve.EXT;
import org.skyve.domain.Bean;
import org.skyve.impl.util.ReportParameters;
import org.skyve.metadata.controller.Download;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.user.User;
import org.skyve.report.ReportFormat;
import org.skyve.report.Reporting;
import org.skyve.util.MailAttachment;

import freemarker.template.Template;
import jakarta.enterprise.inject.Alternative;
import net.sf.jasperreports.engine.JasperPrint;

/**
 * Stateless CDI proxy for {@link Reporting}.
 *
 * <p>Delegates report execution and template rendering to {@link EXT#getReporting()}
 * so passivated beans do not need to serialize heavy reporting infrastructure.
 */
@Alternative
public class ReportingInjectable implements Reporting, Serializable {
	private static final long serialVersionUID = -3488879632970475619L;

	/**
	 * Starts reporting infrastructure.
	 */
	@Override
	public void startup() {
		EXT.getReporting().startup();
	}

	/**
	 * Stops reporting infrastructure.
	 */
	@Override
	public void shutdown() {
		EXT.getReporting().shutdown();
	}

	/**
	 * Executes a Jasper report with bean-backed data and optionally writes formatted output.
	 *
	 * @param user the user context used to resolve permissions and locale.
	 * @param document the target document metadata.
	 * @param reportName the report name.
	 * @param parameters the report parameter map.
	 * @param bean the bean used as the report data source.
	 * @param format the optional export format; when null, only the print model is returned.
	 * @param out the optional output stream for formatted report content.
	 * @return the generated Jasper print model.
	 * @throws Exception if report execution fails.
	 */
	@Override
	public JasperPrint runJasperBeanReport(User user,
											Document document,
											String reportName,
											Map<String, Object> parameters,
											Bean bean,
											ReportFormat format,
											OutputStream out)
	throws Exception {
		return EXT.getReporting().runJasperBeanReport(user, document, reportName, parameters, bean, format, out);
	}

	/**
	 * Executes a Jasper report backed by SQL data and optionally writes formatted output.
	 *
	 * @param user the user context used to resolve permissions and locale.
	 * @param document the target document metadata.
	 * @param reportName the report name.
	 * @param parameters the report parameter map.
	 * @param format the optional export format; when null, only the print model is returned.
	 * @param out the optional output stream for formatted report content.
	 * @return the generated Jasper print model.
	 * @throws Exception if report execution fails.
	 */
	@Override
	public JasperPrint runJasperSQLReport(User user,
											Document document,
											String reportName,
											Map<String, Object> parameters,
											ReportFormat format,
											OutputStream out)
	throws Exception {
		return EXT.getReporting().runJasperSQLReport(user, document, reportName, parameters, format, out);
	}

	/**
	 * Executes a Jasper report and returns the generated print model.
	 *
	 * @param user the user context used to resolve permissions and locale.
	 * @param document the target document metadata.
	 * @param reportName the report name.
	 * @param parameters the report parameter map.
	 * @param bean the bean used as the report data source.
	 * @param format the optional export format; when null, only the print model is returned.
	 * @param out the optional output stream for formatted report content.
	 * @return the generated Jasper print model.
	 * @throws Exception if report execution fails.
	 */
	@Override
	public JasperPrint runJasperReport(User user,
										Document document,
										String reportName,
										Map<String, Object> parameters,
										Bean bean,
										ReportFormat format,
										OutputStream out)
	throws Exception {
		return EXT.getReporting().runJasperBeanReport(user, document, reportName, parameters, bean, format, out);
	}

	/**
	 * Executes a batch of Jasper report requests.
	 *
	 * @param user the user context used to resolve permissions and locale.
	 * @param reportParameters the ordered report parameter definitions.
	 * @param format the optional export format; when null, print models are returned only.
	 * @param out the optional output stream for formatted report content.
	 * @return the generated Jasper print models.
	 * @throws Exception if report execution fails.
	 */
	@Override
	public List<JasperPrint> runJasperReport(User user,
												List<ReportParameters> reportParameters,
												ReportFormat format,
												OutputStream out)
	throws Exception {
		return EXT.getReporting().runJasperReport(user, reportParameters, format, out);
	}

	/**
	 * Renders one Jasper print into the requested output format.
	 *
	 * @param jasperPrint the print model to render.
	 * @param format the output format.
	 * @param out the output stream receiving rendered content.
	 * @throws Exception if rendering fails.
	 */
	@Override
	public void runJasperReport(JasperPrint jasperPrint, ReportFormat format, OutputStream out) throws Exception {
		EXT.getReporting().runJasperReport(jasperPrint, format, out);
	}

	/**
	 * Renders multiple Jasper prints into the requested output format.
	 *
	 * @param jasperPrintList the print models to render.
	 * @param format the output format.
	 * @param out the output stream receiving rendered content.
	 * @throws Exception if rendering fails.
	 */
	@Override
	public void runJasperReport(List<JasperPrint> jasperPrintList, ReportFormat format, OutputStream out)
	throws Exception {
		EXT.getReporting().runJasperReport(jasperPrintList, format, out);
	}

	/**
	 * Creates a mail attachment from a Jasper report definition.
	 *
	 * @param reportModuleName the report module name.
	 * @param reportDocumentName the report document name.
	 * @param reportName the report name.
	 * @param parameters the report parameter map.
	 * @return the generated mail attachment.
	 * @throws Exception if report generation fails.
	 */
	@Override
	public MailAttachment getMailAttachmentFromJasperReport(String reportModuleName,
																String reportDocumentName,
																String reportName,
																Map<String, Object> parameters)
	throws Exception {
		return EXT.getReporting().getMailAttachmentFromJasperReport(reportModuleName, reportDocumentName, reportName, parameters);
	}

	/**
	 * Creates a combined mail attachment from multiple Jasper report definitions.
	 *
	 * @param reportParameters the report parameter definitions.
	 * @return the generated mail attachment.
	 * @throws Exception if report generation fails.
	 */
	@Override
	public MailAttachment getMailAttachmentFromJasperReport(List<ReportParameters> reportParameters)
	throws Exception {
		return EXT.getReporting().getMailAttachmentFromJasperReport(reportParameters);
	}

	/**
	 * Registers or replaces a Freemarker template in the reporting service.
	 *
	 * @param templateName the template name.
	 * @param templateMarkup the template markup content.
	 */
	@Override
	public void addTemplate(String templateName, String templateMarkup) {
		EXT.getReporting().addTemplate(templateName, templateMarkup);
	}

	/**
	 * Returns a Freemarker template by name.
	 *
	 * @param templateName the template name.
	 * @return the resolved Freemarker template.
	 * @throws Exception if template resolution fails.
	 */
	@Override
	public Template getFreemarkerTemplate(String templateName) throws Exception {
		return EXT.getReporting().getFreemarkerTemplate(templateName);
	}

	/**
	 * Generates a PDF from HTML content read from an input stream.
	 *
	 * @param in the HTML input stream.
	 * @param outputFile the target output PDF file.
	 * @throws Exception if PDF generation fails.
	 */
	@Override
	public void generateFreemarkerPDFFromHTML(InputStream in, File outputFile) throws Exception {
		EXT.getReporting().generateFreemarkerPDFFromHTML(in, outputFile);
	}

	/**
	 * Generates a PDF from raw HTML markup.
	 *
	 * @param html the HTML markup.
	 * @param outputFile the target output PDF file.
	 * @throws Exception if PDF generation fails.
	 */
	@Override
	public void generateFreemarkerPDFFromHTML(String html, File outputFile) throws Exception {
		EXT.getReporting().generateFreemarkerPDFFromHTML(html, outputFile);
	}

	/**
	 * Generates a PDF from HTML loaded from a URL.
	 *
	 * @param url the source URL containing HTML content.
	 * @param outputFile the target output PDF file.
	 * @throws Exception if PDF generation fails.
	 */
	@Override
	public void generateFreemarkerPDFFromHTMLURL(String url, File outputFile) throws Exception {
		EXT.getReporting().generateFreemarkerPDFFromHTML(url, outputFile);
	}

	/**
	 * Generates a PDF from HTML content read from an input stream.
	 *
	 * @param in the HTML input stream.
	 * @param out the output stream receiving generated PDF content.
	 * @throws Exception if PDF generation fails.
	 */
	@Override
	public void generateFreemarkerPDFFromHTML(InputStream in, OutputStream out) throws Exception {
		EXT.getReporting().generateFreemarkerPDFFromHTML(in, out);
	}

	/**
	 * Generates a PDF from raw HTML markup.
	 *
	 * @param html the HTML markup.
	 * @param out the output stream receiving generated PDF content.
	 * @throws Exception if PDF generation fails.
	 */
	@Override
	public void generateFreemarkerPDFFromHTML(String html, OutputStream out) throws Exception {
		EXT.getReporting().generateFreemarkerPDFFromHTML(html, out);
	}

	/**
	 * Creates a Freemarker bean report and returns the rendered result.
	 *
	 * @param bean the bean used as report input.
	 * @param reportName the report name.
	 * @param reportParameters the report parameter map.
	 * @return the rendered report content.
	 * @throws Exception if report creation fails.
	 */
	@Override
	public String createFreemarkerBeanReport(Bean bean, String reportName, Map<String, Object> reportParameters)
	throws Exception {
		return EXT.getReporting().createFreemarkerBeanReport(bean, reportName, reportParameters);
	}

	/**
	 * Creates a Freemarker bean report and persists it as a PDF file.
	 *
	 * @param bean the bean used as report input.
	 * @param reportName the report name.
	 * @param reportParameters the report parameter map.
	 * @param reportFilename the target file name.
	 * @return the generated PDF file.
	 * @throws Exception if report creation fails.
	 */
	@Override
	public File createFreemarkerBeanReportPDF(Bean bean,
												String reportName,
												Map<String, Object> reportParameters,
												String reportFilename)
	throws Exception {
		return EXT.getReporting().createFreemarkerBeanReportPDF(bean, reportName, reportParameters, reportFilename);
	}

	/**
	 * Creates a Freemarker report from a template and returns the rendered result.
	 *
	 * @param templateName the template name.
	 * @param reportParameters the report parameter map.
	 * @return the rendered report content.
	 * @throws Exception if report creation fails.
	 */
	@Override
	public String createFreemarkerReport(String templateName, Map<String, Object> reportParameters) throws Exception {
		return EXT.getReporting().createFreemarkerReport(templateName, reportParameters);
	}

	/**
	 * Creates a Freemarker report and persists it as a PDF file.
	 *
	 * @param templateName the template name.
	 * @param reportParameters the report parameter map.
	 * @param reportFilename the target file name.
	 * @return the generated PDF file.
	 * @throws Exception if report creation fails.
	 */
	@Override
	public File createFreemarkerReportPDF(String templateName,
											Map<String, Object> reportParameters,
											String reportFilename)
	throws Exception {
		return EXT.getReporting().createFreemarkerReportPDF(templateName, reportParameters, reportFilename);
	}

	/**
	 * Creates and downloads a Freemarker report in the requested format.
	 *
	 * @param reportName the report name.
	 * @param reportParameters the report parameter map.
	 * @param format the download format.
	 * @param downloadFilename the suggested download filename.
	 * @return a download descriptor for the generated report.
	 * @throws Exception if report generation fails.
	 */
	@Override
	public Download downloadFreemarkerReport(String reportName,
												Map<String, Object> reportParameters,
												ReportFormat format,
												String downloadFilename)
	throws Exception {
		return EXT.getReporting().downloadFreemarkerReport(reportName, reportParameters, format, downloadFilename);
	}

	/**
	 * Executes a Freemarker report and returns the rendered result.
	 *
	 * @param reportName the report name.
	 * @param reportParameters the report parameter map.
	 * @return the rendered report content.
	 * @throws Exception if report execution fails.
	 */
	@Override
	public String runFreemarkerReport(String reportName, Map<String, Object> reportParameters) throws Exception {
		return EXT.getReporting().runFreemarkerReport(reportName, reportParameters);
	}

	/**
	 * Resolves a Freemarker bean report template.
	 *
	 * @param bean the bean used as report context.
	 * @param reportName the report name.
	 * @return the resolved report template.
	 * @throws Exception if template resolution fails.
	 */
	@Override
	public Template getBeanReport(Bean bean, String reportName) throws Exception {
		return EXT.getReporting().getBeanReport(bean, reportName);
	}
}
