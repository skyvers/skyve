package org.skyve.report;

import java.io.File;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.List;
import java.util.Map;

import org.skyve.domain.Bean;
import org.skyve.domain.app.admin.ReportTemplate;
import org.skyve.impl.util.ReportParameters;
import org.skyve.impl.util.SystemObserver;
import org.skyve.metadata.controller.Download;
import org.skyve.metadata.controller.DownloadAction;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.user.User;
import org.skyve.util.MailAttachment;

import freemarker.template.Template;
import net.sf.jasperreports.engine.JRException;
import net.sf.jasperreports.engine.JasperPrint;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Renders JasperReports and Freemarker report templates against Skyve domain data.
 *
 * <p>Obtain the active {@code Reporting} service via {@link org.skyve.EXT#getReporting()}.
 * The interface supports three output paths:
 * <ul>
 *   <li>Direct stream rendering — write a report to an {@link java.io.OutputStream} in the
 *       requested {@link ReportFormat}.</li>
 *   <li>{@link org.skyve.metadata.controller.Download} creation — wrap output for Skyve's
 *       download-action pipeline so the rendered report is returned to the browser.</li>
 *   <li>Mail attachment rendering — produce a {@link org.skyve.util.MailAttachment}
 *       suitable for inclusion in a Skyve mail message.</li>
 * </ul>
 *
 * <p>JasperReports templates are resolved from the customer's report directory using
 * the module/document/report name triple. Parameters are passed as a {@link java.util.Map}
 * and supplemented by the provided bean as the primary data source.
 *
 * <p>Threading: this interface is thread-safe; the implementation may share a single
 * JasperReports compile cache across requests.
 *
 * @see ReportFormat
 * @see org.skyve.EXT#getReporting()
 */
public interface Reporting extends SystemObserver {
	/**
	 * Runs a Jasper report using a single bean as the report data source.
	 *
	 * @param user	current user context for permission/customer resolution
	 * @param document	report-owning document metadata
	 * @param reportName	report template name
	 * @param parameters	report parameter map (may be empty)
	 * @param bean	bean bound as the primary data source
	 * @param format	output format to render
	 * @param out	output stream to write rendered content to
	 * @return the Jasper print model generated for the run
	 * @throws Exception if template resolution, data binding, or export fails
	 */
	@Nullable JasperPrint runJasperBeanReport(@Nonnull User user, @Nonnull Document document, @Nonnull String reportName, @Nonnull Map<String, Object> parameters, @Nullable Bean bean, @Nonnull ReportFormat format, @Nonnull OutputStream out)
	throws Exception;
	
	/**
	 * Runs a Jasper report backed by SQL-defined data sources.
	 *
	 * @param user	current user context for permission/customer resolution
	 * @param document	report-owning document metadata
	 * @param reportName	report template name
	 * @param parameters	report parameter map (may be empty)
	 * @param format	output format to render
	 * @param out	output stream to write rendered content to
	 * @return the Jasper print model generated for the run
	 * @throws Exception if query execution, template resolution, or export fails
	 */
	@Nullable JasperPrint runJasperSQLReport(@Nonnull User user, @Nonnull Document document, @Nonnull String reportName, @Nonnull Map<String, Object> parameters, @Nonnull ReportFormat format, @Nonnull OutputStream out)
	throws Exception;

	/**
	 * Runs a Jasper report by selecting bean or SQL execution based on template metadata.
	 *
	 * @param user current user context for permission/customer resolution
	 * @param document report-owning document metadata
	 * @param reportName report template name
	 * @param parameters report parameter map (may be empty)
	 * @param bean optional bean context for bean-backed reports
	 * @param format output format to render
	 * @param out output stream to write rendered content to
	 * @return the Jasper print model generated for the run
	 * @throws Exception if report execution or export fails
	 */
	@Nullable JasperPrint runJasperReport(@Nonnull User user, @Nonnull Document document, @Nonnull String reportName, @Nonnull Map<String, Object> parameters, @Nullable Bean bean, @Nonnull ReportFormat format, @Nonnull OutputStream out)
	throws Exception;

	/**
	 * Runs multiple Jasper report requests and writes the merged export output.
	 *
	 * @param user current user context for permission/customer resolution
	 * @param reportParameters ordered report requests to execute
	 * @param format output format to render
	 * @param out output stream to write merged rendered content to
	 * @return Jasper print list produced by each request in order
	 * @throws Exception if report execution or export fails
	 */
	@Nonnull List<JasperPrint> runJasperReport(@Nonnull User user, @Nonnull List<ReportParameters> reportParameters, @Nonnull ReportFormat format, @Nonnull OutputStream out)
	throws Exception;

	/**
	 * Exports a pre-rendered Jasper print to the specified output format.
	 *
	 * @param jasperPrint	compiled/filled Jasper print instance
	 * @param format	target export format
	 * @param out	output stream for exported bytes
	 * @throws JRException if Jasper export fails
	 */
	void runJasperReport(@Nonnull JasperPrint jasperPrint, @Nonnull ReportFormat format, @Nonnull OutputStream out)
	throws Exception;

	/**
	 * Exports multiple Jasper prints to the specified output format.
	 *
	 * @param jasperPrintList	ordered Jasper print instances
	 * @param format	target export format
	 * @param out	output stream for exported bytes
	 * @throws JRException if Jasper export fails
	 */
	void runJasperReport(@Nonnull List<JasperPrint> jasperPrintList, @Nonnull ReportFormat format, @Nonnull OutputStream out)
	throws Exception;

	/**
	 * Returns a mail attachment from a Jasper report as a PDF
	 * 
	 * @param reportModuleName module containing the report document
	 * @param reportDocumentName report-owning document name
	 * @param reportName report template name
	 * @param parameters report parameter map
	 * @return rendered PDF attachment
	 * @throws Exception if report execution or attachment creation fails
	 */
	@Nonnull MailAttachment getMailAttachmentFromJasperReport(@Nonnull String reportModuleName,
																@Nonnull String reportDocumentName,
																@Nonnull String reportName,
																@Nonnull Map<String, Object> parameters)
	throws Exception;

	/**
	 * Returns a mail attachment from a Jasper report as a PDF
	 *
	 * @param reportParameters report requests to execute and merge
	 * @return rendered PDF attachment
	 * @throws Exception if report execution or attachment creation fails
	 */
	@Nonnull MailAttachment getMailAttachmentFromJasperReport(@Nonnull List<ReportParameters> reportParameters)
	throws Exception;

	/**
	 * Adds a new in-memory String based template to the list of templates Freemarker will search
	 * for when attempting to resolve templates to merge. This can be used when the template
	 * for a report does not reside in a document report package, or in the default report
	 * database location.
	 * 
	 * @param templateName The name of the template, e.g. <code>myDynamicReport</code>
	 * @param templateMarkup The markup in the template, e.g. <code>"Hello ${user}"</code>
	 */
	void addTemplate(final @Nonnull String templateName, final @Nonnull String templateMarkup);

	/**
	 * Returns a resolved Freemarker template by name.
	 *
	 * @param templateName	template identifier
	 * @return resolved template
	 * @throws Exception if template lookup or parsing fails
	 */
	@Nonnull Template getFreemarkerTemplate(@Nonnull String templateName) throws Exception;
	
	/**
	 * Generates a PDF from a HTML file
	 * 
	 * @param in An input stream containing the source HTML
	 * @param outputFile The file to write the resulting PDF file to
	 * @throws Exception if PDF generation fails
	 */
	void generateFreemarkerPDFFromHTML(@Nonnull InputStream in, @Nonnull File outputFile) throws Exception;

	/**
	 * Generates a PDF from a HTML file
	 * 
	 * @param html The source HTML
	 * @param outputFile The file to write the resulting PDF file to
	 * @throws Exception if PDF generation fails
	 */
	void generateFreemarkerPDFFromHTML(@Nonnull String html, @Nonnull File outputFile) throws Exception;

	/**
	 * Generates a PDF from a HTML file
	 * 
	 * @param url The path to the HTML file on the filesystem
	 * @param outputFile The file to write the resulting PDF file to
	 * @throws Exception if PDF generation fails
	 */
	void generateFreemarkerPDFFromHTMLURL(@Nonnull String url, @Nonnull File outputFile) throws Exception;

	/**
	 * Generates a PDF from a HTML file
	 *
	 * @param in An input stream containing the source HTML
	 * @param outputStream The outputStream to write the resulting PDF to
	 * @throws Exception if PDF generation fails
	 */
	void generateFreemarkerPDFFromHTML(@Nonnull InputStream in, @Nonnull OutputStream out) throws Exception;

	/**
	 * Generates a PDF from a HTML file
	 *
	 * @param html The source HTML
	 * @param outputStream The outputStream to write the resulting PDF to
	 * @throws Exception if PDF generation fails
	 */
	void generateFreemarkerPDFFromHTML(@Nonnull String html, @Nonnull OutputStream out) throws Exception;

	/**
	 * Creates a new Freemarker report and returns the merged output as a String, ready to be processed further.
	 * 
	 * @param bean The Skyve document where the report is located, e.g. <code>admin.User</code>
	 * @param reportName The name of the report in the report package of the document
	 * @param reportParameters Parameters/datasets to fill the report
	 * @return A String with the merged output of the template with the report parameters
	 * @throws Exception if template lookup or merge fails
	 */
	@Nonnull String createFreemarkerBeanReport(final @Nonnull Bean bean, final @Nonnull String reportName, final @Nullable Map<String, Object> reportParameters)
	throws Exception;
	
	/**
	 * Creates a new Freemarker PDF report and writes the file to disk ready to be processed further (e.g. DownloadAction, sent via
	 * email).
	 * 
	 * @param bean The Skyve document where the report is located, e.g. <code>admin.User</code>
	 * @param reportName The name of the report in the report package of the document
	 * @param reportParameters Parameters/datasets to fill the report
	 * @param reportFilename The output filename of the report, excluding the file extension
	 * @return A temporary {@link File} pointing to the written out PDF report on disk
	 * @throws Exception if template lookup, merge, or PDF generation fails
	 */
	@Nonnull File createFreemarkerBeanReportPDF(final @Nonnull Bean bean,
												final @Nonnull String reportName,
												final @Nullable Map<String, Object> reportParameters,
												final @Nonnull String reportFilename)
	throws Exception;
	
	/**
	 * Creates a new Freemarker report and returns the merged output as a String, ready to be processed further.
	 * 
	 * @param templateName The name of the report including the path relative to the project root, e.g.
	 *        <code>moduleName/reports/reportName.html</code>
	 * @param reportParameters Parameters/datasets to fill the report
	 * @return A String with the merged output of the template with the report parameters
	 * @throws Exception if template lookup or merge fails
	 */
	@Nonnull String createFreemarkerReport(final @Nonnull String templateName, final @Nullable Map<String, Object> reportParameters)
	throws Exception;

	/**
	 * Creates a new Freemarker PDF report and writes the file to disk ready to be processed further (e.g. DownloadAction, sent via
	 * email).
	 * 
	 * @param templateName The name of the report including the path relative to the project root, e.g.
	 *        <code>moduleName/reports/reportName.html</code>
	 * @param reportParameters Parameters/datasets to fill the report
	 * @param reportFilename The output filename of the report, excluding the file extension
	 * @return A temporary {@link File} pointing to the written out PDF report on disk
	 * @throws Exception if template lookup, merge, or PDF generation fails
	 */
	@Nonnull File createFreemarkerReportPDF(final @Nonnull String templateName, final @Nullable Map<String, Object> reportParameters, final @Nonnull String reportFilename)
	throws Exception;
	
	/**
	 * Executes a {@link ReportTemplate} which has been created and saved in the database using the
	 * parameters and datasets defined in the template or supplied here, and prepares it ready to
	 * serve in a {@link DownloadAction}.
	 * 
	 * @param reportName The template name of the report in the database, e.g. myReport.flth
	 * @param reportParameters An optional map of parameters which will replace any existing parameters already
	 *        defined in the report template
	 * @param format The output format of the report, CSV or PDF
	 * @param downloadFilename The filename of the report (without the file extension)
	 * @return A download to be returned from a {@link DownloadAction}
	 * @throws Exception if template execution or output generation fails
	 */
	@Nonnull Download downloadFreemarkerReport(final @Nonnull String reportName,
												final @Nullable Map<String, Object> reportParameters,
												final @Nonnull ReportFormat format,
												final @Nonnull String downloadFilename)
	throws Exception;

	/**
	 * Executes a {@link ReportTemplate} which has been created and saved in the database using the
	 * parameters and datasets defined in the template.
	 * 
	 * @param reportName The template name of the report in the database, e.g. myReport.flth
	 * @param reportParameters An optional map of parameters which will replace any existing parameters already
	 *        defined in the report template
	 * @return A String with the merged output of the template with the report parameters
	 * @throws Exception if template execution fails
	 */
	@Nonnull String runFreemarkerReport(final @Nonnull String reportName, final @Nullable Map<String, Object> reportParameters)
	throws Exception;

	/**
	 * Resolves a Freemarker template for a document-scoped bean report.
	 *
	 * @param bean report-owning bean instance
	 * @param reportName report template name
	 * @return resolved Freemarker template
	 * @throws Exception if template lookup fails
	 */
	@Nonnull Template getBeanReport(final @Nonnull Bean bean, final @Nonnull String reportName)
	throws Exception;
}
