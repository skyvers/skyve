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
	JasperPrint runJasperBeanReport(User user, Document document, String reportName, Map<String, Object> parameters, Bean bean, ReportFormat format, OutputStream out)
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
	JasperPrint runJasperSQLReport(User user, Document document, String reportName, Map<String, Object> parameters, ReportFormat format, OutputStream out)
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
	JasperPrint runJasperReport(User user, Document document, String reportName, Map<String, Object> parameters, Bean bean, ReportFormat format, OutputStream out)
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
	List<JasperPrint> runJasperReport(User user, List<ReportParameters> reportParameters, ReportFormat format, OutputStream out)
	throws Exception;

	/**
	 * Exports a pre-rendered Jasper print to the specified output format.
	 *
	 * @param jasperPrint	compiled/filled Jasper print instance
	 * @param format	target export format
	 * @param out	output stream for exported bytes
	 * @throws JRException if Jasper export fails
	 */
	void runJasperReport(JasperPrint jasperPrint, ReportFormat format, OutputStream out)
	throws Exception;

	/**
	 * Exports multiple Jasper prints to the specified output format.
	 *
	 * @param jasperPrintList	ordered Jasper print instances
	 * @param format	target export format
	 * @param out	output stream for exported bytes
	 * @throws JRException if Jasper export fails
	 */
	void runJasperReport(List<JasperPrint> jasperPrintList, ReportFormat format, OutputStream out)
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
	MailAttachment getMailAttachmentFromJasperReport(String reportModuleName,
														String reportDocumentName,
														String reportName,
														Map<String, Object> parameters)
	throws Exception;

	/**
	 * Returns a mail attachment from a Jasper report as a PDF
	 *
	 * @param reportParameters report requests to execute and merge
	 * @return rendered PDF attachment
	 * @throws Exception if report execution or attachment creation fails
	 */
	MailAttachment getMailAttachmentFromJasperReport(List<ReportParameters> reportParameters)
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
	void addTemplate(final String templateName, final String templateMarkup);

	/**
	 * Returns a resolved Freemarker template by name.
	 *
	 * @param templateName	template identifier
	 * @return resolved template
	 * @throws Exception if template lookup or parsing fails
	 */
	Template getFreemarkerTemplate(String templateName) throws Exception;
	
	/**
	 * Generates a PDF from a HTML file
	 * 
	 * @param in An input stream containing the source HTML
	 * @param outputFile The file to write the resulting PDF file to
	 * @throws Exception if PDF generation fails
	 */
	void generateFreemarkerPDFFromHTML(InputStream in, File outputFile) throws Exception;

	/**
	 * Generates a PDF from a HTML file
	 * 
	 * @param html The source HTML
	 * @param outputFile The file to write the resulting PDF file to
	 * @throws Exception if PDF generation fails
	 */
	void generateFreemarkerPDFFromHTML(String html, File outputFile) throws Exception;

	/**
	 * Generates a PDF from a HTML file
	 * 
	 * @param url The path to the HTML file on the filesystem
	 * @param outputFile The file to write the resulting PDF file to
	 * @throws Exception if PDF generation fails
	 */
	void generateFreemarkerPDFFromHTMLURL(String url, File outputFile) throws Exception;

	/**
	 * Generates a PDF from a HTML file
	 *
	 * @param in An input stream containing the source HTML
	 * @param outputStream The outputStream to write the resulting PDF to
	 * @throws Exception if PDF generation fails
	 */
	void generateFreemarkerPDFFromHTML(InputStream in, OutputStream out) throws Exception;

	/**
	 * Generates a PDF from a HTML file
	 *
	 * @param html The source HTML
	 * @param outputStream The outputStream to write the resulting PDF to
	 * @throws Exception if PDF generation fails
	 */
	void generateFreemarkerPDFFromHTML(String html, OutputStream out) throws Exception;

	/**
	 * Creates a new Freemarker report and returns the merged output as a String, ready to be processed further.
	 * 
	 * @param bean The Skyve document where the report is located, e.g. <code>admin.User</code>
	 * @param reportName The name of the report in the report package of the document
	 * @param reportParameters Parameters/datasets to fill the report
	 * @return A String with the merged output of the template with the report parameters
	 * @throws Exception if template lookup or merge fails
	 */
	String createFreemarkerBeanReport(final Bean bean, final String reportName, final Map<String, Object> reportParameters)
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
	File createFreemarkerBeanReportPDF(final Bean bean,
										final String reportName,
										final Map<String, Object> reportParameters,
										final String reportFilename)
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
	String createFreemarkerReport(final String templateName, final Map<String, Object> reportParameters)
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
	File createFreemarkerReportPDF(final String templateName, final Map<String, Object> reportParameters, final String reportFilename)
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
	Download downloadFreemarkerReport(final String reportName,
										final Map<String, Object> reportParameters,
										final ReportFormat format,
										final String downloadFilename)
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
	String runFreemarkerReport(final String reportName, final Map<String, Object> reportParameters)
	throws Exception;

	/**
	 * Resolves a Freemarker template for a document-scoped bean report.
	 *
	 * @param bean report-owning bean instance
	 * @param reportName report template name
	 * @return resolved Freemarker template
	 * @throws Exception if template lookup fails
	 */
	Template getBeanReport(final Bean bean, final String reportName)
	throws Exception;
}
