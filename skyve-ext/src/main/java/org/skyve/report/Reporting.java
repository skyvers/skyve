package org.skyve.report;

import java.io.File;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.List;
import java.util.Map;

import org.skyve.domain.Bean;
import org.skyve.domain.app.admin.ReportTemplate;
import org.skyve.impl.util.ReportParameters;
import org.skyve.metadata.controller.Download;
import org.skyve.metadata.controller.DownloadAction;
import org.skyve.metadata.controller.Observer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.user.User;
import org.skyve.util.MailAttachment;

import freemarker.template.Template;
import net.sf.jasperreports.engine.JRException;
import net.sf.jasperreports.engine.JasperPrint;

public interface Reporting extends Observer {
	/**
	 * 
	 * @param user
	 * @param document
	 * @param reportName
	 * @param parameters
	 * @param bean
	 * @param format
	 * @param out
	 * @return
	 * @throws Exception
	 */
	JasperPrint runJasperBeanReport(User user, Document document, String reportName, Map<String, Object> parameters, Bean bean, ReportFormat format, OutputStream out)
	throws Exception;
	
	/**
	 * 
	 * @param user
	 * @param document
	 * @param reportName
	 * @param parameters
	 * @param format
	 * @param out
	 * @return
	 * @throws Exception
	 */
	JasperPrint runJasperSQLReport(User user, Document document, String reportName, Map<String, Object> parameters, ReportFormat format, OutputStream out)
	throws Exception;

	/**
	 * 
	 * @param user
	 * @param document
	 * @param reportName
	 * @param parameters
	 * @param bean
	 * @param format
	 * @param out
	 * @return
	 * @throws Exception
	 */
	JasperPrint runJasperReport(User user, Document document, String reportName, Map<String, Object> parameters, Bean bean, ReportFormat format, OutputStream out)
	throws Exception;

	/**
	 *
	 * @param user
	 * @param reportParameters
	 * @param format
	 * @param out
	 * @return
	 * @throws Exception
	 */
	List<JasperPrint> runJasperReport(User user, List<ReportParameters> reportParameters, ReportFormat format, OutputStream out)
	throws Exception;

	/**
	 *
	 * @param jasperPrint
	 * @param format
	 * @param out
	 * @throws JRException
	 */
	void runJasperReport(JasperPrint jasperPrint, ReportFormat format, OutputStream out)
	throws Exception;

	/**
	 *
	 * @param jasperPrintList
	 * @param format
	 * @param out
	 * @throws JRException
	 */
	void runJasperReport(List<JasperPrint> jasperPrintList, ReportFormat format, OutputStream out)
	throws Exception;

	/**
	 * Returns a mail attachment from a Jasper report as a PDF
	 * 
	 * @param reportModuleName
	 * @param reportDocumentName
	 * @param reportName
	 * @param parameters
	 */
	MailAttachment getMailAttachmentFromJasperReport(String reportModuleName,
														String reportDocumentName,
														String reportName,
														Map<String, Object> parameters)
	throws Exception;

	/**
	 * Returns a mail attachment from a Jasper report as a PDF
	 *
	 * @param reportParameters
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

	Template getFreemarkerTemplate(String templateName) throws Exception;
	
	/**
	 * Generates a PDF from a HTML file
	 * 
	 * @param in An input stream containing the source HTML
	 * @param outputFile The file to write the resulting PDF file to
	 * @throws Exception
	 */
	void generateFreemarkerPDFFromHTML(InputStream in, File outputFile) throws Exception;

	/**
	 * Generates a PDF from a HTML file
	 * 
	 * @param html The source HTML
	 * @param outputFile The file to write the resulting PDF file to
	 * @throws Exception
	 */
	void generateFreemarkerPDFFromHTML(String html, File outputFile) throws Exception;

	/**
	 * Generates a PDF from a HTML file
	 * 
	 * @param url The path to the HTML file on the filesystem
	 * @param outputFile The file to write the resulting PDF file to
	 * @throws Exception
	 */
	void generateFreemarkerPDFFromHTMLURL(String url, File outputFile) throws Exception;

	/**
	 * Generates a PDF from a HTML file
	 *
	 * @param in An input stream containing the source HTML
	 * @param outputStream The outputStream to write the resulting PDF to
	 * @throws Exception
	 */
	void generateFreemarkerPDFFromHTML(InputStream in, OutputStream out) throws Exception;

	/**
	 * Generates a PDF from a HTML file
	 *
	 * @param html The source HTML
	 * @param outputStream The outputStream to write the resulting PDF to
	 * @throws Exception
	 */
	void generateFreemarkerPDFFromHTML(String html, OutputStream out) throws Exception;

	/**
	 * Creates a new Freemarker report and returns the merged output as a String, ready to be processed further.
	 * 
	 * @param bean The Skyve document where the report is located, e.g. <code>admin.User</code>
	 * @param reportName The name of the report in the report package of the document
	 * @param reportParameters Parameters/datasets to fill the report
	 * @return A String with the merged output of the template with the report parameters
	 * @throws Exception
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
	 * @throws Exception
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
	 * @throws Exception
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
	 * @throws Exception
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
	 * @throws Exception
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
	 * @throws Exception
	 */
	String runFreemarkerReport(final String reportName, final Map<String, Object> reportParameters)
	throws Exception;

	Template getBeanReport(final Bean bean, final String reportName)
	throws Exception;
}
