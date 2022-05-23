package org.skyve.impl.cdi;

import java.io.File;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.Serializable;
import java.util.List;
import java.util.Map;

import javax.enterprise.inject.Alternative;

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
import net.sf.jasperreports.engine.JasperPrint;

/**
 * A proxy that can be Serialized with no state and continue to work after deserialization.
 * 
 * @author mike
 */
@Alternative
public class ReportingInjectable implements Reporting, Serializable {
	private static final long serialVersionUID = -3488879632970475619L;

	@Override
	public void startup() {
		EXT.getReporting().startup();
	}

	@Override
	public void shutdown() {
		EXT.getReporting().shutdown();
	}

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

	@Override
	public List<JasperPrint> runJasperReport(User user,
												List<ReportParameters> reportParameters,
												ReportFormat format,
												OutputStream out)
	throws Exception {
		return EXT.getReporting().runJasperReport(user, reportParameters, format, out);
	}

	@Override
	public void runJasperReport(JasperPrint jasperPrint, ReportFormat format, OutputStream out) throws Exception {
		EXT.getReporting().runJasperReport(jasperPrint, format, out);
	}

	@Override
	public void runJasperReport(List<JasperPrint> jasperPrintList, ReportFormat format, OutputStream out)
	throws Exception {
		EXT.getReporting().runJasperReport(jasperPrintList, format, out);
	}

	@Override
	public MailAttachment getMailAttachmentFromJasperReport(String reportModuleName,
																String reportDocumentName,
																String reportName,
																Map<String, Object> parameters)
	throws Exception {
		return EXT.getReporting().getMailAttachmentFromJasperReport(reportModuleName, reportDocumentName, reportName, parameters);
	}

	@Override
	public MailAttachment getMailAttachmentFromJasperReport(List<ReportParameters> reportParameters)
	throws Exception {
		return EXT.getReporting().getMailAttachmentFromJasperReport(reportParameters);
	}

	@Override
	public void addTemplate(String templateName, String templateMarkup) {
		EXT.getReporting().addTemplate(templateName, templateMarkup);
	}

	@Override
	public Template getFreemarkerTemplate(String templateName) throws Exception {
		return EXT.getReporting().getFreemarkerTemplate(templateName);
	}

	@Override
	public void generateFreemarkerPDFFromHTML(InputStream in, File outputFile) throws Exception {
		EXT.getReporting().generateFreemarkerPDFFromHTML(in, outputFile);
	}

	@Override
	public void generateFreemarkerPDFFromHTML(String html, File outputFile) throws Exception {
		EXT.getReporting().generateFreemarkerPDFFromHTML(html, outputFile);
	}

	@Override
	public void generateFreemarkerPDFFromHTMLURL(String url, File outputFile) throws Exception {
		EXT.getReporting().generateFreemarkerPDFFromHTML(url, outputFile);
	}

	@Override
	public void generateFreemarkerPDFFromHTML(InputStream in, OutputStream out) throws Exception {
		EXT.getReporting().generateFreemarkerPDFFromHTML(in, out);
	}

	@Override
	public void generateFreemarkerPDFFromHTML(String html, OutputStream out) throws Exception {
		EXT.getReporting().generateFreemarkerPDFFromHTML(html, out);
	}

	@Override
	public String createFreemarkerBeanReport(Bean bean, String reportName, Map<String, Object> reportParameters)
	throws Exception {
		return EXT.getReporting().createFreemarkerBeanReport(bean, reportName, reportParameters);
	}

	@Override
	public File createFreemarkerBeanReportPDF(Bean bean,
												String reportName,
												Map<String, Object> reportParameters,
												String reportFilename)
	throws Exception {
		return EXT.getReporting().createFreemarkerBeanReportPDF(bean, reportName, reportParameters, reportFilename);
	}

	@Override
	public String createFreemarkerReport(String templateName, Map<String, Object> reportParameters) throws Exception {
		return EXT.getReporting().createFreemarkerReport(templateName, reportParameters);
	}

	@Override
	public File createFreemarkerReportPDF(String templateName,
											Map<String, Object> reportParameters,
											String reportFilename)
	throws Exception {
		return EXT.getReporting().createFreemarkerReportPDF(templateName, reportParameters, reportFilename);
	}

	@Override
	public Download downloadFreemarkerReport(String reportName,
												Map<String, Object> reportParameters,
												ReportFormat format,
												String downloadFilename)
	throws Exception {
		return EXT.getReporting().downloadFreemarkerReport(reportName, reportParameters, format, downloadFilename);
	}

	@Override
	public String runFreemarkerReport(String reportName, Map<String, Object> reportParameters) throws Exception {
		return EXT.getReporting().runFreemarkerReport(reportName, reportParameters);
	}

	@Override
	public Template getBeanReport(Bean bean, String reportName) throws Exception {
		return EXT.getReporting().getBeanReport(bean, reportName);
	}
}
