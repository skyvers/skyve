package org.skyve.impl.util;

import java.io.File;
import java.io.OutputStream;
import java.sql.Connection;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.skyve.EXT;
import org.skyve.domain.Bean;
import org.skyve.impl.jasperreports.SkyveDataSource;
import org.skyve.impl.metadata.repository.AbstractRepository;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.web.AbstractWebContext;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.user.User;
import org.skyve.report.ReportFormat;

import net.sf.jasperreports.engine.JRAbstractExporter;
import net.sf.jasperreports.engine.JRException;
import net.sf.jasperreports.engine.JRExporterParameter;
import net.sf.jasperreports.engine.JasperFillManager;
import net.sf.jasperreports.engine.JasperPrint;
import net.sf.jasperreports.engine.JasperReport;
import net.sf.jasperreports.engine.export.JExcelApiExporter;
import net.sf.jasperreports.engine.export.JRCsvExporter;
import net.sf.jasperreports.engine.export.JRHtmlExporter;
import net.sf.jasperreports.engine.export.JRHtmlExporterParameter;
import net.sf.jasperreports.engine.export.JRPdfExporter;
import net.sf.jasperreports.engine.export.JRRtfExporter;
import net.sf.jasperreports.engine.export.JRTextExporter;
import net.sf.jasperreports.engine.export.JRTextExporterParameter;
import net.sf.jasperreports.engine.export.JRXhtmlExporter;
import net.sf.jasperreports.engine.export.JRXlsAbstractExporterParameter;
import net.sf.jasperreports.engine.export.JRXmlExporter;
import net.sf.jasperreports.engine.export.oasis.JROdsExporter;
import net.sf.jasperreports.engine.export.oasis.JROdtExporter;
import net.sf.jasperreports.engine.export.ooxml.JRDocxExporter;
import net.sf.jasperreports.engine.export.ooxml.JRPptxExporter;
import net.sf.jasperreports.engine.export.ooxml.JRXlsxExporter;
import net.sf.jasperreports.engine.util.JRLoader;

public final class ReportUtil {
	private ReportUtil() {
		// disallow instantiation
	}
	
	public static JasperPrint runBeanReport(User user,
												Document document,
												String reportName,
												Map<String, Object> parameters,
												Bean bean,
												ReportFormat format,
												OutputStream out)
	throws Exception {
		return runReport(user, document, reportName, parameters, bean, format, out);
	}

	public static JasperPrint runSQLReport(User user,
											Document document,
											String reportName,
											Map<String, Object> parameters,
											ReportFormat format,
											OutputStream out)
	throws Exception {
		return runReport(user, document, reportName, parameters, null, format, out);
	}

	public static JasperPrint runReport(User user,
											Document document, 
											String reportName, 
											Map<String, Object> parameters,
											Bean bean,
											ReportFormat format,
											OutputStream out)
	throws Exception {
		final Customer customer = user.getCustomer();
		final String reportFileName = preProcess(customer, document, reportName, parameters);

        final JasperReport jasperReport = (JasperReport) JRLoader.loadObject(new File(reportFileName));

		return runReport(jasperReport, user, document, parameters, bean, format, out);
	}

	public static JasperPrint runReport(JasperReport jasperReport,
	                                    User user,
	                                    Document document,
	                                    Map<String, Object> parameters,
	                                    Bean bean,
	                                    ReportFormat format,
	                                    OutputStream out) throws JRException {
		String queryLanguage = jasperReport.getQuery().getLanguage();

		JasperPrint result = null;

		UtilImpl.LOGGER.info("QUERY LNG = " + queryLanguage);
		if ("sql".equalsIgnoreCase(queryLanguage)) {
			@SuppressWarnings("resource")
			Connection connection = EXT.getDataStoreConnection();
			try {
				UtilImpl.LOGGER.info("FILL REPORT");
				result = JasperFillManager.fillReport(jasperReport,
						parameters,
						connection);
				UtilImpl.LOGGER.info("PUMP REPORT");
				runReport(result, format, out);
				UtilImpl.LOGGER.info("PUMPED REPORT");
			}
			finally {
				SQLUtil.disconnect(connection);
			}
		}
		else if ("document".equalsIgnoreCase(queryLanguage)) {
			Bean reportBean = bean;
			// if we have no bean then see if there is a bizId parameter
			if (reportBean == null) {
				String id = (String) parameters.get(AbstractWebContext.ID_NAME);
				// if we have a bizId then assume its persistent and load it
				if (id != null) {
					reportBean = AbstractPersistence.get().retrieve(document, id, false);
				}
			}
			UtilImpl.LOGGER.info("FILL REPORT");
			result = JasperFillManager.fillReport(jasperReport,
					parameters,
					new SkyveDataSource(user, reportBean));
			UtilImpl.LOGGER.info("PUMP REPORT");
			runReport(result, format, out);
			UtilImpl.LOGGER.info("PUMPED REPORT");
		}

		return result;
	}

	public static List<JasperPrint> runReport(User user,
										List<ReportParameters> reportParameters,
										ReportFormat format,
										OutputStream out)
			throws Exception {
		final Customer customer = user.getCustomer();

		final List<JasperPrint> result = new ArrayList<>();
		for (ReportParameters reportParameter : reportParameters) {
			final String reportFileName = preProcess(customer, reportParameter);
			final JasperReport jasperReport = (JasperReport) JRLoader.loadObject(new File(reportFileName));
			final String queryLanguage = jasperReport.getQuery().getLanguage();


			UtilImpl.LOGGER.info("QUERY LNG = " + queryLanguage);
			if ("sql".equalsIgnoreCase(queryLanguage)) {
				@SuppressWarnings("resource")
				Connection connection = EXT.getDataStoreConnection();
				try {
					UtilImpl.LOGGER.info("FILL REPORT");
					result.add(JasperFillManager.fillReport(jasperReport,
							reportParameter.getParameters(),
							connection));
					UtilImpl.LOGGER.info("PUMP REPORT");
					runReport(result, format, out);
					UtilImpl.LOGGER.info("PUMPED REPORT");
				}
				finally {
					SQLUtil.disconnect(connection);
				}
			}
			else if ("document".equalsIgnoreCase(queryLanguage)) {
				Bean reportBean = reportParameter.getBean();
				// if we have no bean then see if there is a bizId parameter
				if (reportBean == null) {
					String id = (String) reportParameter.getParameters().get(AbstractWebContext.ID_NAME);
					// if we have a bizId then assume its persistent and load it
					if (id != null) {
						reportBean = AbstractPersistence.get().retrieve(reportParameter.getDocument(), id, false);
					}
				}
				UtilImpl.LOGGER.info("FILL REPORT");
				result.add(JasperFillManager.fillReport(jasperReport,
						reportParameter.getParameters(),
						new SkyveDataSource(user, reportBean)));
			}
		}

		UtilImpl.LOGGER.info("PUMP REPORT");
		runReport(result, format, out);
		UtilImpl.LOGGER.info("PUMPED REPORT");

		return result;
	}

	private static String preProcess(Customer customer, ReportParameters reportParameters) {
		return preProcess(customer,
				reportParameters.getDocument(),
				reportParameters.getReportName(),
				reportParameters.getParameters());
	}

	/**
	 * Adds intrinsic parameters and returns the report file name
	 * @param customer
	 * @param document
	 * @param reportName
	 * @param parameters
	 * @return report file name
	 */
	private static String preProcess(Customer customer,
										Document document,
										String reportName,
										Map<String, Object> parameters) {
		AbstractRepository repository = AbstractRepository.get();
		String result = repository.getReportFileName(customer, document, reportName);
		StringBuilder sb = new StringBuilder(256);
		sb.append(UtilImpl.getAbsoluteBasePath()).append(repository.CUSTOMERS_NAMESPACE);
		sb.append(customer.getName()).append('/').append(repository.RESOURCES_NAMESPACE);
		parameters.put("RESOURCE_DIR", sb.toString());
		sb.setLength(0);
		sb.append(UtilImpl.getAbsoluteBasePath()).append(repository.MODULES_NAMESPACE);
		sb.append(document.getOwningModuleName()).append('/').append(repository.RESOURCES_NAMESPACE);
		parameters.put("MODULE_RESOURCE_DIR", sb.toString());
		int lastFileSeparator = result.lastIndexOf('/');
		parameters.put("SUBREPORT_DIR", result.substring(0, lastFileSeparator + 1));
		
		return result;
	}
	
	public static void runReport(JasperPrint jasperPrint, 
									ReportFormat format,
									OutputStream out)
	throws JRException {
		final JRAbstractExporter exporter = getExporter(format);

		exporter.setParameter(JRExporterParameter.JASPER_PRINT, jasperPrint);
		exporter.setParameter(JRExporterParameter.OUTPUT_STREAM, out);
		exporter.exportReport();
	}

	public static void runReport(List<JasperPrint> jasperPrintList,
								 ReportFormat format,
								 OutputStream out)
			throws JRException {
		final JRAbstractExporter exporter = getExporter(format);

		exporter.setParameter(JRExporterParameter.JASPER_PRINT_LIST, jasperPrintList);
		exporter.setParameter(JRExporterParameter.OUTPUT_STREAM, out);
		exporter.exportReport();
	}

	private static JRAbstractExporter getExporter(ReportFormat format) {
		JRAbstractExporter exporter;
		switch (format) {
			case txt:
				exporter = new JRTextExporter();
				exporter.setParameter(JRTextExporterParameter.PAGE_WIDTH, new Integer(80));
				exporter.setParameter(JRTextExporterParameter.PAGE_HEIGHT, new Integer(24));
				break;
			case csv:
				exporter = new JRCsvExporter();
				break;
			case html:
				exporter = new JRHtmlExporter();
				exporter.setParameter(JRHtmlExporterParameter.IMAGES_URI, "image?image=");
				break;
			case xhtml:
				exporter = new JRXhtmlExporter();
				exporter.setParameter(JRHtmlExporterParameter.IMAGES_URI, "image?image=");
				break;
			case pdf:
				exporter = new JRPdfExporter();
				break;
			case xls:
				exporter = new JExcelApiExporter(); // JRXlsExporter(); POI doesn't handle embedded images very well
				exporter.setParameter(JRXlsAbstractExporterParameter.IS_ONE_PAGE_PER_SHEET, Boolean.FALSE);
				exporter.setParameter(JRXlsAbstractExporterParameter.IS_WHITE_PAGE_BACKGROUND, Boolean.FALSE);
				break;
			case rtf:
				exporter = new JRRtfExporter();
				break;
			case odt:
				exporter = new JROdtExporter();
				break;
			case ods:
				exporter = new JROdsExporter();
				break;
			case docx:
				exporter = new JRDocxExporter();
				break;
			case xlsx:
				exporter = new JRXlsxExporter();
				break;
			case pptx:
				exporter = new JRPptxExporter();
				break;
			case xml:
				exporter = new JRXmlExporter();
//			exporter.setParameter(JRXmlExporterParameter.DTD_LOCATION, "");
				break;
			default:
				throw new IllegalStateException("Report format " + format + " not catered for.");
		}

		return exporter;
	}
}
