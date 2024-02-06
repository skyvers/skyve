package org.skyve.impl.report.jasperreports;

import java.io.File;
import java.io.OutputStream;
import java.sql.Connection;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import javax.annotation.Nonnull;

import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.domain.Bean;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.util.ReportParameters;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.AbstractWebContext;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;
import org.skyve.metadata.repository.ProvidedRepository;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.model.list.ListModel;
import org.skyve.persistence.AutoClosingIterable;
import org.skyve.report.ReportFormat;

import net.sf.jasperreports.engine.JRAbstractExporter;
import net.sf.jasperreports.engine.JRDataSource;
import net.sf.jasperreports.engine.JasperFillManager;
import net.sf.jasperreports.engine.JasperPrint;
import net.sf.jasperreports.engine.JasperReport;
import net.sf.jasperreports.engine.export.HtmlExporter;
import net.sf.jasperreports.engine.export.JRCsvExporter;
import net.sf.jasperreports.engine.export.JRExporterContext;
import net.sf.jasperreports.engine.export.JRPdfExporter;
import net.sf.jasperreports.engine.export.JRRtfExporter;
import net.sf.jasperreports.engine.export.JRTextExporter;
import net.sf.jasperreports.engine.export.JRXlsExporter;
import net.sf.jasperreports.engine.export.JRXmlExporter;
import net.sf.jasperreports.engine.export.oasis.JROdsExporter;
import net.sf.jasperreports.engine.export.oasis.JROdtExporter;
import net.sf.jasperreports.engine.export.ooxml.JRDocxExporter;
import net.sf.jasperreports.engine.export.ooxml.JRPptxExporter;
import net.sf.jasperreports.engine.export.ooxml.JRXlsxExporter;
import net.sf.jasperreports.engine.util.JRLoader;
import net.sf.jasperreports.export.ExporterConfiguration;
import net.sf.jasperreports.export.ExporterOutput;
import net.sf.jasperreports.export.ReportExportConfiguration;
import net.sf.jasperreports.export.SimpleExporterInput;
import net.sf.jasperreports.export.SimpleHtmlExporterOutput;
import net.sf.jasperreports.export.SimpleOdsReportConfiguration;
import net.sf.jasperreports.export.SimpleOutputStreamExporterOutput;
import net.sf.jasperreports.export.SimpleTextReportConfiguration;
import net.sf.jasperreports.export.SimpleWriterExporterOutput;
import net.sf.jasperreports.export.SimpleXlsReportConfiguration;
import net.sf.jasperreports.export.SimpleXlsxReportConfiguration;
import net.sf.jasperreports.export.SimpleXmlExporterOutput;
import net.sf.jasperreports.web.util.WebHtmlResourceHandler;

public final class JasperReportUtil {
	private JasperReportUtil() {
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
		// Cast to null to remove method call ambiguity, this does not throw an NPE.
		return runReport(user, document, reportName, parameters, (Bean) null, format, out);
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

	public static JasperPrint runReport(User user,
											Document document,
											String reportName,
											Map<String, Object> parameters,
											ListModel<Bean> listModel,
											ReportFormat format,
											OutputStream out)
	throws Exception {
		final Customer customer = user.getCustomer();
		final String reportFileName = preProcess(customer, document, reportName, parameters);
		final JasperReport jasperReport = (JasperReport) JRLoader.loadObject(new File(reportFileName));
		return runReport(jasperReport, user, parameters, listModel, format, out);
	}

	public static JasperPrint runReport(JasperReport jasperReport,
											User user,
											Document document,
											Map<String, Object> parameters,
											Bean bean,
											ReportFormat format,
											OutputStream out)
	throws Exception {
		String queryLanguage = jasperReport.getQuery().getLanguage();
		UtilImpl.LOGGER.info("QUERY LNG = " + queryLanguage);

		JasperPrint result = null;
		if ("sql".equalsIgnoreCase(queryLanguage)) {
			result = fillSqlReport(jasperReport, parameters, format, out);
		}
		else if ("document".equalsIgnoreCase(queryLanguage)) {
			UtilImpl.LOGGER.info("FILL REPORT");
			Bean reportBean = bean;
			// if we have no bean then see if there is a bizId parameter
			if (reportBean == null) {
				String id = (String) parameters.get(AbstractWebContext.ID_NAME);
				// if we have a bizId then assume its persistent and load it
				if (id != null) {
					reportBean = AbstractPersistence.get().retrieve(document, id);
				}
			}
			result = JasperFillManager.fillReport(jasperReport, parameters, new SkyveDataSource(user, reportBean));
			UtilImpl.LOGGER.info("PUMP REPORT");
			runReport(result, format, out);
			UtilImpl.LOGGER.info("PUMPED REPORT");
		}

		return result;
	}

	public static JasperPrint runReport(JasperReport jasperReport,
											User user,
											Map<String, Object> parameters,
											ListModel<Bean> listModel,
											ReportFormat format,
											OutputStream out)
	throws Exception {
		UtilImpl.LOGGER.info("FILL REPORT");
		JasperPrint result;
		try (AutoClosingIterable<Bean> iterable = listModel.iterate()) {
			final JRDataSource dataSource = new SkyveDataSource(user, iterable.iterator());
			result = JasperFillManager.fillReport(jasperReport, parameters, dataSource);
		}
		UtilImpl.LOGGER.info("PUMP REPORT");
		runReport(result, format, out);
		UtilImpl.LOGGER.info("PUMPED REPORT");

		return result;
	}

	private static JasperPrint fillSqlReport(JasperReport jasperReport,
										Map<String, Object> parameters,
										ReportFormat format,
										OutputStream out)
	throws Exception {
		UtilImpl.LOGGER.info("FILL REPORT");
		JasperPrint result;
		try (Connection connection = EXT.getDataStoreConnection()) {
			result = JasperFillManager.fillReport(jasperReport, parameters, connection);
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
				UtilImpl.LOGGER.info("FILL REPORT");
				try (Connection connection = EXT.getDataStoreConnection()) {
					result.add(JasperFillManager.fillReport(jasperReport, reportParameter.getParameters(), connection));
					UtilImpl.LOGGER.info("PUMP REPORT");
					runReport(result, format, out);
					UtilImpl.LOGGER.info("PUMPED REPORT");
				}
			}
			else if ("document".equalsIgnoreCase(queryLanguage)) {
				UtilImpl.LOGGER.info("FILL REPORT");
				Bean reportBean = reportParameter.getBean();
				// if we have no bean then see if there is a bizId parameter
				if (reportBean == null) {
					String id = (String) reportParameter.getParameters().get(AbstractWebContext.ID_NAME);
					// if we have a bizId then assume its persistent and load it
					if (id != null) {
						reportBean = AbstractPersistence.get().retrieve(reportParameter.getDocument(), id);
					}
				}
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

	private static String preProcess(@Nonnull Customer customer, @Nonnull ReportParameters reportParameters) {
		return preProcess(customer, reportParameters.getDocument(), reportParameters.getReportName(), reportParameters.getParameters());
	}

	/**
	 * Adds intrinsic parameters and returns the report file name
	 * 
	 * @param customer
	 * @param document
	 * @param reportName
	 * @param parameters
	 * @return report file name
	 */
	private static @Nonnull String preProcess(@Nonnull Customer customer,
												@Nonnull Document document,
												@Nonnull String reportName,
												@Nonnull Map<String, Object> parameters) {
		ProvidedRepository repository = ProvidedRepositoryFactory.get();
		String result = repository.getReportFileName(customer, document, reportName);
		if (result == null) {
			throw new MetaDataException("Report " + reportName + " in document " + document.getOwningModuleName() + '.' + document.getName() + 
											" for customer " + customer.getName() + " does not exist.");
		}
		StringBuilder sb = new StringBuilder(256);
		sb.append(UtilImpl.getAbsoluteBasePath()).append(ProvidedRepository.CUSTOMERS_NAMESPACE);
		sb.append(customer.getName()).append('/').append(ProvidedRepository.RESOURCES_NAMESPACE);
		parameters.put("RESOURCE_DIR", sb.toString());
		sb.setLength(0);
		sb.append(UtilImpl.getAbsoluteBasePath()).append(ProvidedRepository.MODULES_NAMESPACE);
		sb.append(document.getOwningModuleName()).append('/').append(ProvidedRepository.RESOURCES_NAMESPACE);
		parameters.put("MODULE_RESOURCE_DIR", sb.toString());
		int lastFileSeparator = result.lastIndexOf('/');
		parameters.put("SUBREPORT_DIR", result.substring(0, lastFileSeparator + 1));

		return result;
	}

	public static void runReport(JasperPrint jasperPrint,
									ReportFormat format,
									OutputStream out)
	throws Exception {
		final JRAbstractExporter<? extends ReportExportConfiguration, ? extends ExporterConfiguration, ? extends ExporterOutput, ? extends JRExporterContext> exporter = getExporter(format, out);
		exporter.setExporterInput(new SimpleExporterInput(jasperPrint));
		exporter.exportReport();
	}

	public static void runReport(List<JasperPrint> jasperPrintList, ReportFormat format, OutputStream out)
	throws Exception {
		final JRAbstractExporter<? extends ReportExportConfiguration, ? extends ExporterConfiguration, ? extends ExporterOutput, ? extends JRExporterContext> exporter = getExporter(format, out);
		exporter.setExporterInput(SimpleExporterInput.getInstance(jasperPrintList));
		exporter.exportReport();
	}

	private static JRAbstractExporter<? extends ReportExportConfiguration, ? extends ExporterConfiguration, ? extends ExporterOutput, ? extends JRExporterContext> getExporter(ReportFormat format, OutputStream out) {
		JRAbstractExporter<? extends ReportExportConfiguration, ? extends ExporterConfiguration, ? extends ExporterOutput, ? extends JRExporterContext> result;
		switch (format) {
		case txt:
			SimpleTextReportConfiguration textConfig = new SimpleTextReportConfiguration();
			textConfig.setPageWidthInChars(Integer.valueOf(80));
			textConfig.setPageHeightInChars(Integer.valueOf(24));
			JRTextExporter text = new JRTextExporter();
			text.setConfiguration(textConfig);
			text.setExporterOutput(new SimpleWriterExporterOutput(out));
			result = text;
			break;
		case csv:
			JRCsvExporter csv = new JRCsvExporter();
			csv.setExporterOutput(new SimpleWriterExporterOutput(out));
			result = csv;
			break;
		case html:
			SimpleHtmlExporterOutput htmlOutput = new SimpleHtmlExporterOutput(out);
			htmlOutput.setImageHandler(new WebHtmlResourceHandler("image?image={0}"));
			HtmlExporter html = new HtmlExporter();
			html.setExporterOutput(htmlOutput);
			result = html;
			break;
		case pdf:
			JRPdfExporter pdf = new JRPdfExporter();
			pdf.setExporterOutput(new SimpleOutputStreamExporterOutput(out));
			result = pdf;
			break;
		case xls:
			SimpleXlsReportConfiguration xlsConfig = new SimpleXlsReportConfiguration();
			xlsConfig.setOnePagePerSheet(Boolean.FALSE);
			xlsConfig.setRemoveEmptySpaceBetweenRows(Boolean.TRUE);
			xlsConfig.setRemoveEmptySpaceBetweenColumns(Boolean.TRUE);
			xlsConfig.setUseTimeZone(Boolean.TRUE);
			xlsConfig.setWhitePageBackground(Boolean.FALSE);
			xlsConfig.setDetectCellType(Boolean.TRUE);
			JRXlsExporter xls = new JRXlsExporter();
			xls.setConfiguration(xlsConfig);
			xls.setExporterOutput(new SimpleOutputStreamExporterOutput(out));
			result = xls;
			break;
		case rtf:
			JRRtfExporter rtf = new JRRtfExporter();
			rtf.setExporterOutput(new SimpleWriterExporterOutput(out));
			result = rtf;
			break;
		case odt:
			JROdtExporter odt = new JROdtExporter();
			odt.setExporterOutput(new SimpleOutputStreamExporterOutput(out));
			result = odt;
			break;
		case ods:
			SimpleOdsReportConfiguration odsConfig = new SimpleOdsReportConfiguration();
			odsConfig.setOnePagePerSheet(Boolean.FALSE);
			odsConfig.setRemoveEmptySpaceBetweenRows(Boolean.TRUE);
			odsConfig.setRemoveEmptySpaceBetweenColumns(Boolean.TRUE);
			odsConfig.setUseTimeZone(Boolean.TRUE);
			odsConfig.setWhitePageBackground(Boolean.FALSE);
			odsConfig.setDetectCellType(Boolean.TRUE);
			JROdsExporter ods = new JROdsExporter();
			ods.setExporterOutput(new SimpleOutputStreamExporterOutput(out));
			result = ods;
			break;
		case docx:
			JRDocxExporter docx = new JRDocxExporter();
			docx.setExporterOutput(new SimpleOutputStreamExporterOutput(out));
			result = docx;
			break;
		case xlsx:
			SimpleXlsxReportConfiguration xlsxConfig = new SimpleXlsxReportConfiguration();
			xlsxConfig.setOnePagePerSheet(Boolean.FALSE);
			xlsxConfig.setRemoveEmptySpaceBetweenRows(Boolean.TRUE);
			xlsxConfig.setRemoveEmptySpaceBetweenColumns(Boolean.TRUE);
			xlsxConfig.setUseTimeZone(Boolean.TRUE);
			xlsxConfig.setWhitePageBackground(Boolean.FALSE);
			xlsxConfig.setDetectCellType(Boolean.TRUE);
			JRXlsxExporter xlsx = new JRXlsxExporter();
			xlsx.setConfiguration(xlsxConfig);
			xlsx.setExporterOutput(new SimpleOutputStreamExporterOutput(out));
			result = xlsx;
			break;
		case pptx:
			JRPptxExporter pptx = new JRPptxExporter();
			pptx.setExporterOutput(new SimpleOutputStreamExporterOutput(out));
			result = pptx;
			break;
		case xml:
			JRXmlExporter xml = new JRXmlExporter();
			xml.setExporterOutput(new SimpleXmlExporterOutput(out));
			result = xml;
			break;
		default:
			throw new IllegalStateException("Report format " + format + " not catered for.");
		}

		return result;
	}

	public static ListModel<Bean> getQueryListModel(Module module, String documentOrQueryOrModelName) {
		final Customer customer = CORE.getCustomer();
		MetaDataQueryDefinition query = module.getMetaDataQuery(documentOrQueryOrModelName);
		if (query == null) {
			query = module.getDocumentDefaultQuery(customer, documentOrQueryOrModelName);
		}
		if (query == null) {
			throw new IllegalArgumentException("DataSource does not reference a valid query " + documentOrQueryOrModelName);
		}

		return EXT.newListModel(query);
	}
}
