package org.skyve.impl.cdi;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.spy;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.Test;
import org.skyve.CORE;
import org.skyve.domain.app.AppConstants;
import org.skyve.domain.Bean;
import org.skyve.impl.generate.jasperreports.DesignSpecification;
import org.skyve.impl.generate.jasperreports.DocumentReportDesignGenerator;
import org.skyve.impl.generate.jasperreports.JasperReportRenderer;
import org.skyve.impl.report.freemarker.FreemarkerReportUtil;
import org.skyve.impl.report.jasperreports.SkyveDataSource;
import org.skyve.impl.util.ReportParameters;
import org.skyve.impl.web.AbstractWebContext;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.repository.ProvidedRepository;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.metadata.user.User;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.Persistence;
import org.skyve.report.ReportFormat;

import modules.test.domain.AllAttributesPersistent;
import net.sf.jasperreports.engine.JasperFillManager;
import net.sf.jasperreports.engine.JasperPrint;
import net.sf.jasperreports.engine.JasperReport;
import net.sf.jasperreports.engine.util.JRSaver;
import util.AbstractH2Test;

@SuppressWarnings("static-method")
class ReportingInjectableH2Test extends AbstractH2Test {
	@Test
	@SuppressWarnings("java:S2093")
	void delegatesFreemarkerTemplateAndPdfOperations() throws Exception {
		ReportingInjectable injectable = new ReportingInjectable();
		String templateName = "reportingInjectableH2/simple.html";
		String pdfTemplateName = "reportingInjectableH2/pdf.html";
		String html = "<html><body><h1>Skyve</h1><p>H2 reporting test</p></body></html>";
		File htmlPdfFile = new File("target/reportingInjectable-h2-html.pdf");
		File urlPdfFile = new File("target/reportingInjectable-h2-url.pdf");
		File templatePdfFile;

		injectable.startup();
		injectable.addTemplate(templateName, "Hello ${name}");
		injectable.addTemplate(pdfTemplateName, html);
		try {
			assertNotNull(injectable.getFreemarkerTemplate(templateName));
			String rendered = injectable.createFreemarkerReport(templateName, Map.of("name", "Skyve"));
			assertTrue(rendered.contains("Hello Skyve"));

			injectable.generateFreemarkerPDFFromHTML(html, htmlPdfFile);
			assertTrue(htmlPdfFile.exists());
			assertTrue(htmlPdfFile.length() > 0L);

			ByteArrayOutputStream out = new ByteArrayOutputStream();
			injectable.generateFreemarkerPDFFromHTML(new ByteArrayInputStream(html.getBytes(StandardCharsets.UTF_8)), out);
			assertTrue(out.size() > 0);

			injectable.generateFreemarkerPDFFromHTML(html, out);
			assertTrue(out.size() > 0);

			injectable.generateFreemarkerPDFFromHTMLURL(htmlPdfFile.toURI().toURL().toExternalForm(), urlPdfFile);
			assertTrue(urlPdfFile.exists());
			assertTrue(urlPdfFile.length() > 0L);

			templatePdfFile = injectable.createFreemarkerReportPDF(pdfTemplateName, Map.of(), "reportingInjectable-h2-template");
			assertNotNull(templatePdfFile);
			assertTrue(templatePdfFile.exists());
			assertTrue(templatePdfFile.length() > 0L);
		}
		finally {
			FreemarkerReportUtil.removeTemplate(templateName);
			FreemarkerReportUtil.removeTemplate(pdfTemplateName);
			injectable.shutdown();
		}
	}

	@Test
	@SuppressWarnings("java:S2093")
	void delegatesBeanFreemarkerReportOperations() throws Exception {
		ReportingInjectable injectable = new ReportingInjectable();
		AllAttributesPersistent bean = AllAttributesPersistent.newInstance();
		bean.setText("bean-text");
		bean = CORE.getPersistence().save(bean);

		String reportName = "report.html";
		String beanTemplateKey = String.format("%s/%s/reports/%s", bean.getBizModule(), bean.getBizDocument(), reportName);
		injectable.startup();
		injectable.addTemplate(beanTemplateKey, "Bean value: ${value}");
		try {
			assertNotNull(injectable.getBeanReport(bean, reportName));
			String rendered = injectable.createFreemarkerBeanReport(bean, reportName, Map.of("value", bean.getText()));
			assertTrue(rendered.contains("bean-text"));

			File pdf = injectable.createFreemarkerBeanReportPDF(bean, reportName, Map.of("value", bean.getText()),
					"reportingInjectable-h2-bean");
			assertNotNull(pdf);
			assertTrue(pdf.exists());
			assertTrue(pdf.length() > 0L);
		}
		finally {
			FreemarkerReportUtil.removeTemplate(beanTemplateKey);
			injectable.shutdown();
		}
	}

	@Test
	void delegatesJasperPrintRenderingOperations() throws Exception {
		ReportingInjectable injectable = new ReportingInjectable();
		JasperPrint print = createFilledJasperPrint();
		ByteArrayOutputStream singleOut = new ByteArrayOutputStream();
		ByteArrayOutputStream listOut = new ByteArrayOutputStream();

		injectable.runJasperReport(print, ReportFormat.pdf, singleOut);
		injectable.runJasperReport(List.of(print), ReportFormat.pdf, listOut);

		assertTrue(singleOut.size() > 0);
		assertTrue(listOut.size() > 0);
		assertFalse(print.getPages().isEmpty());
	}

	@Test
	void delegatesRemainingOverloadsForCoverage() throws Exception {
		ReportingInjectable injectable = new ReportingInjectable();
		Persistence persistence = CORE.getPersistence();
		User user = persistence.getUser();

		AllAttributesPersistent bean = AllAttributesPersistent.newInstance();
		bean.setText("delegatesRemainingOverloadsForCoverage");
		bean = persistence.save(bean);
		final AllAttributesPersistent savedBean = bean;

		Document document = user.getCustomer()
								.getModule(savedBean.getBizModule())
								.getDocument(user.getCustomer(), savedBean.getBizDocument());
								
		ByteArrayOutputStream out = new ByteArrayOutputStream();
		Map<String, Object> parameters = Map.of();

		String templateName = "reportingInjectableH2/remaining-overloads.html";
		String templateMarkup = "<html><body>Remaining ${name}</body></html>";
		String reportName = "missing-report-template";
		File inToFilePdf = new File("target/reportingInjectable-h2-in-to-file.pdf");

		injectable.startup();
		injectable.addTemplate(templateName, templateMarkup);
		try {
			try (ByteArrayInputStream in = new ByteArrayInputStream(templateMarkup.getBytes(StandardCharsets.UTF_8))) {
				injectable.generateFreemarkerPDFFromHTML(in, inToFilePdf);
			}
			assertTrue(inToFilePdf.exists());
			assertTrue(inToFilePdf.length() > 0L);

			assertThrows(Exception.class,
					() -> injectable.runFreemarkerReport(templateName, Map.of("name", "Skyve")));

			assertThrows(Exception.class,
					() -> injectable.downloadFreemarkerReport(templateName,
							Map.of("name", "Skyve"),
							ReportFormat.pdf,
							"reportingInjectable-remaining-overloads"));

			assertThrows(Exception.class, () -> injectable.runJasperBeanReport(user,
					document,
					reportName,
					parameters,
					savedBean,
					ReportFormat.pdf,
					out));

			assertThrows(Exception.class, () -> injectable.runJasperSQLReport(user,
					document,
					reportName,
					parameters,
					ReportFormat.pdf,
					out));

			assertThrows(Exception.class, () -> injectable.runJasperReport(user,
					document,
					reportName,
					parameters,
					savedBean,
					ReportFormat.pdf,
					out));

			assertThrows(Exception.class, () -> injectable.runJasperReport(user,
					List.of(new ReportParameters(document, reportName, parameters, savedBean)),
					ReportFormat.pdf,
					out));

			assertThrows(Exception.class, () -> injectable.getMailAttachmentFromJasperReport(
					savedBean.getBizModule(),
					savedBean.getBizDocument(),
					reportName,
					parameters));

			assertThrows(Exception.class, () -> injectable.getMailAttachmentFromJasperReport(
					List.of(new ReportParameters(document, reportName, parameters, savedBean))));
		}
		finally {
			FreemarkerReportUtil.removeTemplate(templateName);
			injectable.shutdown();
		}
	}

	@Test
	void delegatesRemainingOverloadsWithSuccessfulReturns() throws Exception {
		ReportingInjectable injectable = new ReportingInjectable();
		Persistence persistence = CORE.getPersistence();
		User user = persistence.getUser();

		AllAttributesPersistent bean = AllAttributesPersistent.newInstance();
		bean.setText("delegatesRemainingOverloadsWithSuccessfulReturns");
		bean = persistence.save(bean);

		Document document = user.getCustomer()
								.getModule(bean.getBizModule())
								.getDocument(user.getCustomer(), bean.getBizDocument());

		modules.admin.ReportTemplate.ReportTemplateExtension reportTemplate = modules.admin.domain.ReportTemplate.newInstance();
		reportTemplate.setName("Reporting Injectable H2 DB Template");
		reportTemplate.setTemplateName("reportingInjectableH2-db-template.html");
		reportTemplate.setTemplate("<html><body><p>DB template path</p></body></html>");
		persistence.save(reportTemplate);
		persistence.flush();

		DocumentQuery query = persistence.newDocumentQuery(AppConstants.ADMIN_MODULE_NAME,
				AppConstants.REPORT_TEMPLATE_DOCUMENT_NAME);
		query.getFilter().addEquals(AppConstants.TEMPLATE_NAME_ATTRIBUTE_NAME, reportTemplate.getTemplateName());
		assertNotNull(query.beanResult());

		File documentReportFile = createGeneratedReportFile(DesignSpecification.Mode.bean,
				"reportingInjectable-h2-doc-report-file");

		ProvidedRepository originalRepository = ProvidedRepositoryFactory.get();
		ProvidedRepository repositorySpy = spy(originalRepository);
		doReturn(documentReportFile.getAbsolutePath()).when(repositorySpy).getReportFileName(any(), any(), eq("doc-report"));
		doReturn(documentReportFile.getAbsolutePath()).when(repositorySpy).getReportFileName(any(), any(), eq("sql-report"));
		ProvidedRepositoryFactory.set(repositorySpy);
		injectable.startup();

		Map<String, Object> mutableParameters = new HashMap<>();
		mutableParameters.put(AbstractWebContext.ID_NAME, bean.getBizId());
		try (ByteArrayOutputStream out = new ByteArrayOutputStream()) {
			assertNotNull(injectable.runJasperBeanReport(user,
					document,
					"doc-report",
					new HashMap<>(mutableParameters),
					bean,
					ReportFormat.pdf,
					out));

			assertNotNull(injectable.runJasperSQLReport(user,
					document,
					"sql-report",
					new HashMap<>(mutableParameters),
					ReportFormat.pdf,
					out));

			assertNotNull(injectable.runJasperReport(user,
					document,
					"doc-report",
					new HashMap<>(mutableParameters),
					bean,
					ReportFormat.pdf,
					out));

			assertFalse(injectable.runJasperReport(user,
					List.of(new ReportParameters(document, "doc-report", new HashMap<>(mutableParameters), bean)),
					ReportFormat.pdf,
					out).isEmpty());

			assertNotNull(injectable.getMailAttachmentFromJasperReport(bean.getBizModule(),
					bean.getBizDocument(),
					"sql-report",
					new HashMap<>(mutableParameters)));

			assertNotNull(injectable.getMailAttachmentFromJasperReport(
					List.of(new ReportParameters(document, "doc-report", new HashMap<>(mutableParameters), bean))));

			String rendered = injectable.runFreemarkerReport(reportTemplate.getTemplateName(), Map.of());
			assertTrue(rendered.contains("DB template path"));

			assertNotNull(injectable.downloadFreemarkerReport(reportTemplate.getTemplateName(),
					Map.of(),
					ReportFormat.csv,
					"reportingInjectable-db-template-download"));
		}
		finally {
			ProvidedRepositoryFactory.set(originalRepository);
			persistence.delete(reportTemplate);
			injectable.shutdown();
		}
	}

	private static JasperPrint createFilledJasperPrint() throws Exception {
		Persistence p = CORE.getPersistence();
		User user = p.getUser();

		AllAttributesPersistent bean = AllAttributesPersistent.newInstance();
		bean.setText("reportingInjectable jasper");
		bean = p.save(bean);

		DesignSpecification spec = new DesignSpecification();
		spec.setModuleName(AllAttributesPersistent.MODULE_NAME);
		spec.setDocumentName(AllAttributesPersistent.DOCUMENT_NAME);
		spec.setDefinitionSource(DesignSpecification.DefinitionSource.document);
		spec.setReportType(DesignSpecification.ReportType.report);
		spec.setMode(DesignSpecification.Mode.bean);
		spec.setName("reportingInjectableH2");
		spec.setOrientation(DesignSpecification.Orientation.portrait);
		spec.setWidth(Integer.valueOf(842));
		spec.setHeight(Integer.valueOf(595));
		spec.setLeftMargin(Integer.valueOf(20));
		spec.setRightMargin(Integer.valueOf(20));
		spec.setTopMargin(Integer.valueOf(20));
		spec.setBottomMargin(Integer.valueOf(20));
		spec.setColumnWidth(Integer.valueOf(802));
		spec.setDefaultElementHeight(Integer.valueOf(20));
		spec.setDefaultFontSize(Integer.valueOf(10));

		new DocumentReportDesignGenerator().populateDesign(spec);
		JasperReport report = new JasperReportRenderer(spec).getReport();

		return JasperFillManager.fillReport(report, new HashMap<>(), new SkyveDataSource(user, List.<Bean>of(bean)));
	}

	private static File createGeneratedReportFile(DesignSpecification.Mode mode, String filenamePrefix) throws Exception {
		File reportFile = new File("target/" + filenamePrefix + ".jasper");
		DesignSpecification spec = new DesignSpecification();
		spec.setModuleName(AllAttributesPersistent.MODULE_NAME);
		spec.setDocumentName(AllAttributesPersistent.DOCUMENT_NAME);
		spec.setDefinitionSource(DesignSpecification.DefinitionSource.document);
		spec.setReportType(DesignSpecification.ReportType.report);
		spec.setMode(mode);
		spec.setName(filenamePrefix);
		spec.setOrientation(DesignSpecification.Orientation.portrait);
		spec.setWidth(Integer.valueOf(842));
		spec.setHeight(Integer.valueOf(595));
		spec.setLeftMargin(Integer.valueOf(20));
		spec.setRightMargin(Integer.valueOf(20));
		spec.setTopMargin(Integer.valueOf(20));
		spec.setBottomMargin(Integer.valueOf(20));
		spec.setColumnWidth(Integer.valueOf(802));
		spec.setDefaultElementHeight(Integer.valueOf(20));
		spec.setDefaultFontSize(Integer.valueOf(10));

		new DocumentReportDesignGenerator().populateDesign(spec);
		JasperReport compiled = new JasperReportRenderer(spec).getReport();
		JRSaver.saveObject(compiled, reportFile.getAbsolutePath());
		return reportFile;
	}
}
