package util;

import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.ByteArrayOutputStream;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.CORE;
import org.skyve.impl.generate.jasperreports.DesignSpecification;
import org.skyve.impl.generate.jasperreports.DocumentReportDesignGenerator;
import org.skyve.impl.generate.jasperreports.JasperReportRenderer;
import org.skyve.impl.report.jasperreports.JasperReportUtil;
import org.skyve.impl.report.jasperreports.SkyveDataSource;
import org.skyve.metadata.user.User;
import org.skyve.persistence.Persistence;
import org.skyve.report.ReportFormat;

import modules.test.domain.AllAttributesPersistent;
import net.sf.jasperreports.engine.JasperFillManager;
import net.sf.jasperreports.engine.JasperPrint;
import net.sf.jasperreports.engine.JasperReport;

/**
 * Tier 0 — Pipeline smoke tests (upgrade gate).
 *
 * These tests must pass on JasperReports 6.20.5 and must be re-confirmed after upgrading
 * to 7.x. All seven methods guard a specific runtime JAR or API change described in
 * jasper-impacts.md.
 */
public class JasperReportPipelineH2Test extends AbstractH2Test {

	private JasperReport compiledReport;
	private JasperPrint filledPrint;

	@BeforeEach
	public void compileAndFill() throws Exception {
		Persistence p = CORE.getPersistence();
		User user = p.getUser();

		// Persist a minimal bean to use as the data source
		AllAttributesPersistent bean = AllAttributesPersistent.newInstance();
		bean.setText("pipeline smoke test");
		bean = p.save(bean);

		// Build a minimal DesignSpecification for AllAttributesPersistent
		DesignSpecification spec = buildMinimalSpec();

		// Populate fields and bands from document metadata so the compiled report has data rows
		new DocumentReportDesignGenerator().populateDesign(spec);

		// Compile: JasperReportRenderer → JasperCompileManager (guards JasperCompileManager API)
		JasperReportRenderer renderer = new JasperReportRenderer(spec);
		compiledReport = renderer.getReport();

		// Fill: pass a SkyveDataSource directly to avoid the query executer path
		Map<String, Object> params = new HashMap<>();
		SkyveDataSource dataSource = new SkyveDataSource(user, Collections.singletonList(bean));
		filledPrint = JasperFillManager.fillReport(compiledReport, params, dataSource);
	}

	@Test
	public void compilesDesignSpecToJasperReport() {
		assertThat(compiledReport, notNullValue());
	}

	@Test
	public void fillsReportWithSkyveDataSource() {
		assertThat(filledPrint, notNullValue());
	}

	@Test
	public void exportsToPdfProducesNonEmptyOutput() throws Exception {
		ByteArrayOutputStream out = new ByteArrayOutputStream();
		JasperReportUtil.runReport(filledPrint, ReportFormat.pdf, out);
		assertTrue(out.size() > 0, "PDF output must not be empty");
	}

	@Test
	public void exportsToXlsProducesNonEmptyOutput() throws Exception {
		ByteArrayOutputStream out = new ByteArrayOutputStream();
		JasperReportUtil.runReport(filledPrint, ReportFormat.xls, out);
		assertTrue(out.size() > 0, "XLS output must not be empty");
	}

	@Test
	public void exportsToXlsxProducesNonEmptyOutput() throws Exception {
		ByteArrayOutputStream out = new ByteArrayOutputStream();
		JasperReportUtil.runReport(filledPrint, ReportFormat.xlsx, out);
		assertTrue(out.size() > 0, "XLSX output must not be empty");
	}

	@Test
	public void exportsToHtmlProducesNonEmptyOutput() throws Exception {
		ByteArrayOutputStream out = new ByteArrayOutputStream();
		JasperReportUtil.runReport(filledPrint, ReportFormat.html, out);
		assertTrue(out.size() > 0, "HTML output must not be empty");
	}

	@Test
	public void exportsToCsvProducesNonEmptyOutput() throws Exception {
		ByteArrayOutputStream out = new ByteArrayOutputStream();
		JasperReportUtil.runReport(filledPrint, ReportFormat.csv, out);
		assertTrue(out.size() > 0, "CSV output must not be empty");
	}

	@Test
	public void exportsToRtfProducesNonEmptyOutput() throws Exception {
		ByteArrayOutputStream out = new ByteArrayOutputStream();
		JasperReportUtil.runReport(filledPrint, ReportFormat.rtf, out);
		assertTrue(out.size() > 0, "RTF output must not be empty");
	}

	@Test
	public void exportsToOdtProducesNonEmptyOutput() throws Exception {
		ByteArrayOutputStream out = new ByteArrayOutputStream();
		JasperReportUtil.runReport(filledPrint, ReportFormat.odt, out);
		assertTrue(out.size() > 0, "ODT output must not be empty");
	}

	@Test
	public void exportsToOdsProducesNonEmptyOutput() throws Exception {
		ByteArrayOutputStream out = new ByteArrayOutputStream();
		JasperReportUtil.runReport(filledPrint, ReportFormat.ods, out);
		assertTrue(out.size() > 0, "ODS output must not be empty");
	}

	@Test
	public void exportsToDocxProducesNonEmptyOutput() throws Exception {
		ByteArrayOutputStream out = new ByteArrayOutputStream();
		JasperReportUtil.runReport(filledPrint, ReportFormat.docx, out);
		assertTrue(out.size() > 0, "DOCX output must not be empty");
	}

	@Test
	public void exportsToPptxProducesNonEmptyOutput() throws Exception {
		ByteArrayOutputStream out = new ByteArrayOutputStream();
		JasperReportUtil.runReport(filledPrint, ReportFormat.pptx, out);
		assertTrue(out.size() > 0, "PPTX output must not be empty");
	}

	@Test
	public void exportsToXmlProducesNonEmptyOutput() throws Exception {
		ByteArrayOutputStream out = new ByteArrayOutputStream();
		JasperReportUtil.runReport(filledPrint, ReportFormat.xml, out);
		assertTrue(out.size() > 0, "XML output must not be empty");
	}

	@Test
	public void exportsToTxtProducesNonEmptyOutput() throws Exception {
		ByteArrayOutputStream out = new ByteArrayOutputStream();
		JasperReportUtil.runReport(filledPrint, ReportFormat.txt, out);
		assertTrue(out.size() > 0, "TXT output must not be empty");
	}

	// -------------------------------------------------------------------------

	private static DesignSpecification buildMinimalSpec() {
		DesignSpecification spec = new DesignSpecification();
		spec.setModuleName(AllAttributesPersistent.MODULE_NAME);
		spec.setDocumentName(AllAttributesPersistent.DOCUMENT_NAME);
		spec.setDefinitionSource(DesignSpecification.DefinitionSource.document);
		spec.setReportType(DesignSpecification.ReportType.report);
		spec.setMode(DesignSpecification.Mode.bean);
		spec.setName("smoke");
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
		return spec;
	}
}
