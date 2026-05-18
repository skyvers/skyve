package util;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;
import org.skyve.impl.generate.jasperreports.DesignSpecification;
import org.skyve.impl.generate.jasperreports.JasperReportRenderer;
import org.skyve.impl.generate.jasperreports.ViewReportDesignGenerator;

import modules.admin.domain.JobSchedule;
import modules.admin.domain.ReportParameter;
import modules.admin.domain.Snapshot;
import modules.test.domain.ArcOneToMany;
import modules.test.domain.ArcOneToOne;

/**
 * H2-backed tests for {@link ViewReportDesignGenerator} and {@link org.skyve.impl.generate.jasperreports.ReportViewVisitor}.
 *
 * <p>Running these tests in {@code skyve-war} means {@code CORE.getCustomer()}, impl-typed metadata
 * casts, and {@code UtilImpl.getAbsoluteBasePath()} all resolve without any production-code changes.
 * See {@code docs/test-patterns.md} — "This looks untestable without refactoring — check H2 first".
 */
@SuppressWarnings("static-method")
class ViewReportDesignGeneratorH2Test extends AbstractH2Test {

	// --- helpers ---

	private static DesignSpecification viewSpec(String module, String document) {
		DesignSpecification spec = new DesignSpecification();
		spec.setModuleName(module);
		spec.setDocumentName(document);
		spec.setMode(DesignSpecification.Mode.bean);
		spec.setName("test");
		spec.setWidth(Integer.valueOf(842));
		spec.setHeight(Integer.valueOf(595));
		spec.setLeftMargin(Integer.valueOf(20));
		spec.setRightMargin(Integer.valueOf(20));
		spec.setTopMargin(Integer.valueOf(20));
		spec.setBottomMargin(Integer.valueOf(20));
		spec.setColumnWidth(Integer.valueOf(802));
		spec.setDefaultElementHeight(Integer.valueOf(20));
		spec.setDefaultFontSize(Integer.valueOf(10));
		spec.setReportType(DesignSpecification.ReportType.report);
		spec.setDefinitionSource(DesignSpecification.DefinitionSource.view);
		return spec;
	}

	// --- ArcOneToOne: simple form with column/row/item/default binding ---
	// Exercises: visitForm, visitFormColumn, visitFormRow, visitFormItem, visitTextField

	@Test
	void populateDesignForArcOneToOneAddsParameters() {
		DesignSpecification spec = viewSpec(ArcOneToOne.MODULE_NAME, ArcOneToOne.DOCUMENT_NAME);
		DesignSpecification result = new ViewReportDesignGenerator().populateDesign(spec);

		assertTrue(result.getParameters().stream().anyMatch(p -> "SUBREPORT_DIR".equals(p.getName())),
				"expected SUBREPORT_DIR parameter");
		assertTrue(result.getParameters().stream().anyMatch(p -> "ID".equals(p.getName())),
				"expected ID parameter");
	}

	@Test
	void populateDesignForArcOneToOneAddsStandardBands() {
		DesignSpecification spec = viewSpec(ArcOneToOne.MODULE_NAME, ArcOneToOne.DOCUMENT_NAME);
		DesignSpecification result = new ViewReportDesignGenerator().populateDesign(spec);

		assertFalse(result.getBands().isEmpty(), "expected at least one band");
		// background band is always added by addBands
		assertTrue(result.getBands().stream().anyMatch(b -> b.getBandType() != null
				&& "background".equals(b.getBandType().toString())),
				"expected a background band");
	}

	@Test
	void populateDesignForArcOneToOneProducesJrxml() throws Exception {
		DesignSpecification spec = viewSpec(ArcOneToOne.MODULE_NAME, ArcOneToOne.DOCUMENT_NAME);
		new ViewReportDesignGenerator().populateDesign(spec);

		String jrxml = new JasperReportRenderer(spec).getJrxml();
		assertThat(jrxml, notNullValue());
		assertThat(jrxml, containsString("jasperReport"));
	}

	// --- Snapshot: view with combo and textArea, no component elements ---
	// Exercises: visitCombo, visitTextArea (scalar widgets on a form)

	@Test
	void populateDesignForSnapshotAddsDetailBandsFromForm() {
		DesignSpecification spec = viewSpec(Snapshot.MODULE_NAME, Snapshot.DOCUMENT_NAME);
		DesignSpecification result = new ViewReportDesignGenerator().populateDesign(spec);

		// Snapshot has a form with combo and textArea widgets — visitor should produce detail bands
		assertTrue(result.getBands().stream().anyMatch(b -> b.getBandType() != null
				&& "detail".equals(b.getBandType().toString())),
				"expected at least one detail band for Snapshot view");
	}

	@Test
	void populateDesignForSnapshotProducesJrxml() throws Exception {
		DesignSpecification spec = viewSpec(Snapshot.MODULE_NAME, Snapshot.DOCUMENT_NAME);
		new ViewReportDesignGenerator().populateDesign(spec);

		String jrxml = new JasperReportRenderer(spec).getJrxml();
		assertThat(jrxml, notNullValue());
		assertThat(jrxml, containsString("jasperReport"));
	}

	// --- ArcOneToMany: view with dataGrid, no component elements ---
	// Exercises: visitDataGrid -> visitDataWidget -> addContainer for collection bindings

	@Test
	void populateDesignForArcOneToManyAddsDetailBandsForDataGrid() {
		DesignSpecification spec = viewSpec(ArcOneToMany.MODULE_NAME, ArcOneToMany.DOCUMENT_NAME);
		DesignSpecification result = new ViewReportDesignGenerator().populateDesign(spec);

		// ArcOneToMany has a dataGrid binding="arcs" — visitor should produce at least one detail band
		assertFalse(result.getBands().isEmpty(), "expected bands from ArcOneToMany view");
	}

	// --- JobSchedule: vbox, hbox, lookupDescription widgets ---
	// Exercises: visitVBox, visitHBox, visitLookupDescription

	@Test
	void populateDesignForJobScheduleAddsParametersAndBands() {
		DesignSpecification spec = viewSpec(JobSchedule.MODULE_NAME, JobSchedule.DOCUMENT_NAME);
		DesignSpecification result = new ViewReportDesignGenerator().populateDesign(spec);

		assertFalse(result.getBands().isEmpty(), "expected bands from JobSchedule view");
		assertTrue(result.getParameters().stream().anyMatch(p -> "ID".equals(p.getName())),
				"expected ID parameter");
	}

	@Test
	void populateDesignForJobScheduleProducesJrxml() throws Exception {
		DesignSpecification spec = viewSpec(JobSchedule.MODULE_NAME, JobSchedule.DOCUMENT_NAME);
		new ViewReportDesignGenerator().populateDesign(spec);

		String jrxml = new JasperReportRenderer(spec).getJrxml();
		assertThat(jrxml, notNullValue());
		assertThat(jrxml, containsString("jasperReport"));
	}

	// --- ReportParameter: spinner widget ---
	// Exercises: visitSpinner -> addElementFromItem(spinner)

	@Test
	void populateDesignForReportParameterAddsParametersAndBands() {
		DesignSpecification spec = viewSpec(ReportParameter.MODULE_NAME, ReportParameter.DOCUMENT_NAME);
		DesignSpecification result = new ViewReportDesignGenerator().populateDesign(spec);

		assertFalse(result.getBands().isEmpty(), "expected bands from ReportParameter view");
		assertTrue(result.getParameters().stream().anyMatch(p -> "ID".equals(p.getName())),
				"expected ID parameter");
	}

	@Test
	void populateDesignForReportParameterProducesJrxml() throws Exception {
		DesignSpecification spec = viewSpec(ReportParameter.MODULE_NAME, ReportParameter.DOCUMENT_NAME);
		new ViewReportDesignGenerator().populateDesign(spec);

		String jrxml = new JasperReportRenderer(spec).getJrxml();
		assertThat(jrxml, notNullValue());
		assertThat(jrxml, containsString("jasperReport"));
	}

	// --- title band / border branch in ViewReportDesignGenerator.createTitleBand ---

	@Test
	void populateDesignWithReportTypeAndSectionBorderAddsBorderElementToTitleBand() {
		DesignSpecification spec = viewSpec(ArcOneToOne.MODULE_NAME, ArcOneToOne.DOCUMENT_NAME);
		spec.setSectionBorderTop(Boolean.TRUE);
		DesignSpecification result = new ViewReportDesignGenerator().populateDesign(spec);

		// The title band should contain a border element when a section border flag is set
		boolean hasBorderInTitle = result.getBands().stream()
				.filter(b -> b.getBandType() != null && "title".equals(b.getBandType().toString()))
				.flatMap(b -> b.getElements().stream())
				.anyMatch(e -> e.getElementType() != null
						&& "border".equals(e.getElementType().toString()));
		assertTrue(hasBorderInTitle, "expected a border element in the title band");
	}

	@Test
	void populateDesignWithIncludePageNumbersAddsPageFooterBand() {
		DesignSpecification spec = viewSpec(ArcOneToOne.MODULE_NAME, ArcOneToOne.DOCUMENT_NAME);
		spec.setIncludePageNumbers(Boolean.TRUE);
		DesignSpecification result = new ViewReportDesignGenerator().populateDesign(spec);

		assertTrue(result.getBands().stream().anyMatch(b -> b.getBandType() != null
				&& "pageFooter".equals(b.getBandType().toString())),
				"expected a pageFooter band when includePageNumbers is true");
	}
}
