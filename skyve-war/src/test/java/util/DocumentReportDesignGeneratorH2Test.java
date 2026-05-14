package util;

import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.generate.jasperreports.DesignSpecification;
import org.skyve.impl.generate.jasperreports.DocumentReportDesignGenerator;
import org.skyve.impl.generate.jasperreports.ReportDesignGeneratorFactory;

import modules.test.domain.AllAttributesPersistent;

public class DocumentReportDesignGeneratorH2Test extends AbstractH2Test {

	private DesignSpecification spec;

	@BeforeEach
	public void buildSpec() {
		spec = new DesignSpecification();
		spec.setModuleName(AllAttributesPersistent.MODULE_NAME);
		spec.setDocumentName(AllAttributesPersistent.DOCUMENT_NAME);
		spec.setDefinitionSource(DesignSpecification.DefinitionSource.document);
		spec.setReportType(DesignSpecification.ReportType.report);
		spec.setMode(DesignSpecification.Mode.bean);
		spec.setName("test");
		spec.setWidth(Integer.valueOf(842));
		spec.setHeight(Integer.valueOf(595));
		spec.setLeftMargin(Integer.valueOf(20));
		spec.setRightMargin(Integer.valueOf(20));
		spec.setTopMargin(Integer.valueOf(20));
		spec.setBottomMargin(Integer.valueOf(20));
		spec.setDefaultElementHeight(Integer.valueOf(20));
		spec.setDefaultFontSize(Integer.valueOf(10));
	}

	@Test
	public void populateDesignAddsStandardParameters() {
		DocumentReportDesignGenerator gen = new DocumentReportDesignGenerator();
		DesignSpecification result = gen.populateDesign(spec);

		boolean hasSUBREPORT_DIR = result.getParameters().stream().anyMatch(p -> "SUBREPORT_DIR".equals(p.getName()));
		boolean hasRESOURCE_DIR = result.getParameters().stream().anyMatch(p -> "RESOURCE_DIR".equals(p.getName()));
		boolean hasID = result.getParameters().stream().anyMatch(p -> "ID".equals(p.getName()));

		assertTrue(hasSUBREPORT_DIR, "missing SUBREPORT_DIR parameter");
		assertTrue(hasRESOURCE_DIR, "missing RESOURCE_DIR parameter");
		assertTrue(hasID, "missing ID parameter");
	}

	@Test
	public void populateDesignAddsFieldsForDocument() {
		DocumentReportDesignGenerator gen = new DocumentReportDesignGenerator();
		DesignSpecification result = gen.populateDesign(spec);

		assertFalse(result.getFields().isEmpty(), "expected fields for AllAttributesPersistent");
	}

	@Test
	public void populateDesignAddsBands() {
		DocumentReportDesignGenerator gen = new DocumentReportDesignGenerator();
		DesignSpecification result = gen.populateDesign(spec);

		boolean hasColumnHeader = result.getBands().stream()
				.anyMatch(b -> b.getBandType() != null && b.getBandType().toString().equalsIgnoreCase("columnHeader"));
		boolean hasDetail = result.getBands().stream()
				.anyMatch(b -> b.getBandType() != null && b.getBandType().toString().equalsIgnoreCase("detail"));

		assertTrue(hasColumnHeader, "missing columnHeader band");
		assertTrue(hasDetail, "missing detail band");
	}

	@Test
	public void factoryReturnsDocumentGeneratorForDocumentSource() {
		assertThat(
				ReportDesignGeneratorFactory.getGeneratorForDesign(spec),
				instanceOf(DocumentReportDesignGenerator.class));
	}

	@Test
	public void generatedDesignIsNotNull() {
		DocumentReportDesignGenerator gen = new DocumentReportDesignGenerator();
		DesignSpecification result = gen.populateDesign(spec);
		assertThat(result, notNullValue());
	}

	// --- ReportDesignGenerator.addBands branch: includePageNumbers ---

	@Test
	public void populateDesignWithIncludePageNumbersAddsPageFooterBand() {
		spec.setIncludePageNumbers(Boolean.TRUE);
		DesignSpecification result = new DocumentReportDesignGenerator().populateDesign(spec);

		assertTrue(result.getBands().stream().anyMatch(b -> b.getBandType() != null
				&& "pageFooter".equals(b.getBandType().toString())),
				"expected a pageFooter band when includePageNumbers is true");
	}

	@Test
	public void populateDesignWithPageNumbersAddsPageXOfYElements() {
		spec.setIncludePageNumbers(Boolean.TRUE);
		DesignSpecification result = new DocumentReportDesignGenerator().populateDesign(spec);

		long pageFooterElements = result.getBands().stream()
				.filter(b -> "pageFooter".equals(b.getBandType().toString()))
				.flatMap(b -> b.getElements().stream())
				.count();
		assertTrue(pageFooterElements >= 2, "expected at least two elements (pageX + pageOfY) in pageFooter");
	}

	// --- ReportDesignGenerator.addBands branch: sectionBorder flags -> noData border ---

	@Test
	public void populateDesignWithSectionBorderTopAddsNoDataBorderElement() {
		spec.setSectionBorderTop(Boolean.TRUE);
		DesignSpecification result = new DocumentReportDesignGenerator().populateDesign(spec);

		boolean hasBorderInNoData = result.getBands().stream()
				.filter(b -> b.getBandType() != null && "noData".equals(b.getBandType().toString()))
				.flatMap(b -> b.getElements().stream())
				.anyMatch(e -> e.getElementType() != null && "border".equals(e.getElementType().toString()));
		assertTrue(hasBorderInNoData, "expected a border element in the noData band when section border is set");
	}

	// --- ReportDesignGenerator.addSubreports: collection fields -> subreport specs ---

	@Test
	public void populateDesignForDocumentWithCollectionFieldsAddsSubreports() {
		// AllAttributesPersistent has both aggregated and composed collections
		DesignSpecification result = new DocumentReportDesignGenerator().populateDesign(spec);

		assertFalse(result.getSubReports().isEmpty(),
				"expected at least one subreport for AllAttributesPersistent collections");
	}

	// --- ReportDesignGenerator sql mode: addFields adds fields for all scalar attributes ---

	@Test
	public void populateDesignInSqlModeAddsFieldsWithSqlNames() {
		spec.setMode(DesignSpecification.Mode.sql);
		DesignSpecification result = new DocumentReportDesignGenerator().populateDesign(spec);

		// In sql mode, scalar fields should have a nameSql set
		long fieldsWithSqlName = result.getFields().stream()
				.filter(f -> !Boolean.TRUE.equals(f.getCollection()))
				.filter(f -> f.getNameSql() != null)
				.count();
		assertTrue(fieldsWithSqlName > 0, "expected fields with sql name aliases in sql mode");
	}
}
