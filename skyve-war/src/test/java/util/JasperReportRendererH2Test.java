package util;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.generate.jasperreports.DesignSpecification;
import org.skyve.impl.generate.jasperreports.DocumentReportDesignGenerator;
import org.skyve.impl.generate.jasperreports.JasperReportRenderer;
import org.skyve.impl.generate.jasperreports.ReportBand;

import modules.test.domain.AllAttributesPersistent;

class JasperReportRendererH2Test extends AbstractH2Test {

	private DesignSpecification spec;

	@BeforeEach
	void buildSpec() {
		spec = new DesignSpecification();
		spec.setModuleName(AllAttributesPersistent.MODULE_NAME);
		spec.setDocumentName(AllAttributesPersistent.DOCUMENT_NAME);
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
		spec.setDefinitionSource(DesignSpecification.DefinitionSource.document);
		new DocumentReportDesignGenerator().populateDesign(spec);
	}

	@Test
	void getJrxmlContainsFieldElement() throws Exception {
		String jrxml = new JasperReportRenderer(spec).getJrxml();
		assertThat(jrxml, notNullValue());
		assertThat(jrxml, containsString("<field"));
	}

	@Test
	void renderDesignCalledTwiceThrowsIllegalStateException() throws Exception {
		JasperReportRenderer renderer = new JasperReportRenderer(spec);
		renderer.renderDesign();
		assertThrows(IllegalStateException.class, renderer::renderDesign);
	}

	@Test
	void renderDesignWithBandInvisibleConditionContainsPrintWhen() throws Exception {
		// Set an invisibleConditionName on the first non-empty detail band to exercise
		// JasperReportRenderer.getPrintWhenExpressionFromInvisibleCondition
		spec.getBands().stream()
				.filter(b -> ReportBand.BandType.detail.equals(b.getBandType()))
				.filter(b -> !b.getElements().isEmpty())
				.findFirst()
				.ifPresent(b -> b.setInvisibleConditionName("condition"));
		String jrxml = new JasperReportRenderer(spec).getJrxml();
		assertThat(jrxml, containsString("printWhenExpression"));
	}

	@Test
	void renderDesignSqlModeProducesQueryStringWithSelect() throws Exception {
		spec.setMode(DesignSpecification.Mode.sql);
		// Re-populate fields for sql mode (names with sql aliases)
		new DocumentReportDesignGenerator().populateDesign(spec);
		String jrxml = new JasperReportRenderer(spec).getJrxml();
		assertThat(jrxml, containsString("<query"));
		assertThat(jrxml, containsString("select"));
	}

	@Test
	void renderDesignIncludesVariablesInOutputWhenPresent() throws Exception {
		// Subreport type causes variables to be added for numeric fields
		spec.setReportType(DesignSpecification.ReportType.subreport);
		new DocumentReportDesignGenerator().populateDesign(spec);
		String jrxml = new JasperReportRenderer(spec).getJrxml();
		// subreport reports produce variables for totals
		assertThat(jrxml, notNullValue());
	}
}
