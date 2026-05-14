package util;

import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.generate.jasperreports.DesignSpecification;
import org.skyve.impl.generate.jasperreports.QueryReportDesignGenerator;
import org.skyve.impl.generate.jasperreports.ReportDesignGeneratorFactory;

import modules.test.domain.MappedBase;

public class QueryReportDesignGeneratorH2Test extends AbstractH2Test {

	private DesignSpecification spec;

	@BeforeEach
	public void buildSpec() {
		spec = new DesignSpecification();
		spec.setModuleName(MappedBase.MODULE_NAME);
		spec.setDocumentName(MappedBase.DOCUMENT_NAME);
		spec.setQueryName("qMB");
		spec.setDefinitionSource(DesignSpecification.DefinitionSource.query);
		spec.setReportType(DesignSpecification.ReportType.report);
		spec.setMode(DesignSpecification.Mode.bean);
		spec.setName("querytest");
		spec.setWidth(Integer.valueOf(842));
		spec.setHeight(Integer.valueOf(595));
		spec.setLeftMargin(Integer.valueOf(20));
		spec.setRightMargin(Integer.valueOf(20));
		spec.setTopMargin(Integer.valueOf(20));
		spec.setBottomMargin(Integer.valueOf(20));
		spec.setColumnWidth(Integer.valueOf(802));
		spec.setDefaultElementHeight(Integer.valueOf(20));
		spec.setDefaultFontSize(Integer.valueOf(10));
	}

	@Test
	public void populateDesignAddsStandardParameters() {
		QueryReportDesignGenerator gen = new QueryReportDesignGenerator();
		DesignSpecification result = gen.populateDesign(spec);

		boolean hasSUBREPORT_DIR = result.getParameters().stream().anyMatch(p -> "SUBREPORT_DIR".equals(p.getName()));
		boolean hasRESOURCE_DIR = result.getParameters().stream().anyMatch(p -> "RESOURCE_DIR".equals(p.getName()));
		boolean hasID = result.getParameters().stream().anyMatch(p -> "ID".equals(p.getName()));

		assertTrue(hasSUBREPORT_DIR, "missing SUBREPORT_DIR parameter");
		assertTrue(hasRESOURCE_DIR, "missing RESOURCE_DIR parameter");
		assertTrue(hasID, "missing ID parameter");
	}

	@Test
	public void populateDesignAddsFieldsForQueryBindings() {
		QueryReportDesignGenerator gen = new QueryReportDesignGenerator();
		DesignSpecification result = gen.populateDesign(spec);

		// qMB has a column with binding="text"; MappedBase has a text attribute
		boolean hasTextField = result.getFields().stream().anyMatch(f -> "text".equals(f.getName()));
		assertTrue(hasTextField, "expected field named 'text' from qMB query binding");
	}

	@Test
	public void populateDesignAddsBands() {
		QueryReportDesignGenerator gen = new QueryReportDesignGenerator();
		DesignSpecification result = gen.populateDesign(spec);

		assertFalse(result.getBands().isEmpty(), "expected at least one band");
		boolean hasDetail = result.getBands().stream()
				.anyMatch(b -> b.getBandType() != null && b.getBandType().toString().equalsIgnoreCase("detail"));
		assertTrue(hasDetail, "missing detail band");
	}

	@Test
	public void factoryReturnsQueryGeneratorForQuerySource() {
		assertThat(
				ReportDesignGeneratorFactory.getGeneratorForDesign(spec),
				instanceOf(QueryReportDesignGenerator.class));
	}

	@Test
	public void generatedDesignIsNotNull() {
		QueryReportDesignGenerator gen = new QueryReportDesignGenerator();
		DesignSpecification result = gen.populateDesign(spec);
		assertThat(result, notNullValue());
	}
}
