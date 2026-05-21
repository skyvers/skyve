package org.skyve.impl.generate.jasperreports;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.lang.reflect.Field;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.util.UtilImpl;

@SuppressWarnings("static-method")
class ReportDesignGeneratorTest {

	@BeforeEach
	void setUp() throws Exception {
		Field f = UtilImpl.class.getDeclaredField("absoluteBasePath");
		f.setAccessible(true);
		f.set(null, "/test/base/");
	}

	@AfterEach
	void tearDown() throws Exception {
		Field f = UtilImpl.class.getDeclaredField("absoluteBasePath");
		f.setAccessible(true);
		f.set(null, null);
	}

	/**
	 * addParameters is a static protected method on ReportDesignGenerator.
	 * We test it via the concrete subclass QueryReportDesignGenerator.
	 */
	@Test
	void addParametersAddsThreeStandardParameters() {
		DesignSpecification design = new DesignSpecification();
		design.setModuleName("myModule");
		design.setDocumentName("myDocument");

		ReportDesignGenerator.addParameters(design);

		assertEquals(3, design.getParameters().size());
		assertThat(design.getParameters().get(0).getName(), is("SUBREPORT_DIR"));
		assertThat(design.getParameters().get(1).getName(), is("RESOURCE_DIR"));
		assertThat(design.getParameters().get(2).getName(), is("ID"));
	}

	@Test
	void addParametersSubreportDirHasDefaultValueExpression() {
		DesignSpecification design = new DesignSpecification();
		design.setModuleName("testMod");
		design.setDocumentName("testDoc");

		ReportDesignGenerator.addParameters(design);

		assertThat(design.getParameters().get(0).getDefaultValueExpression(), notNullValue());
	}

	@Test
	void addParametersResourceDirHasEmptyDefaultValueExpression() {
		DesignSpecification design = new DesignSpecification();
		design.setModuleName("mod");
		design.setDocumentName("doc");

		ReportDesignGenerator.addParameters(design);

		assertThat(design.getParameters().get(1).getDefaultValueExpression(), is(""));
	}

	@Test
	void addParametersIdParameterHasNoDefaultValueExpression() {
		DesignSpecification design = new DesignSpecification();
		design.setModuleName("mod");
		design.setDocumentName("doc");

		ReportDesignGenerator.addParameters(design);

		assertThat(design.getParameters().get(2).getDefaultValueExpression(), is((String) null));
	}

	@Test
	void addVariablesWithNonSubreportTypeDoesNotAddVariables() {
		DesignSpecification design = new DesignSpecification();
		design.setReportType(DesignSpecification.ReportType.report);

		ReportDesignGenerator.addVariables(design);

		assertEquals(0, design.getVariables().size());
	}

	@Test
	void addVariablesWithSubreportTypeAddsVariablesForIncludedTotalFields() {
		DesignSpecification design = new DesignSpecification();
		design.setReportType(DesignSpecification.ReportType.subreport);

		ReportField field = new ReportField();
		field.setParent(design);
		field.setName("amount");
		field.setTypeClass("java.math.BigDecimal");
		field.setIncludeTotal(Boolean.TRUE);
		design.getFields().add(field);

		ReportDesignGenerator.addVariables(design);

		assertEquals(1, design.getVariables().size());
		assertThat(design.getVariables().get(0).getName(), is("amount"));
	}

	@Test
	void addVariablesSkipsImplicitFields() {
		DesignSpecification design = new DesignSpecification();
		design.setReportType(DesignSpecification.ReportType.subreport);

		ReportField field = new ReportField();
		field.setParent(design);
		field.setName("bizKey");
		field.setTypeClass("java.lang.String");
		field.setImplicit(Boolean.TRUE);
		field.setIncludeTotal(Boolean.TRUE);
		design.getFields().add(field);

		ReportDesignGenerator.addVariables(design);

		assertEquals(0, design.getVariables().size());
	}

	@Test
	void addVariablesSkipsCollectionTypeFields() {
		DesignSpecification design = new DesignSpecification();
		design.setReportType(DesignSpecification.ReportType.subreport);

		ReportField field = new ReportField();
		field.setParent(design);
		field.setName("myCollection");
		field.setSkyveType("collection");
		field.setIncludeTotal(Boolean.TRUE);
		design.getFields().add(field);

		ReportDesignGenerator.addVariables(design);

		assertEquals(0, design.getVariables().size());
	}

	@Test
	void addVariablesSkipsFieldsWithIncludeTotalFalse() {
		DesignSpecification design = new DesignSpecification();
		design.setReportType(DesignSpecification.ReportType.subreport);

		ReportField field = new ReportField();
		field.setParent(design);
		field.setName("someField");
		field.setTypeClass("java.lang.String");
		field.setIncludeTotal(Boolean.FALSE);
		design.getFields().add(field);

		ReportDesignGenerator.addVariables(design);

		assertEquals(0, design.getVariables().size());
	}
}
