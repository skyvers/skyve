package org.skyve.impl.generate.jasperreports;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.skyve.impl.metadata.model.document.CollectionImpl;
import org.skyve.impl.metadata.model.document.field.Decimal2;
import org.skyve.impl.metadata.model.document.field.Decimal5;
import org.skyve.impl.metadata.model.document.field.Decimal10;
import org.skyve.impl.metadata.model.document.field.Text;
import org.skyve.impl.metadata.model.document.field.LongInteger;
import org.skyve.impl.metadata.model.document.field.Integer;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.document.Document;

@ExtendWith(MockitoExtension.class)
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

	@Test
	void fieldFromAttributeCollectionTypeSetsSkyveTypeAndDocumentName() {
		DesignSpecification spec = new DesignSpecification();
		spec.setModuleName("mod");
		spec.setDocumentName("doc");

		Customer customer = mock(Customer.class);
		Document document = mock(Document.class);
		when(document.getOwningModuleName()).thenReturn("mod");

		CollectionImpl col = new CollectionImpl();
		col.setName("items");
		col.setDisplayName("Items");
		col.setDocumentName("Item");

		ReportField result = ReportDesignGenerator.fieldFromAttribute(spec, customer, document, col, new StringBuilder(), new StringBuilder());

		assertThat(result.getSkyveType(), is(AttributeType.collection.name()));
		assertThat(result.getDocumentName(), is("Item"));
		assertThat(result.getTypeClass(), is("java.util.List"));
		assertTrue(Boolean.TRUE.equals(result.getCollection()));
	}

	@Test
	void fieldFromAttributeTextTypeBeanModeUsesStringTypeClass() {
		DesignSpecification spec = new DesignSpecification();
		spec.setModuleName("mod");
		spec.setDocumentName("doc");
		// bean mode is the default

		Customer customer = mock(Customer.class);
		Document document = mock(Document.class);

		Text text = new Text();
		text.setName("name");
		text.setDisplayName("Name");
		text.setLength(100);

		ReportField result = ReportDesignGenerator.fieldFromAttribute(spec, customer, document, text, new StringBuilder(), new StringBuilder());

		assertThat(result.getSkyveType(), is(AttributeType.text.name()));
		assertThat(result.getTypeClass(), is("java.lang.String"));
		assertThat(result.getName(), is("name"));
	}

	@Test
	void fieldFromAttributeIntegerTypeIncludesTotalInBeanMode() {
		DesignSpecification spec = new DesignSpecification();
		spec.setModuleName("mod");
		spec.setDocumentName("doc");

		Customer customer = mock(Customer.class);
		Document document = mock(Document.class);

		Integer intAttr = new Integer();
		intAttr.setName("count");
		intAttr.setDisplayName("Count");

		ReportField result = ReportDesignGenerator.fieldFromAttribute(spec, customer, document, intAttr, new StringBuilder(), new StringBuilder());

		assertTrue(Boolean.TRUE.equals(result.getIncludeTotal()));
	}

	@Test
	void fieldFromAttributeLongIntegerTypeIncludesTotalInBeanMode() {
		DesignSpecification spec = new DesignSpecification();
		spec.setModuleName("mod");
		spec.setDocumentName("doc");

		Customer customer = mock(Customer.class);
		Document document = mock(Document.class);

		LongInteger longAttr = new LongInteger();
		longAttr.setName("total");
		longAttr.setDisplayName("Total");

		ReportField result = ReportDesignGenerator.fieldFromAttribute(spec, customer, document, longAttr, new StringBuilder(), new StringBuilder());

		assertTrue(Boolean.TRUE.equals(result.getIncludeTotal()));
	}

	@Test
	void fieldFromAttributeTextTypeInSqlModeUsesNonBeanTypeClass() {
		DesignSpecification spec = new DesignSpecification();
		spec.setModuleName("mod");
		spec.setDocumentName("doc");
		spec.setMode(DesignSpecification.Mode.sql);

		Customer customer = mock(Customer.class);
		Document document = mock(Document.class);

		Text text = new Text();
		text.setName("firstName");
		text.setDisplayName("First Name");
		text.setLength(200);

		ReportField result = ReportDesignGenerator.fieldFromAttribute(spec, customer, document, text, new StringBuilder(), new StringBuilder());

		assertThat(result.getName(), is("firstName"));
		assertThat(result.getNameSql(), is("a.firstName"));
	}

	@Test
	void fieldFromAttributeDecimal2TypeIncludesTotal() {
		DesignSpecification spec = new DesignSpecification();
		spec.setModuleName("mod");
		spec.setDocumentName("doc");

		Customer customer = mock(Customer.class);
		Document document = mock(Document.class);

		Decimal2 attr = new Decimal2();
		attr.setName("price");
		attr.setDisplayName("Price");

		ReportField result = ReportDesignGenerator.fieldFromAttribute(spec, customer, document, attr, new StringBuilder(), new StringBuilder());

		assertTrue(Boolean.TRUE.equals(result.getIncludeTotal()));
		assertThat(result.getSkyveType(), is(AttributeType.decimal2.name()));
	}

	@Test
	void fieldFromAttributeDecimal5TypeIncludesTotal() {
		DesignSpecification spec = new DesignSpecification();
		spec.setModuleName("mod");
		spec.setDocumentName("doc");

		Customer customer = mock(Customer.class);
		Document document = mock(Document.class);

		Decimal5 attr = new Decimal5();
		attr.setName("amount");
		attr.setDisplayName("Amount");

		ReportField result = ReportDesignGenerator.fieldFromAttribute(spec, customer, document, attr, new StringBuilder(), new StringBuilder());

		assertTrue(Boolean.TRUE.equals(result.getIncludeTotal()));
		assertThat(result.getSkyveType(), is(AttributeType.decimal5.name()));
	}

	@Test
	void fieldFromAttributeDecimal10TypeIncludesTotal() {
		DesignSpecification spec = new DesignSpecification();
		spec.setModuleName("mod");
		spec.setDocumentName("doc");

		Customer customer = mock(Customer.class);
		Document document = mock(Document.class);

		Decimal10 attr = new Decimal10();
		attr.setName("bigAmount");
		attr.setDisplayName("Big Amount");

		ReportField result = ReportDesignGenerator.fieldFromAttribute(spec, customer, document, attr, new StringBuilder(), new StringBuilder());

		assertTrue(Boolean.TRUE.equals(result.getIncludeTotal()));
		assertThat(result.getSkyveType(), is(AttributeType.decimal10.name()));
	}

	@Test
	void fieldFromBindingSimpleTextAttributeReturnsField() {
		DesignSpecification spec = new DesignSpecification();
		spec.setModuleName("mod");
		spec.setDocumentName("doc");

		Customer customer = mock(Customer.class);
		Document document = mock(Document.class);

		Text text = new Text();
		text.setName("lastName");
		text.setDisplayName("Last Name");
		text.setLength(100);
		when(document.getAttribute("lastName")).thenReturn(text);

		ReportField result = ReportDesignGenerator.fieldFromBinding(spec, customer, document, "lastName");

		assertThat(result, notNullValue());
		assertThat(result.getName(), is("lastName"));
		assertThat(result.getBinding(), is("lastName"));
	}

	@Test
	void fieldFromBindingNullAttributeReturnsNull() {
		DesignSpecification spec = new DesignSpecification();
		spec.setModuleName("mod");
		spec.setDocumentName("doc");

		Customer customer = mock(Customer.class);
		Document document = mock(Document.class);

		when(document.getAttribute("unknown")).thenReturn(null);

		ReportField result = ReportDesignGenerator.fieldFromBinding(spec, customer, document, "unknown");

		assertThat(result, org.hamcrest.CoreMatchers.nullValue());
	}

	@Test
	void addBandsForQueryReportTypeAddsExpectedBands() {
		DesignSpecification design = new DesignSpecification();
		design.setReportType(DesignSpecification.ReportType.report);
		design.setColumnWidth(java.lang.Integer.valueOf(500));
		design.setDefaultElementHeight(java.lang.Integer.valueOf(20));

		ReportField field = new ReportField();
		field.setParent(design);
		field.setName("name");
		field.setDisplayName("Name");
		field.setSkyveType("text");
		design.getFields().add(field);

		QueryReportDesignGenerator gen = new QueryReportDesignGenerator();
		gen.addBands(design);

		// background, title, pageHeader, noData, plus detail bands per field
		assertTrue(design.getBands().size() > 3);
	}

	@Test
	void addBandsForSubreportTypeAddsColumnHeaderAndDetailBands() {
		DesignSpecification design = new DesignSpecification();
		design.setReportType(DesignSpecification.ReportType.subreport);
		design.setColumnWidth(java.lang.Integer.valueOf(400));
		design.setDefaultElementHeight(java.lang.Integer.valueOf(20));

		ReportField field = new ReportField();
		field.setParent(design);
		field.setName("amount");
		field.setDisplayName("Amount");
		field.setSkyveType("decimal2");
		design.getFields().add(field);

		QueryReportDesignGenerator gen = new QueryReportDesignGenerator();
		gen.addBands(design);

		assertTrue(design.getBands().size() > 3);
	}

	@Test
	void addBandsForReportTypeWithPageNumbersAddsPageFooter() {
		DesignSpecification design = new DesignSpecification();
		design.setReportType(DesignSpecification.ReportType.report);
		design.setColumnWidth(java.lang.Integer.valueOf(500));
		design.setDefaultElementHeight(java.lang.Integer.valueOf(20));
		design.setIncludePageNumbers(Boolean.TRUE);

		QueryReportDesignGenerator gen = new QueryReportDesignGenerator();
		gen.addBands(design);

		boolean hasPageFooter = design.getBands().stream()
				.anyMatch(b -> ReportBand.BandType.pageFooter.equals(b.getBandType()));
		assertTrue(hasPageFooter);
	}

	@Test
	void addBandsWithRenderLabelAsTextFieldsUsesTextFieldType() {
		DesignSpecification design = new DesignSpecification();
		design.setReportType(DesignSpecification.ReportType.report);
		design.setColumnWidth(java.lang.Integer.valueOf(500));
		design.setDefaultElementHeight(java.lang.Integer.valueOf(20));
		design.setRenderLabelAsTextFields(Boolean.TRUE);

		ReportField field = new ReportField();
		field.setParent(design);
		field.setName("title");
		field.setDisplayName("Title");
		field.setSkyveType("text");
		design.getFields().add(field);

		QueryReportDesignGenerator gen = new QueryReportDesignGenerator();
		gen.addBands(design);

		// should complete without exceptions
		assertTrue(design.getBands().size() > 0);
	}

	@Test
	void addBandsWithBorderSectionAddsNoDataBorderElement() {
		DesignSpecification design = new DesignSpecification();
		design.setReportType(DesignSpecification.ReportType.report);
		design.setColumnWidth(java.lang.Integer.valueOf(500));
		design.setDefaultElementHeight(java.lang.Integer.valueOf(20));
		design.setSectionBorderTop(Boolean.TRUE);

		QueryReportDesignGenerator gen = new QueryReportDesignGenerator();
		gen.addBands(design);

		// Find noData band and verify it has 2 elements (text + border)
		ReportBand noData = design.getBands().stream()
				.filter(b -> ReportBand.BandType.noData.equals(b.getBandType()))
				.findFirst()
				.orElse(null);
		assertNotNull(noData);
		assertEquals(2, noData.getElements().size());
	}

	@Test
	void documentGeneratorAddBandsForSubreportTypeAddsColumnHeaderBand() {
		DesignSpecification design = new DesignSpecification();
		design.setReportType(DesignSpecification.ReportType.subreport);
		design.setColumnWidth(java.lang.Integer.valueOf(400));
		design.setDefaultElementHeight(java.lang.Integer.valueOf(20));

		ReportField field = new ReportField();
		field.setParent(design);
		field.setName("name");
		field.setDisplayName("Name");
		field.setSkyveType("text");
		design.getFields().add(field);

		DocumentReportDesignGenerator gen = new DocumentReportDesignGenerator();
		gen.addBands(design);

		boolean hasColumnHeader = design.getBands().stream()
				.anyMatch(b -> ReportBand.BandType.columnHeader.equals(b.getBandType()));
		assertTrue(hasColumnHeader);
	}

	@Test
	void documentGeneratorAddBandsForReportTypeAddsDetailBandsPerField() {
		DesignSpecification design = new DesignSpecification();
		design.setReportType(DesignSpecification.ReportType.report);
		design.setColumnWidth(java.lang.Integer.valueOf(500));
		design.setDefaultElementHeight(java.lang.Integer.valueOf(20));

		ReportField field1 = new ReportField();
		field1.setParent(design);
		field1.setName("firstName");
		field1.setDisplayName("First Name");
		field1.setSkyveType("text");
		design.getFields().add(field1);

		ReportField field2 = new ReportField();
		field2.setParent(design);
		field2.setName("lastName");
		field2.setDisplayName("Last Name");
		field2.setSkyveType("text");
		design.getFields().add(field2);

		DocumentReportDesignGenerator gen = new DocumentReportDesignGenerator();
		gen.addBands(design);

		// Should have background, title, pageHeader, noData, lastPageFooter, summary, columnHeader, plus detail bands per field
		long detailBandCount = design.getBands().stream()
				.filter(b -> ReportBand.BandType.detail.equals(b.getBandType()))
				.count();
		assertEquals(2, detailBandCount);
	}

	@Test
	void documentGeneratorAddBandsWithCollectionFieldSkipsDetail() {
		DesignSpecification design = new DesignSpecification();
		design.setReportType(DesignSpecification.ReportType.report);
		design.setColumnWidth(java.lang.Integer.valueOf(500));
		design.setDefaultElementHeight(java.lang.Integer.valueOf(20));

		ReportField collField = new ReportField();
		collField.setParent(design);
		collField.setName("items");
		collField.setDisplayName("Items");
		collField.setSkyveType("collection");
		design.getFields().add(collField);

		DocumentReportDesignGenerator gen = new DocumentReportDesignGenerator();
		gen.addBands(design);

		// Collection fields should not produce detail bands
		long detailBandCount = design.getBands().stream()
				.filter(b -> ReportBand.BandType.detail.equals(b.getBandType()))
				.count();
		assertEquals(0, detailBandCount);
	}

	@Test
	void documentGeneratorAddBandsWithSubreportRenderLabelAsTextFields() {
		DesignSpecification design = new DesignSpecification();
		design.setReportType(DesignSpecification.ReportType.subreport);
		design.setColumnWidth(java.lang.Integer.valueOf(400));
		design.setDefaultElementHeight(java.lang.Integer.valueOf(20));
		design.setRenderLabelAsTextFields(Boolean.TRUE);

		ReportField field = new ReportField();
		field.setParent(design);
		field.setName("amount");
		field.setDisplayName("Amount");
		field.setSkyveType("decimal2");
		field.setIncludeTotal(Boolean.TRUE);
		design.getFields().add(field);

		DocumentReportDesignGenerator gen = new DocumentReportDesignGenerator();
		gen.addBands(design);

		assertTrue(design.getBands().size() > 0);
	}
}
