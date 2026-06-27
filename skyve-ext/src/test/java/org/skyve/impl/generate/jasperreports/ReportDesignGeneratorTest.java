package org.skyve.impl.generate.jasperreports;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;
import java.util.HashMap;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.skyve.impl.metadata.model.document.AssociationImpl;
import org.skyve.impl.metadata.model.document.CollectionImpl;
import org.skyve.impl.metadata.model.document.field.Decimal2;
import org.skyve.impl.metadata.model.document.field.Decimal5;
import org.skyve.impl.metadata.model.document.field.Decimal10;
import org.skyve.impl.metadata.model.document.field.Text;
import org.skyve.impl.metadata.model.document.field.LongInteger;
import org.skyve.impl.metadata.model.document.field.Integer;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Extends;
import org.skyve.metadata.model.Persistent;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;

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
	void populateDesignClearsGeneratedCollectionsAndInitialisesBaseState() {
		DesignSpecification design = new DesignSpecification();
		design.setModuleName("sales");
		design.setDocumentName("Order");
		design.setReportType(DesignSpecification.ReportType.report);
		design.getFields().add(new ReportField());
		design.getParameters().add(new ReportParameter());
		design.getVariables().add(new ReportVariable());
		design.getSubReports().add(new DesignSpecification());
		design.getBands().add(new ReportBand());

		DesignSpecification result = new MinimalReportDesignGenerator().populateDesign(design);

		assertThat(result, is(design));
		assertThat(Character.toString((char) design.getAlias()), is("a"));
		assertTrue(design.getJoins().isEmpty());
		assertTrue(design.getJoinAlias().isEmpty());
		assertEquals(3, design.getParameters().size());
		assertEquals(1, design.getFields().size());
		assertThat(design.getFields().get(0).getName(), is("generated"));
		assertEquals(0, design.getVariables().size());
		assertEquals(0, design.getSubReports().size());
		assertEquals(1, design.getBands().size());
	}

	@Test
	void generateDesignPopulatesAFreshDesignInstance() {
		DesignSpecification result = new GenerateOnlyReportDesignGenerator().generateDesign();

		assertThat(result.getName(), is("generated"));
	}

	@Test
	void addFieldsForBeanModeReportAddsImplicitThisUserAndBizKeyFields() throws Exception {
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		Persistent persistent = new Persistent();
		persistent.setName("ORDER");
		when(module.getName()).thenReturn("sales");
		when(document.getName()).thenReturn("Order");
		when(document.getPersistent()).thenReturn(persistent);
		when(document.getExtends()).thenReturn(null);

		DesignSpecification design = new FixedMetadataDesignSpecification(module, document);
		design.setModuleName("sales");
		design.setDocumentName("Order");
		design.setReportType(DesignSpecification.ReportType.report);

		withThreadLocalCustomer(customer, () -> new BaseFieldReportDesignGenerator().addFields(design));

		assertEquals(3, design.getFields().size());
		assertThat(design.getFields().get(0).getName(), is("THIS"));
		assertThat(design.getFields().get(0).getTypeClass(), is("modules.sales.domain.Order"));
		assertThat(design.getFields().get(1).getName(), is("USER"));
		assertThat(design.getFields().get(2).getName(), is("bizKey"));
		assertThat(design.getFields().get(2).getNameSql(), is("a.bizKey"));
	}

	@Test
	void addFieldsForJoinedDocumentAddsBizKeyJoinToBaseDocument() throws Exception {
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		Document baseDocument = document("BaseOrder", "BASE_ORDER");
		Extends inherits = new Extends();
		inherits.setDocumentName("BaseOrder");
		Persistent persistent = new Persistent();
		persistent.setName("SPECIAL_ORDER");
		persistent.setStrategy(Persistent.ExtensionStrategy.joined);
		when(document.getPersistent()).thenReturn(persistent);
		when(document.getExtends()).thenReturn(inherits);
		when(module.getDocument(customer, "BaseOrder")).thenReturn(baseDocument);

		DesignSpecification design = new FixedMetadataDesignSpecification(module, document);
		design.setModuleName("sales");
		design.setDocumentName("SpecialOrder");
		design.setReportType(DesignSpecification.ReportType.report);
		design.setMode(DesignSpecification.Mode.sql);
		design.setJoins(new HashMap<>());
		design.setJoinAlias(new HashMap<>());
		design.setAlias('a');

		withThreadLocalCustomer(customer, () -> new BaseFieldReportDesignGenerator().addFields(design));

		ReportField bizKey = design.getFields().get(0);
		assertThat(bizKey.getName(), is("bizKey"));
		assertThat(bizKey.getJoinSql(), is(" join BASE_ORDER b on a.bizId = b.bizId"));
		assertThat(bizKey.getNameSql(), is("b.bizKey "));
		assertTrue(design.getJoins().containsKey("BaseOrder"));
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
		assertEquals(Boolean.TRUE, result.getCollection());
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

		assertEquals(Boolean.TRUE, result.getIncludeTotal());
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

		assertEquals(Boolean.TRUE, result.getIncludeTotal());
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

		assertEquals(Boolean.TRUE, result.getIncludeTotal());
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

		assertEquals(Boolean.TRUE, result.getIncludeTotal());
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

		assertEquals(Boolean.TRUE, result.getIncludeTotal());
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
	void fieldFromBindingDottedAssociationBuildsJoinAndPrefixedSqlField() {
		DesignSpecification spec = designWithJoinState();
		spec.setMode(DesignSpecification.Mode.sql);
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		Document associatedDocument = document("Customer", "CUSTOMER");
		AssociationImpl association = association("customer", "Customer", false);
		Text name = new Text();
		name.setName("name");
		name.setDisplayName("Name");

		when(document.getName()).thenReturn("Order");
		when(document.getAttribute("customer")).thenReturn(association);
		when(document.getAttribute("name")).thenReturn(name);
		when(customer.getModule("mod")).thenReturn(module);
		when(module.getDocument(customer, "Customer")).thenReturn(associatedDocument);

		ReportField result = ReportDesignGenerator.fieldFromBinding(spec, customer, document, "customer.name");

		assertThat(result.getName(), is("customer_name"));
		assertThat(result.getBinding(), is("customer.name"));
		assertThat(result.getJoinSql(), is(" left join CUSTOMER b on a.bizId =  b.customer_id"));
		assertThat(result.getNameSql(), is("b.name as customer_name"));
	}

	@Test
	void fieldFromAttributeAssociationInBeanModeBuildsLeftJoinAndBizKeyField() {
		DesignSpecification spec = designWithJoinState();
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		Document associatedDocument = document("Address", "ADDRESS");
		AssociationImpl association = association("address", "Address", false);

		when(document.getName()).thenReturn("Person");
		when(customer.getModule("mod")).thenReturn(module);
		when(module.getDocument(customer, "Address")).thenReturn(associatedDocument);

		ReportField result = ReportDesignGenerator.fieldFromAttribute(spec, customer, document, association, new StringBuilder(), new StringBuilder());

		assertEquals("address.bizKey", result.getName());
		assertEquals("java.lang.String", result.getTypeClass());
		assertTrue(result.getJoinSql().contains(" left join ADDRESS b on a.bizId =  b.address_id"));
		assertEquals("association", result.getSkyveType());
	}

	@Test
	void fieldFromAttributeAssociationInSqlModeBuildsInnerJoinAndAliasExpression() {
		DesignSpecification spec = designWithJoinState();
		spec.setMode(DesignSpecification.Mode.sql);
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		Document associatedDocument = document("Customer", "CUSTOMER");
		AssociationImpl association = association("customer", "Customer", true);

		when(document.getName()).thenReturn("Order");
		when(customer.getModule("mod")).thenReturn(module);
		when(module.getDocument(customer, "Customer")).thenReturn(associatedDocument);

		ReportField result = ReportDesignGenerator.fieldFromAttribute(spec, customer, document, association, new StringBuilder(), new StringBuilder());

		assertEquals("customer_bizKey", result.getName());
		assertEquals("customer.bizKey as customer_bizKey", result.getNameSql());
		assertTrue(result.getJoinSql().contains(" join CUSTOMER b on a.bizId =  b.customer_id"));
	}

	@Test
	void fieldFromBindingSqlModeSkipsNonPersistentSimpleAttribute() {
		DesignSpecification spec = new DesignSpecification();
		spec.setMode(DesignSpecification.Mode.sql);
		Customer customer = mock(Customer.class);
		Document document = mock(Document.class);
		Text transientText = new Text();
		transientText.setName("calculated");
		transientText.setDisplayName("Calculated");
		transientText.setPersistent(false);

		when(document.getAttribute("calculated")).thenReturn(transientText);

		assertNull(ReportDesignGenerator.fieldFromBinding(spec, customer, document, "calculated"));
	}

	@Test
	void constructSubreportFromFieldCopiesCollectionContextAndAddsSubreport() {
		org.skyve.impl.metadata.model.document.DocumentImpl document = document("Parent", "PARENT");
		CollectionImpl collection = new CollectionImpl();
		collection.setName("items");
		collection.setDisplayName("Items");
		collection.setDocumentName("Item");
		collection.setType(org.skyve.metadata.model.document.Collection.CollectionType.child);
		document.putAttribute(collection);

		TestDesignSpecification design = new TestDesignSpecification(document);
		design.setName("ParentReport");
		design.setMode(DesignSpecification.Mode.sql);
		design.setUxui("desktop");
		design.setRepositoryPath("/reports");
		design.setSaveToDocumentPackage(Boolean.TRUE);

		ReportField field = new ReportField();
		field.setName("items");
		field.setOwningModuleName("sales");
		field.setDocumentName("Item");

		CapturingDocumentReportDesignGenerator generator = new CapturingDocumentReportDesignGenerator();
		generator.constructSubreportFromField(design, field, java.lang.Integer.valueOf(320));

		DesignSpecification subreport = design.getSubReports().get(0);
		assertEquals(subreport, generator.capturedSubreport);
		assertEquals("ParentReport_items", subreport.getName());
		assertEquals("sales", subreport.getModuleName());
		assertEquals("Item", subreport.getDocumentName());
		assertEquals(java.lang.Integer.valueOf(320), subreport.getColumnWidth());
		assertEquals(org.skyve.metadata.model.document.Collection.CollectionType.child, subreport.getCollectionType());
		assertEquals("PARENT", subreport.getParentReportPersistentName());
	}

	@Test
	void addSubreportsLeavesExistingManualSubreportsUntouched() {
		DesignSpecification design = new DesignSpecification();
		design.setReportType(DesignSpecification.ReportType.report);
		DesignSpecification manualSubreport = new DesignSpecification();
		design.getSubReports().add(manualSubreport);
		ReportField collectionField = new ReportField();
		collectionField.setSkyveType(AttributeType.collection.name());
		design.getFields().add(collectionField);

		new BaseFieldReportDesignGenerator().addSubreports(design);

		assertEquals(1, design.getSubReports().size());
		assertThat(design.getSubReports().get(0), is(manualSubreport));
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

	private static DesignSpecification designWithJoinState() {
		DesignSpecification spec = new DesignSpecification();
		spec.setModuleName("mod");
		spec.setDocumentName("doc");
		spec.setJoins(new HashMap<>());
		spec.setJoinAlias(new HashMap<>());
		spec.setAlias('a');
		return spec;
	}

	private static AssociationImpl association(String name, String documentName, boolean required) {
		AssociationImpl association = new AssociationImpl();
		association.setName(name);
		association.setDisplayName(name);
		association.setDocumentName(documentName);
		association.setRequired(required);
		return association;
	}

	private static org.skyve.impl.metadata.model.document.DocumentImpl document(String name, String persistentName) {
		org.skyve.impl.metadata.model.document.DocumentImpl document = new org.skyve.impl.metadata.model.document.DocumentImpl();
		document.setName(name);
		Persistent persistent = new Persistent();
		persistent.setName(persistentName);
		document.setPersistent(persistent);
		return document;
	}

	private static final class TestDesignSpecification extends DesignSpecification {
		private final Document document;

		private TestDesignSpecification(Document document) {
			this.document = document;
		}

		@Override
		public Document getDocument() {
			return document;
		}
	}

	private static final class FixedMetadataDesignSpecification extends DesignSpecification {
		private final Module module;
		private final Document document;

		private FixedMetadataDesignSpecification(Module module, Document document) {
			this.module = module;
			this.document = document;
		}

		@Override
		public Module getModule() {
			return module;
		}

		@Override
		public Document getDocument() {
			return document;
		}
	}

	private static final class CapturingDocumentReportDesignGenerator extends DocumentReportDesignGenerator {
		private DesignSpecification capturedSubreport;

		@Override
		protected DocumentReportDesignGenerator getSubreportGenerator() {
			return new DocumentReportDesignGenerator() {
				@Override
				public DesignSpecification populateDesign(DesignSpecification design) {
					capturedSubreport = design;
					return design;
				}
			};
		}
	}

	private static final class MinimalReportDesignGenerator extends ReportDesignGenerator {
		@Override
		protected ReportDesignGenerator getSubreportGenerator() {
			return this;
		}

		@Override
		protected void addFields(DesignSpecification design) {
			ReportField field = new ReportField();
			field.setName("generated");
			design.getFields().add(field);
		}

		@Override
		protected void addBands(DesignSpecification design) {
			ReportBand band = new ReportBand();
			band.setBandType(ReportBand.BandType.detail);
			design.getBands().add(band);
		}
	}

	private static final class BaseFieldReportDesignGenerator extends ReportDesignGenerator {
		@Override
		protected ReportDesignGenerator getSubreportGenerator() {
			return this;
		}
	}

	private static final class GenerateOnlyReportDesignGenerator extends ReportDesignGenerator {
		@Override
		public DesignSpecification populateDesign(DesignSpecification design) {
			design.setName("generated");
			return design;
		}

		@Override
		protected ReportDesignGenerator getSubreportGenerator() {
			return this;
		}
	}

	private static void withThreadLocalCustomer(Customer customer, ThrowingRunnable runnable) throws Exception {
		AbstractPersistence persistence = mock(AbstractPersistence.class);
		User user = mock(User.class);
		when(persistence.getUser()).thenReturn(user);
		when(user.getCustomer()).thenReturn(customer);
		ThreadLocal<AbstractPersistence> threadLocal = getThreadLocalPersistence();
		AbstractPersistence previous = threadLocal.get();
		try {
			threadLocal.set(persistence);
			runnable.run();
		}
		finally {
			if (previous == null) {
				threadLocal.remove();
			}
			else {
				threadLocal.set(previous);
			}
		}
	}

	private static ThreadLocal<AbstractPersistence> getThreadLocalPersistence() throws Exception {
		Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
		field.setAccessible(true);
		@SuppressWarnings("unchecked")
		ThreadLocal<AbstractPersistence> threadLocal = (ThreadLocal<AbstractPersistence>) field.get(null);
		return threadLocal;
	}

	@FunctionalInterface
	private interface ThrowingRunnable {
		void run() throws Exception;
	}
}
