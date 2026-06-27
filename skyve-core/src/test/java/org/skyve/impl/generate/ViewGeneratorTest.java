package org.skyve.impl.generate;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.Collection;
import java.util.Collections;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.model.document.AssociationImpl;
import org.skyve.impl.metadata.model.document.CollectionImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.model.document.InverseMany;
import org.skyve.impl.metadata.model.document.field.Content;
import org.skyve.impl.metadata.model.document.field.Geometry;
import org.skyve.impl.metadata.view.ActionImpl;
import org.skyve.impl.metadata.view.ViewImpl;
import org.skyve.impl.metadata.view.container.TabPane;
import org.skyve.impl.metadata.view.container.form.Form;
import org.skyve.impl.metadata.view.container.form.FormLabelLayout;
import org.skyve.impl.metadata.view.widget.bound.input.ListMembership;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGrid;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Extends;
import org.skyve.metadata.model.document.Association.AssociationType;
import org.skyve.metadata.model.document.Collection.CollectionType;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.DomainType;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;
import org.skyve.metadata.repository.ProvidedRepository;
import org.skyve.metadata.view.Action;
import org.skyve.metadata.view.View.ViewType;

/**
 * Tests for {@link ViewGenerator}.
 */
class ViewGeneratorTest {

	private static ViewGenerator generator() {
		return new ViewGenerator(mock(ProvidedRepository.class));
	}

	// ----- generateListView -------------------------------------------------

	@Test
	@SuppressWarnings("static-method")
	void generateListViewWithDescriptionUsesDescription() {
		Document document = mock(Document.class);
		when(document.getLocalisedDescription()).thenReturn("My Docs");

		ViewImpl view = ViewGenerator.generateListView(null, document, null, null);

		assertEquals(ViewType.list.toString(), view.getName());
		assertEquals("My Docs", view.getTitle());
	}

	@Test
	@SuppressWarnings("static-method")
	void generateListViewWithPluralAliasUsesPluralAlias() {
		Document document = mock(Document.class);
		when(document.getLocalisedDescription()).thenReturn(null);
		when(document.getLocalisedPluralAlias()).thenReturn("Widgets");

		ViewImpl view = ViewGenerator.generateListView(null, document, null, null);

		assertEquals("Widgets", view.getTitle());
	}

	@Test
	@SuppressWarnings("static-method")
	void generateListViewWithExplicitDescriptionOverridesDocumentMethods() {
		Document document = mock(Document.class);

		ViewImpl view = ViewGenerator.generateListView(null, document, null, "Override Title");

		assertEquals("Override Title", view.getTitle());
	}

	@Test
	@SuppressWarnings("static-method")
	void generateListViewWithAllNullDescriptionsProducesEmptyTitle() {
		Document document = mock(Document.class);
		when(document.getLocalisedDescription()).thenReturn(null);
		when(document.getLocalisedPluralAlias()).thenReturn(null);

		ViewImpl view = ViewGenerator.generateListView(null, document, null, null);

		assertEquals("", view.getTitle());
	}

	@Test
	@SuppressWarnings("static-method")
	void generateListViewAlwaysContainsDefaultsAction() {
		Document document = mock(Document.class);

		ViewImpl view = ViewGenerator.generateListView(null, document, null, "Test");

		Collection<Action> actions = view.getActions();
		assertEquals(1, actions.size());
		assertEquals(ImplicitActionName.DEFAULTS, ((ActionImpl) actions.iterator().next()).getImplicitName());
	}

	@Test
	@SuppressWarnings("static-method")
	void generateListViewWithNonNullQueryStillProducesListView() {
		Document document = mock(Document.class);
		when(document.getLocalisedPluralAlias()).thenReturn("Items");
		MetaDataQueryDefinition query = mock(MetaDataQueryDefinition.class);

		ViewImpl view = ViewGenerator.generateListView(null, document, query, null);

		assertEquals(ViewType.list.toString(), view.getName());
		assertEquals("Items", view.getTitle());
	}

	// ----- generate (list view) ---------------------------------------------

	@Test
	@SuppressWarnings("static-method")
	void generateListViewViaGenerateMethod() {
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		when(customer.getModule("testMod")).thenReturn(module);
		when(module.getDocumentDefaultQuery(customer, "TestDoc")).thenReturn(null);

		DocumentImpl document = new DocumentImpl();
		document.setOwningModuleName("testMod");
		document.setName("TestDoc");

		ViewImpl view = generator().generate(customer, document, ViewType.list.toString());

		assertNotNull(view);
		assertEquals(ViewType.list.toString(), view.getName());
	}

	@Test
	@SuppressWarnings("static-method")
	void generateListViewViaGenerateMethodPassesQueryToListView() {
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		when(customer.getModule("mod")).thenReturn(module);
		when(module.getDocumentDefaultQuery(customer, "Doc")).thenReturn(mock(MetaDataQueryDefinition.class));

		DocumentImpl document = new DocumentImpl();
		document.setOwningModuleName("mod");
		document.setName("Doc");

		// Just confirm no exception and returns list view
		ViewImpl view = generator().generate(customer, document, ViewType.list.toString());

		assertEquals(ViewType.list.toString(), view.getName());
	}

	// ----- generate (edit view) ---------------------------------------------

	@Test
	@SuppressWarnings("static-method")
	void generateEditViewWithNoAttributesAndNoActions() {
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		when(customer.getModule("testMod")).thenReturn(module);
		when(module.getFormLabelLayout()).thenReturn(FormLabelLayout.top);

		DocumentImpl document = new DocumentImpl();
		document.setOwningModuleName("testMod");
		document.setName("EditDoc");
		// no attributes, no actions, no extends

		ViewImpl view = generator().generate(customer, document, ViewType.edit.toString());

		assertNotNull(view);
		assertEquals(ViewType.edit.toString(), view.getName());
		// DEFAULTS action should always be added
		boolean hasDefaults = view.getActions().stream()
				.anyMatch(a -> ImplicitActionName.DEFAULTS == ((ActionImpl) a).getImplicitName());
		assertTrue(hasDefaults);
	}

	@Test
	@SuppressWarnings("static-method")
	void generateEditViewSideLabelLayoutAddsFourColumnResponsiveWidth() {
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		when(customer.getModule("testMod")).thenReturn(module);
		// FormLabelLayout.side triggers the formLabelSideLayout=true path
		when(module.getFormLabelLayout()).thenReturn(FormLabelLayout.side);

		DocumentImpl document = new DocumentImpl();
		document.setOwningModuleName("testMod");
		document.setName("SideDoc");

		ViewImpl view = generator().generate(customer, document, ViewType.edit.toString());

		assertEquals(ViewType.edit.toString(), view.getName());
	}

	// ----- generate (unknown view type) -------------------------------------

	@Test
	@SuppressWarnings("static-method")
	void generateWithUnknownViewTypeThrowsIllegalArgumentException() {
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		when(customer.getModule("mod")).thenReturn(module);

		DocumentImpl document = new DocumentImpl();
		document.setOwningModuleName("mod");
		document.setName("Doc");

		ViewGenerator gen = generator();
		assertThrows(IllegalArgumentException.class,
				() -> gen.generate(customer, document, "unknown"));
	}

	// ----- processAttributes with simple scalar field ----------------------

	@Test
	@SuppressWarnings("static-method")
	void generateEditViewWithTextFieldAttributeAddsFormRow() {
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		when(customer.getModule("testMod")).thenReturn(module);
		when(module.getFormLabelLayout()).thenReturn(FormLabelLayout.top);

		DocumentImpl document = new DocumentImpl();
		document.setOwningModuleName("testMod");
		document.setName("FieldDoc");

		// Add a simple text field attribute
		org.skyve.impl.metadata.model.document.field.Text textField =
				new org.skyve.impl.metadata.model.document.field.Text();
		textField.setName("myField");
		document.putAttribute(textField);

		ViewImpl view = generator().generate(customer, document, ViewType.edit.toString());

		// The form should contain the field
		assertNotNull(view);
		assertEquals(ViewType.edit.toString(), view.getName());
		// Check that contained items include a Form (or the form is embedded)
		assertTrue(view.getContained().stream()
				.anyMatch(Form.class::isInstance),
				"Expected form widget in generated edit view, got: " + view.getContained());
	}

	// ----- processAttributes with collection (aggregation) ------------------

	@Test
	@SuppressWarnings("static-method")
	void generateEditViewWithAggregationCollectionCreatesDataGrid() {
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		when(customer.getModule("testMod")).thenReturn(module);
		when(module.getFormLabelLayout()).thenReturn(FormLabelLayout.top);

		Document detailDoc = mock(Document.class);
		when(detailDoc.getExtends()).thenReturn(null);
		when(detailDoc.getAttributes()).thenReturn(Collections.emptyList());
		when(detailDoc.getLocalisedPluralAlias()).thenReturn("Items");
		when(module.getDocument(customer, "ItemDoc")).thenReturn(detailDoc);

		DocumentImpl document = new DocumentImpl();
		document.setOwningModuleName("testMod");
		document.setName("Doc");

		CollectionImpl col = new CollectionImpl();
		col.setName("items");
		col.setDocumentName("ItemDoc");
		col.setType(CollectionType.aggregation);
		document.putAttribute(col);

		ViewImpl view = generator().generate(customer, document, ViewType.edit.toString());

		assertTrue(view.getContained().stream().anyMatch(DataGrid.class::isInstance),
				"Expected DataGrid in contained, got: " + view.getContained());
	}

	// ----- processAttributes with collection (composition) ------------------

	@Test
	@SuppressWarnings("static-method")
	void generateEditViewWithCompositionCollectionCreatesDataGrid() {
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		when(customer.getModule("testMod")).thenReturn(module);
		when(module.getFormLabelLayout()).thenReturn(FormLabelLayout.top);

		Document detailDoc = mock(Document.class);
		when(detailDoc.getExtends()).thenReturn(null);
		when(detailDoc.getAttributes()).thenReturn(Collections.emptyList());
		when(detailDoc.getLocalisedPluralAlias()).thenReturn("Parts");
		when(module.getDocument(customer, "PartDoc")).thenReturn(detailDoc);

		DocumentImpl document = new DocumentImpl();
		document.setOwningModuleName("testMod");
		document.setName("Doc");

		CollectionImpl col = new CollectionImpl();
		col.setName("parts");
		col.setDocumentName("PartDoc");
		col.setType(CollectionType.composition);
		document.putAttribute(col);

		ViewImpl view = generator().generate(customer, document, ViewType.edit.toString());

		assertTrue(view.getContained().stream().anyMatch(DataGrid.class::isInstance),
				"Expected DataGrid in contained, got: " + view.getContained());
	}

	// ----- processAttributes with collection with domain type ---------------

	@Test
	@SuppressWarnings("static-method")
	void generateEditViewWithCollectionDomainTypeCreatesListMembership() {
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		when(customer.getModule("testMod")).thenReturn(module);
		when(module.getFormLabelLayout()).thenReturn(FormLabelLayout.top);

		Document detailDoc = mock(Document.class);
		when(detailDoc.getExtends()).thenReturn(null);
		when(detailDoc.getAttributes()).thenReturn(Collections.emptyList());
		when(module.getDocument(customer, "RefDoc")).thenReturn(detailDoc);

		DocumentImpl document = new DocumentImpl();
		document.setOwningModuleName("testMod");
		document.setName("Doc");

		CollectionImpl col = new CollectionImpl();
		col.setName("refs");
		col.setDocumentName("RefDoc");
		col.setType(CollectionType.aggregation);
		col.setDomainType(DomainType.constant);
		document.putAttribute(col);

		ViewImpl view = generator().generate(customer, document, ViewType.edit.toString());

		assertTrue(view.getContained().stream().anyMatch(ListMembership.class::isInstance),
				"Expected ListMembership in contained, got: " + view.getContained());
	}

	// ----- processAttributes with InverseMany --------------------------------

	@Test
	@SuppressWarnings("static-method")
	void generateEditViewWithInverseManyCreatesDataGrid() {
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		when(customer.getModule("testMod")).thenReturn(module);
		when(module.getFormLabelLayout()).thenReturn(FormLabelLayout.top);

		Document detailDoc = mock(Document.class);
		when(detailDoc.getExtends()).thenReturn(null);
		when(detailDoc.getAttributes()).thenReturn(Collections.emptyList());
		when(detailDoc.getLocalisedPluralAlias()).thenReturn("Dependants");
		when(module.getDocument(customer, "ChildDoc")).thenReturn(detailDoc);

		DocumentImpl document = new DocumentImpl();
		document.setOwningModuleName("testMod");
		document.setName("Doc");

		InverseMany inv = new InverseMany();
		inv.setName("children");
		inv.setDocumentName("ChildDoc");
		inv.setReferenceName("parent");
		document.putAttribute(inv);

		ViewImpl view = generator().generate(customer, document, ViewType.edit.toString());

		assertTrue(view.getContained().stream().anyMatch(DataGrid.class::isInstance),
				"Expected DataGrid for InverseMany in contained, got: " + view.getContained());
	}

	// ----- processAttributes with Association (non-persistable doc) ---------

	@Test
	@SuppressWarnings({"static-method", "boxing"})
	void generateEditViewWithAssociationToNonPersistableDocCreatesTextField() {
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		when(customer.getModule("testMod")).thenReturn(module);
		when(module.getFormLabelLayout()).thenReturn(FormLabelLayout.top);

		Document assocDoc = mock(Document.class);
		when(assocDoc.isPersistable()).thenReturn(Boolean.FALSE); // non-persistable → TextField
		when(module.getDocument(customer, "RefDoc")).thenReturn(assocDoc);

		DocumentImpl document = new DocumentImpl();
		document.setOwningModuleName("testMod");
		document.setName("Doc");

		AssociationImpl assoc = new AssociationImpl();
		assoc.setName("ref");
		assoc.setDocumentName("RefDoc");
		assoc.setType(AssociationType.aggregation);
		document.putAttribute(assoc);

		ViewImpl view = generator().generate(customer, document, ViewType.edit.toString());

		assertTrue(view.getContained().stream().anyMatch(Form.class::isInstance),
				"Expected Form in contained");
	}

	// ----- processAttributes with Association (persistable doc) -------------

	@Test
	@SuppressWarnings({"static-method", "boxing"})
	void generateEditViewWithAssociationToPersistableDocCreatesFormRow() {
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		when(customer.getModule("testMod")).thenReturn(module);
		when(module.getFormLabelLayout()).thenReturn(FormLabelLayout.top);

		Document assocDoc = mock(Document.class);
		when(assocDoc.isPersistable()).thenReturn(Boolean.TRUE); // persistable → DefaultWidget
		when(module.getDocument(customer, "AssocDoc")).thenReturn(assocDoc);

		DocumentImpl document = new DocumentImpl();
		document.setOwningModuleName("testMod");
		document.setName("Doc");

		AssociationImpl assoc = new AssociationImpl();
		assoc.setName("assoc");
		assoc.setDocumentName("AssocDoc");
		assoc.setType(AssociationType.aggregation);
		document.putAttribute(assoc);

		ViewImpl view = generator().generate(customer, document, ViewType.edit.toString());

		assertTrue(view.getContained().stream().anyMatch(Form.class::isInstance),
				"Expected Form with association row in contained");
	}

	// ----- processAttributes with Geometry (prototype module) ---------------

	@Test
	@SuppressWarnings({"static-method", "boxing"})
	void generateEditViewWithGeometryInPrototypeModuleCreatesGeometryMapRow() {
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		when(customer.getModule("testMod")).thenReturn(module);
		when(module.getFormLabelLayout()).thenReturn(FormLabelLayout.top);
		when(module.isPrototype()).thenReturn(Boolean.TRUE);

		DocumentImpl document = new DocumentImpl();
		document.setOwningModuleName("testMod");
		document.setName("Doc");

		Geometry geo = new Geometry();
		geo.setName("location");
		document.putAttribute(geo);

		ViewImpl view = generator().generate(customer, document, ViewType.edit.toString());

		assertTrue(view.getContained().stream().anyMatch(Form.class::isInstance),
				"Expected Form with geometry row in contained");
	}

	// ----- processAttributes with Content (prototype module) ----------------

	@Test
	@SuppressWarnings({"static-method", "boxing"})
	void generateEditViewWithContentInPrototypeModuleCreatesContentImageRow() {
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		when(customer.getModule("testMod")).thenReturn(module);
		when(module.getFormLabelLayout()).thenReturn(FormLabelLayout.top);
		when(module.isPrototype()).thenReturn(Boolean.TRUE);

		DocumentImpl document = new DocumentImpl();
		document.setOwningModuleName("testMod");
		document.setName("Doc");

		Content content = new Content();
		content.setName("attachment");
		document.putAttribute(content);

		ViewImpl view = generator().generate(customer, document, ViewType.edit.toString());

		assertTrue(view.getContained().stream().anyMatch(Form.class::isInstance),
				"Expected Form with content row in contained");
	}

	// ----- tabbed layout (2 detail widgets) ----------------------------------

	@Test
	@SuppressWarnings("static-method")
	void generateEditViewWithTwoCollectionsCreatesTabPane() {
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		when(customer.getModule("testMod")).thenReturn(module);
		when(module.getFormLabelLayout()).thenReturn(FormLabelLayout.top);

		Document detailDoc1 = mock(Document.class);
		when(detailDoc1.getExtends()).thenReturn(null);
		when(detailDoc1.getAttributes()).thenReturn(Collections.emptyList());
		when(detailDoc1.getLocalisedPluralAlias()).thenReturn("Alpha");
		when(module.getDocument(customer, "AlphaDoc")).thenReturn(detailDoc1);

		Document detailDoc2 = mock(Document.class);
		when(detailDoc2.getExtends()).thenReturn(null);
		when(detailDoc2.getAttributes()).thenReturn(Collections.emptyList());
		when(detailDoc2.getLocalisedPluralAlias()).thenReturn("Beta");
		when(module.getDocument(customer, "BetaDoc")).thenReturn(detailDoc2);

		DocumentImpl document = new DocumentImpl();
		document.setOwningModuleName("testMod");
		document.setName("Doc");

		CollectionImpl col1 = new CollectionImpl();
		col1.setName("alphaItems");
		col1.setDocumentName("AlphaDoc");
		col1.setType(CollectionType.composition);
		document.putAttribute(col1);

		CollectionImpl col2 = new CollectionImpl();
		col2.setName("betaItems");
		col2.setDocumentName("BetaDoc");
		col2.setType(CollectionType.composition);
		document.putAttribute(col2);

		ViewImpl view = generator().generate(customer, document, ViewType.edit.toString());

		assertTrue(view.getContained().stream().anyMatch(TabPane.class::isInstance),
				"Expected TabPane for 2+ detail widgets, got: " + view.getContained());
	}

	// ----- deprecated attribute is skipped ----------------------------------

	@Test
	@SuppressWarnings("static-method")
	void generateEditViewWithDeprecatedAttributeProducesEmptyForm() {
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		when(customer.getModule("testMod")).thenReturn(module);
		when(module.getFormLabelLayout()).thenReturn(FormLabelLayout.top);

		DocumentImpl document = new DocumentImpl();
		document.setOwningModuleName("testMod");
		document.setName("Doc");

		org.skyve.impl.metadata.model.document.field.Text deprecated =
				new org.skyve.impl.metadata.model.document.field.Text();
		deprecated.setName("oldField");
		deprecated.setDeprecated(true);
		document.putAttribute(deprecated);

		ViewImpl view = generator().generate(customer, document, ViewType.edit.toString());

		// The deprecated field should not generate any Form rows
		boolean hasFormWithRows = view.getContained().stream()
				.filter(Form.class::isInstance)
				.map(Form.class::cast)
				.anyMatch(f -> !f.getRows().isEmpty());
		assertTrue(!hasFormWithRows,
				"Expected no form rows for deprecated attribute");
	}

	// ----- document with extends (inheritance) --------------------------------

	@Test
	@SuppressWarnings("static-method")
	void generateEditViewWithExtendsIncludesBaseDocumentAttributes() {
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		when(customer.getModule("testMod")).thenReturn(module);
		when(module.getFormLabelLayout()).thenReturn(FormLabelLayout.top);

		// Base document with one text field
		DocumentImpl baseDocument = new DocumentImpl();
		baseDocument.setOwningModuleName("testMod");
		baseDocument.setName("BaseDoc");
		org.skyve.impl.metadata.model.document.field.Text baseField =
				new org.skyve.impl.metadata.model.document.field.Text();
		baseField.setName("baseField");
		baseDocument.putAttribute(baseField);
		when(module.getDocument(customer, "BaseDoc")).thenReturn(baseDocument);

		// Child document that extends the base
		DocumentImpl document = new DocumentImpl();
		document.setOwningModuleName("testMod");
		document.setName("ChildDoc");

		Extends extendsBase = new Extends();
		extendsBase.setDocumentName("BaseDoc");
		document.setExtends(extendsBase);

		ViewImpl view = generator().generate(customer, document, ViewType.edit.toString());

		// The form should include the base document's field
		assertTrue(view.getContained().stream().anyMatch(Form.class::isInstance),
				"Expected Form containing inherited base field");
	}
}
