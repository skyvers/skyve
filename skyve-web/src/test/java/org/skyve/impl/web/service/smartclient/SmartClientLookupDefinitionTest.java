package org.skyve.impl.web.service.smartclient;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.Collections;
import java.util.List;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.view.widget.bound.input.LookupDescription;
import org.skyve.impl.metadata.view.widget.bound.input.LookupDescriptionColumn;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.Reference;
import org.skyve.metadata.model.document.Relation;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;
import org.skyve.metadata.user.User;

class SmartClientLookupDefinitionTest {
	@Test
	void constructorUsesReferenceQueryWhenLookupIsNull() {
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		Reference relation = mock(Reference.class);
		MetaDataQueryDefinition query = mock(MetaDataQueryDefinition.class);
		Document queryDocument = mock(Document.class);
		Customer customer = mock(Customer.class);

		when(module.getName()).thenReturn("admin");
		when(document.getName()).thenReturn("Invoice");
		when(relation.getName()).thenReturn("customer");
		when(relation.getDocumentName()).thenReturn("Contact");
		when(relation.getQueryName()).thenReturn("ordersByCustomer");
		when(module.getNullSafeMetaDataQuery("ordersByCustomer")).thenReturn(query);
		when(query.getName()).thenReturn("ordersByCustomer");
		when(query.getDocumentName()).thenReturn("Contact");
		when(module.getDocument(customer, "Contact")).thenReturn(queryDocument);

		SmartClientLookupDefinition definition = new SmartClientLookupDefinition(false,
				null,
				customer,
				module,
				document,
				relation,
				null,
				true,
				"desktop");

		assertEquals("admin_ordersByCustomer_Invoice_customer", definition.getOptionDataSource());
		assertEquals("bizKey", definition.getDisplayField());
		assertEquals(1, definition.getPickListFields().size());
		assertEquals("bizKey", definition.getPickListFields().get(0));
		assertTrue(definition.getFilterFields().isEmpty());
		assertTrue(definition.getCanCreate());
		assertTrue(definition.getCanUpdate());
		assertFalse(definition.isBindingToDataGrid());
	}

	@Test
	void constructorFallsBackToDefaultQueryAndAppliesUserPermissions() {
		User user = mock(User.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		Relation relation = mock(Relation.class);
		LookupDescription lookup = mock(LookupDescription.class);
		MetaDataQueryDefinition query = mock(MetaDataQueryDefinition.class);
		Document queryDocument = mock(Document.class);
		Customer customer = mock(Customer.class);

		when(module.getName()).thenReturn("sales");
		when(document.getName()).thenReturn("Order");
		when(relation.getName()).thenReturn("contact");
		when(relation.getDocumentName()).thenReturn("Contact");
		when(lookup.getQuery()).thenReturn(null);
		when(lookup.getDescriptionBinding()).thenReturn("address[0].line1");
		when(lookup.getDropDownColumns()).thenReturn(null);
		when(module.getDocumentDefaultQuery(customer, "Contact")).thenReturn(query);
		when(query.getName()).thenReturn("contactDefault");
		when(query.getDocumentName()).thenReturn("Contact");
		when(module.getDocument(customer, "Contact")).thenReturn(queryDocument);
		when(user.canCreateDocument(queryDocument)).thenReturn(false);
		when(user.canUpdateDocument(queryDocument)).thenReturn(true);

		SmartClientLookupDefinition definition = new SmartClientLookupDefinition(true,
				user,
				customer,
				module,
				document,
				relation,
				lookup,
				true,
				"desktop");

		assertEquals("sales_contactDefault_Order_contact", definition.getOptionDataSource());
		assertEquals("address_0__line1", definition.getDisplayField());
		assertEquals(1, definition.getPickListFields().size());
		assertEquals("address_0__line1", definition.getPickListFields().get(0));
		assertTrue(definition.getFilterFields().isEmpty());
		assertFalse(definition.getCanCreate());
		assertTrue(definition.getCanUpdate());
		assertTrue(definition.isBindingToDataGrid());
	}

	@Test
	void constructorWithDropDownColumnsAndNoProjectedColumnsLeavesPickListEmpty() {
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		Relation relation = mock(Relation.class);
		LookupDescription lookup = mock(LookupDescription.class);
		MetaDataQueryDefinition query = mock(MetaDataQueryDefinition.class);
		Document queryDocument = mock(Document.class);
		Customer customer = mock(Customer.class);

		when(module.getName()).thenReturn("admin");
		when(document.getName()).thenReturn("Invoice");
		when(relation.getName()).thenReturn("customer");
		when(relation.getDocumentName()).thenReturn("Contact");
		when(lookup.getQuery()).thenReturn("customerQuery");
		when(lookup.getDescriptionBinding()).thenReturn("bizKey");
		when(lookup.getDropDownColumns()).thenReturn(Collections.singletonList(mock(LookupDescriptionColumn.class)));
		when(module.getNullSafeMetaDataQuery("customerQuery")).thenReturn(query);
		when(query.getName()).thenReturn("customerQuery");
		when(query.getDocumentName()).thenReturn("Contact");
		when(query.getColumns()).thenReturn(Collections.emptyList());
		when(module.getDocument(customer, "Contact")).thenReturn(queryDocument);

		SmartClientLookupDefinition definition = new SmartClientLookupDefinition(false,
				null,
				customer,
				module,
				document,
				relation,
				lookup,
				true,
				"desktop");

		assertTrue(definition.getPickListFields().isEmpty());
		assertTrue(definition.getFilterFields().isEmpty());
	}

	@Test
	void constructorWithEmptyDropDownColumnsFallsBackToDisplayField() {
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		Relation relation = mock(Relation.class);
		LookupDescription lookup = mock(LookupDescription.class);
		MetaDataQueryDefinition query = mock(MetaDataQueryDefinition.class);
		Document queryDocument = mock(Document.class);
		Customer customer = mock(Customer.class);

		when(module.getName()).thenReturn("admin");
		when(document.getName()).thenReturn("Invoice");
		when(relation.getName()).thenReturn("customer");
		when(relation.getDocumentName()).thenReturn("Contact");
		when(lookup.getQuery()).thenReturn("customerQuery");
		when(lookup.getDescriptionBinding()).thenReturn("display.name");
		when(lookup.getDropDownColumns()).thenReturn(Collections.emptyList());
		when(module.getNullSafeMetaDataQuery("customerQuery")).thenReturn(query);
		when(query.getName()).thenReturn("customerQuery");
		when(query.getDocumentName()).thenReturn("Contact");
		when(module.getDocument(customer, "Contact")).thenReturn(queryDocument);

		SmartClientLookupDefinition definition = new SmartClientLookupDefinition(false,
				null,
				customer,
				module,
				document,
				relation,
				lookup,
				true,
				"desktop");

		assertEquals("display_name", definition.getDisplayField());
		assertEquals(List.of("display_name"), definition.getPickListFields());
		assertTrue(definition.getFilterFields().isEmpty());
	}

	@Test
	void mutablePropertySettersUpdateState() {
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		Relation relation = mock(Relation.class);
		MetaDataQueryDefinition query = mock(MetaDataQueryDefinition.class);
		Document queryDocument = mock(Document.class);
		Customer customer = mock(Customer.class);

		when(module.getName()).thenReturn("admin");
		when(document.getName()).thenReturn("Invoice");
		when(relation.getName()).thenReturn("customer");
		when(relation.getDocumentName()).thenReturn("Contact");
		when(module.getDocumentDefaultQuery(customer, "Contact")).thenReturn(query);
		when(query.getName()).thenReturn("defaultQuery");
		when(query.getDocumentName()).thenReturn("Contact");
		when(module.getDocument(customer, "Contact")).thenReturn(queryDocument);

		SmartClientLookupDefinition definition = new SmartClientLookupDefinition(false,
				null,
				customer,
				module,
				document,
				relation,
				null,
				true,
				"desktop");

		MetaDataQueryDefinition queryOverride = mock(MetaDataQueryDefinition.class);
		definition.setDisplayField("name");
		definition.setOptionDataSource("opt_ds");
		definition.setBindingToDataGrid(true);
		definition.setCanCreate(false);
		definition.setCanUpdate(false);
		definition.setQuery(queryOverride);

		assertEquals("name", definition.getDisplayField());
		assertEquals("opt_ds", definition.getOptionDataSource());
		assertTrue(definition.isBindingToDataGrid());
		assertFalse(definition.getCanCreate());
		assertFalse(definition.getCanUpdate());
		assertSame(queryOverride, definition.getQuery());
	}
}
