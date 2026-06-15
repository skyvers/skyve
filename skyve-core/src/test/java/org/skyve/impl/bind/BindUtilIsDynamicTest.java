package org.skyve.impl.bind;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.model.document.field.Text;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.Relation;
import org.skyve.metadata.module.Module;

@SuppressWarnings({"static-method", "boxing"})
class BindUtilIsDynamicTest {

	// -----------------------------------------------------------------------
	// isDynamic(Customer, Module, Document, Attribute) - 3-arg overload with document
	// -----------------------------------------------------------------------

	@Test
	void isDynamicDocumentAttributeTrueWhenDocumentIsDynamic() {
		Document doc = mock(Document.class);
		when(doc.isDynamic()).thenReturn(true);

		assertTrue(BindUtil.isDynamic(null, mock(Module.class), doc, null));
	}

	@Test
	void isDynamicDocumentAttributeFalseWhenBothFalseAndNullAttribute() {
		Document doc = mock(Document.class);
		when(doc.isDynamic()).thenReturn(false);

		assertFalse(BindUtil.isDynamic(null, mock(Module.class), doc, null));
	}

	@Test
	void isDynamicDocumentWithDynamicFieldAttributeReturnsTrue() {
		Document doc = mock(Document.class);
		when(doc.isDynamic()).thenReturn(false);

		Text field = new Text();
		field.setDynamic(true);

		assertTrue(BindUtil.isDynamic(null, mock(Module.class), doc, field));
	}

	@Test
	void isDynamicDocumentWithNonDynamicFieldAttributeReturnsFalse() {
		Document doc = mock(Document.class);
		when(doc.isDynamic()).thenReturn(false);

		Text field = new Text();
		field.setDynamic(false);

		assertFalse(BindUtil.isDynamic(null, mock(Module.class), doc, field));
	}

	// -----------------------------------------------------------------------
	// isDynamic(Customer, Module, Attribute) - attribute-only overload
	// -----------------------------------------------------------------------

	@Test
	void isDynamicAttributeNullReturnsFalse() {
		assertFalse(BindUtil.isDynamic(null, mock(Module.class), (org.skyve.metadata.model.Attribute) null));
	}

	@Test
	void isDynamicFieldAttributeTrueWhenFieldIsDynamic() {
		Text field = new Text();
		field.setDynamic(true);

		assertTrue(BindUtil.isDynamic(null, mock(Module.class), field));
	}

	@Test
	void isDynamicFieldAttributeFalseWhenFieldNotDynamic() {
		Text field = new Text();
		field.setDynamic(false);

		assertFalse(BindUtil.isDynamic(null, mock(Module.class), field));
	}

	@Test
	void isDynamicRelationAttributeDelegatesToRelationOverload() {
		Relation relation = mock(Relation.class);
		when(relation.getDocumentName()).thenReturn("Contact");

		Document relatedDoc = mock(Document.class);
		when(relatedDoc.isDynamic()).thenReturn(true);

		Module module = mock(Module.class);
		Customer customer = mock(Customer.class);
		when(module.getDocument(customer, "Contact")).thenReturn(relatedDoc);

		assertTrue(BindUtil.isDynamic(customer, module, relation));
	}

	// -----------------------------------------------------------------------
	// isDynamic(Customer, Module, Relation) - relation overload
	// -----------------------------------------------------------------------

	@Test
	void isDynamicRelationReturnsTrueWhenRelatedDocIsDynamic() {
		Relation relation = mock(Relation.class);
		when(relation.getDocumentName()).thenReturn("DynDoc");

		Document relatedDoc = mock(Document.class);
		when(relatedDoc.isDynamic()).thenReturn(true);

		Module module = mock(Module.class);
		Customer customer = mock(Customer.class);
		when(module.getDocument(customer, "DynDoc")).thenReturn(relatedDoc);

		assertTrue(BindUtil.isDynamic(customer, module, relation));
	}

	@Test
	void isDynamicRelationReturnsFalseWhenRelatedDocNotDynamic() {
		Relation relation = mock(Relation.class);
		when(relation.getDocumentName()).thenReturn("StaticDoc");

		Document relatedDoc = mock(Document.class);
		when(relatedDoc.isDynamic()).thenReturn(false);

		Module module = mock(Module.class);
		Customer customer = mock(Customer.class);
		when(module.getDocument(customer, "StaticDoc")).thenReturn(relatedDoc);

		assertFalse(BindUtil.isDynamic(customer, module, relation));
	}

	// -----------------------------------------------------------------------
	// isDynamic with Relation in the 4-arg Document+Attribute overload
	// -----------------------------------------------------------------------

	@Test
	void isDynamicDocumentWithRelationAttributeTrueWhenRelatedDocDynamic() {
		Document owningDoc = mock(Document.class);
		when(owningDoc.isDynamic()).thenReturn(false);

		Relation relation = mock(Relation.class);
		when(relation.getDocumentName()).thenReturn("DynDoc");

		Document relatedDoc = mock(Document.class);
		when(relatedDoc.isDynamic()).thenReturn(true);

		Module module = mock(Module.class);
		Customer customer = mock(Customer.class);
		when(module.getDocument(customer, "DynDoc")).thenReturn(relatedDoc);

		assertTrue(BindUtil.isDynamic(customer, module, owningDoc, relation));
	}
}
