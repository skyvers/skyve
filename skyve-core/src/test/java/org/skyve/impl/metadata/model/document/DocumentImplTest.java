package org.skyve.impl.metadata.model.document;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsInAnyOrder;
import static org.hamcrest.Matchers.empty;
import static org.hamcrest.Matchers.hasKey;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.nullValue;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.Map;
import java.util.Set;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.domain.Bean;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.Attribute.Sensitivity;
import org.skyve.metadata.model.document.Condition;
import org.skyve.metadata.model.document.UniqueConstraint;

class DocumentImplTest {

	private DocumentImpl doc;

	@BeforeEach
	void setUp() {
		doc = new DocumentImpl();
	}

	// ----------------------------------------------------------------
	// lastModifiedMillis
	// ----------------------------------------------------------------

	@Test
	void getLastModifiedMillisDefaultsToMaxValue() {
		assertEquals(Long.MAX_VALUE, doc.getLastModifiedMillis());
	}

	@Test
	void setLastModifiedMillisRoundTrips() {
		doc.setLastModifiedMillis(12345L);
		assertEquals(12345L, doc.getLastModifiedMillis());
	}

	// ----------------------------------------------------------------
	// lastCheckedMillis
	// ----------------------------------------------------------------

	@Test
	void getLastCheckedMillisIsInitialised() {
		assertTrue(doc.getLastCheckedMillis() > 0);
	}

	@Test
	void setLastCheckedMillisRoundTrips() {
		doc.setLastCheckedMillis(99999L);
		assertEquals(99999L, doc.getLastCheckedMillis());
	}

	// ----------------------------------------------------------------
	// parentDocumentName
	// ----------------------------------------------------------------

	@Test
	void getParentDocumentNameDefaultsToNull() {
		assertThat(doc.getParentDocumentName(), nullValue());
	}

	@Test
	void setParentDocumentNameRoundTrips() {
		doc.setParentDocumentName("Contact");
		assertThat(doc.getParentDocumentName(), is("Contact"));
	}

	// ----------------------------------------------------------------
	// parentDatabaseIndex
	// ----------------------------------------------------------------

	@Test
	void getParentDatabaseIndexDefaultsToNull() {
		assertThat(doc.getParentDatabaseIndex(), nullValue());
	}

	@Test
	void setParentDatabaseIndexRoundTrips() {
		doc.setParentDatabaseIndex(Boolean.TRUE);
		assertThat(doc.getParentDatabaseIndex(), is(Boolean.TRUE));
	}

	// ----------------------------------------------------------------
	// bizKeyMethodCode
	// ----------------------------------------------------------------

	@Test
	void getBizKeyMethodCodeDefaultsToNull() {
		assertThat(doc.getBizKeyMethodCode(), nullValue());
	}

	@Test
	void setBizKeyMethodCodeRoundTrips() {
		doc.setBizKeyMethodCode("return getName();");
		assertThat(doc.getBizKeyMethodCode(), is("return getName();"));
	}

	// ----------------------------------------------------------------
	// bizKeyExpression
	// ----------------------------------------------------------------

	@Test
	void getBizKeyExpressionDefaultsToNull() {
		assertThat(doc.getBizKeyExpression(), nullValue());
	}

	@Test
	void setBizKeyExpressionRoundTrips() {
		doc.setBizKeyExpression("{name}");
		assertThat(doc.getBizKeyExpression(), is("{name}"));
	}

	// ----------------------------------------------------------------
	// bizKeySensitity
	// ----------------------------------------------------------------

	@Test
	void getBizKeySensitityDefaultsToNull() {
		assertThat(doc.getBizKeySensitity(), nullValue());
	}

	@Test
	void setBizKeySensitityRoundTrips() {
		doc.setBizKeySensitity(Sensitivity.confidential);
		assertThat(doc.getBizKeySensitity(), is(Sensitivity.confidential));
	}

	// ----------------------------------------------------------------
	// ordered
	// ----------------------------------------------------------------

	@Test
	void isOrderedDefaultsToFalse() {
		assertFalse(doc.isOrdered());
	}

	@Test
	void setOrderedRoundTrips() {
		doc.setOrdered(true);
		assertTrue(doc.isOrdered());
	}

	// ----------------------------------------------------------------
	// documentation
	// ----------------------------------------------------------------

	@Test
	void getDocumentationDefaultsToNull() {
		assertThat(doc.getDocumentation(), nullValue());
	}

	@Test
	void setDocumentationRoundTrips() {
		doc.setDocumentation("Some docs.");
		assertThat(doc.getDocumentation(), is("Some docs."));
	}

	// ----------------------------------------------------------------
	// properties
	// ----------------------------------------------------------------

	@Test
	void getPropertiesInitiallyEmpty() {
		assertTrue(doc.getProperties().isEmpty());
	}

	@Test
	void propertiesMapIsMutable() {
		Map<String, String> props = doc.getProperties();
		props.put("key", "value");
		assertThat(doc.getProperties(), hasKey("key"));
	}

	// ----------------------------------------------------------------
	// definedActionNames
	// ----------------------------------------------------------------

	@Test
	void getDefinedActionNamesInitiallyEmpty() {
		assertThat(doc.getDefinedActionNames(), empty());
	}

	@Test
	void definedActionNamesCanBePopulated() {
		Set<String> names = doc.getDefinedActionNames();
		names.add("Save");
		names.add("Delete");
		assertThat(doc.getDefinedActionNames(), containsInAnyOrder("Delete", "Save"));
	}

	// ----------------------------------------------------------------
	// uniqueConstraints
	// ----------------------------------------------------------------

	@Test
	void getUniqueConstraintsInitiallyEmpty() {
		assertThat(doc.getUniqueConstraints(), empty());
	}

	@Test
	void putUniqueConstraintAddsToList() {
		UniqueConstraintImpl uc = new UniqueConstraintImpl();
		uc.setName("uc1");
		doc.putUniqueConstraint(uc);
		assertEquals(1, doc.getUniqueConstraints().size());
	}

	@Test
	void getUniqueConstraintByNameReturnsMatchingConstraint() {
		UniqueConstraintImpl uc = new UniqueConstraintImpl();
		uc.setName("uc1");
		doc.putUniqueConstraint(uc);
		UniqueConstraint result = doc.getUniqueConstraint("uc1");
		assertThat(result, is((UniqueConstraint) uc));
	}

	@Test
	void getUniqueConstraintReturnsNullForUnknownName() {
		assertThat(doc.getUniqueConstraint("nope"), nullValue());
	}

	// ----------------------------------------------------------------
	// referenceNames / putRelation / getReferenceByName
	// ----------------------------------------------------------------

	@Test
	void getReferenceNamesInitiallyEmpty() {
		assertThat(doc.getReferenceNames(), empty());
	}

	@Test
	void putRelationExposesReferenceByName() {
		AssociationImpl assoc = new AssociationImpl();
		assoc.setName("contact");
		doc.putRelation(assoc);
		assertThat(doc.getReferenceByName("contact"), is(notNullValue()));
	}

	@Test
	void putRelationAddsToReferenceNames() {
		AssociationImpl assoc = new AssociationImpl();
		assoc.setName("contact");
		doc.putRelation(assoc);
		assertThat(doc.getReferenceNames(), containsInAnyOrder("contact"));
	}

	@Test
	void getReferenceByNameReturnsNullForUnknown() {
		assertThat(doc.getReferenceByName("unknown"), nullValue());
	}

	// ----------------------------------------------------------------
	// conditions
	// ----------------------------------------------------------------

	@Test
	void getConditionsInitiallyEmpty() {
		assertTrue(doc.getConditions().isEmpty());
	}

	@Test
	void getConditionNamesInitiallyEmpty() {
		assertThat(doc.getConditionNames(), empty());
	}

	@Test
	void getConditionReturnsNullForUnknownName() {
		assertThat(doc.getCondition("nope"), nullValue());
	}

	@Test
	void getConditionReturnsInsertedCondition() {
		ConditionImpl cond = new ConditionImpl();
		cond.setExpression("true");
		doc.getConditions().put("myCondition", cond);
		Condition result = doc.getCondition("myCondition");
		assertThat(result, is((Condition) cond));
	}

	@Test
	void getConditionNamesReflectsInsertedCondition() {
		ConditionImpl cond = new ConditionImpl();
		cond.setExpression("false");
		doc.getConditions().put("active", cond);
		assertThat(doc.getConditionNames(), containsInAnyOrder("active"));
	}

	// ----------------------------------------------------------------
	// static factory attributes
	// ----------------------------------------------------------------

	@SuppressWarnings("static-method")
	@Test
	void getBizKeyAttributeReturnsBizKeyFieldWithCorrectName() {
		assertThat(DocumentImpl.getBizKeyAttribute().getName(), is(Bean.BIZ_KEY));
	}

	@SuppressWarnings("static-method")
	@Test
	void getBizKeyAttributeHasTextType() {
		assertThat(DocumentImpl.getBizKeyAttribute().getAttributeType(), is(AttributeType.text));
	}

	@SuppressWarnings("static-method")
	@Test
	void getBizOrdinalAttributeReturnsOrdinalFieldWithCorrectName() {
		assertThat(DocumentImpl.getBizOrdinalAttribute().getName(), is(Bean.ORDINAL_NAME));
	}

	@SuppressWarnings("static-method")
	@Test
	void getBizOrdinalAttributeHasIntegerType() {
		assertThat(DocumentImpl.getBizOrdinalAttribute().getAttributeType(), is(AttributeType.integer));
	}

	// ---- InverseOne.getCardinality ----

	@SuppressWarnings("static-method")
	@Test
	void inverseOneGetCardinalityReturnsOne() {
		assertThat(new InverseOne().getCardinality(), is(org.skyve.metadata.model.document.Inverse.InverseCardinality.one));
	}

	// ---- CollectionImpl.isRequired ----

	@SuppressWarnings("static-method")
	@Test
	void collectionImplIsRequiredWhenMinCardinalityAboveZero() {
		CollectionImpl coll = new CollectionImpl();
		coll.setMinCardinality(1);
		assertTrue(coll.isRequired());
	}

	// ---- AssociationImpl.setRequiredBool ----

	@SuppressWarnings("static-method")
	@Test
	void associationImplSetRequiredBoolSetsRequired() {
		AssociationImpl assoc = new AssociationImpl();
		assoc.setRequiredBool(Boolean.TRUE);
		assertTrue(assoc.isRequired());
	}
}
