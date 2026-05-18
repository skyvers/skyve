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

public class DocumentImplTest {

	private DocumentImpl doc;

	@BeforeEach
	public void setUp() {
		doc = new DocumentImpl();
	}

	// ----------------------------------------------------------------
	// lastModifiedMillis
	// ----------------------------------------------------------------

	@Test
	public void getLastModifiedMillisDefaultsToMaxValue() {
		assertEquals(Long.MAX_VALUE, doc.getLastModifiedMillis());
	}

	@Test
	public void setLastModifiedMillisRoundTrips() {
		doc.setLastModifiedMillis(12345L);
		assertEquals(12345L, doc.getLastModifiedMillis());
	}

	// ----------------------------------------------------------------
	// lastCheckedMillis
	// ----------------------------------------------------------------

	@Test
	public void getLastCheckedMillisIsInitialised() {
		assertTrue(doc.getLastCheckedMillis() > 0);
	}

	@Test
	public void setLastCheckedMillisRoundTrips() {
		doc.setLastCheckedMillis(99999L);
		assertEquals(99999L, doc.getLastCheckedMillis());
	}

	// ----------------------------------------------------------------
	// parentDocumentName
	// ----------------------------------------------------------------

	@Test
	public void getParentDocumentNameDefaultsToNull() {
		assertThat(doc.getParentDocumentName(), nullValue());
	}

	@Test
	public void setParentDocumentNameRoundTrips() {
		doc.setParentDocumentName("Contact");
		assertThat(doc.getParentDocumentName(), is("Contact"));
	}

	// ----------------------------------------------------------------
	// parentDatabaseIndex
	// ----------------------------------------------------------------

	@Test
	public void getParentDatabaseIndexDefaultsToNull() {
		assertThat(doc.getParentDatabaseIndex(), nullValue());
	}

	@Test
	public void setParentDatabaseIndexRoundTrips() {
		doc.setParentDatabaseIndex(Boolean.TRUE);
		assertThat(doc.getParentDatabaseIndex(), is(Boolean.TRUE));
	}

	// ----------------------------------------------------------------
	// bizKeyMethodCode
	// ----------------------------------------------------------------

	@Test
	public void getBizKeyMethodCodeDefaultsToNull() {
		assertThat(doc.getBizKeyMethodCode(), nullValue());
	}

	@Test
	public void setBizKeyMethodCodeRoundTrips() {
		doc.setBizKeyMethodCode("return getName();");
		assertThat(doc.getBizKeyMethodCode(), is("return getName();"));
	}

	// ----------------------------------------------------------------
	// bizKeyExpression
	// ----------------------------------------------------------------

	@Test
	public void getBizKeyExpressionDefaultsToNull() {
		assertThat(doc.getBizKeyExpression(), nullValue());
	}

	@Test
	public void setBizKeyExpressionRoundTrips() {
		doc.setBizKeyExpression("{name}");
		assertThat(doc.getBizKeyExpression(), is("{name}"));
	}

	// ----------------------------------------------------------------
	// bizKeySensitity
	// ----------------------------------------------------------------

	@Test
	public void getBizKeySensitityDefaultsToNull() {
		assertThat(doc.getBizKeySensitity(), nullValue());
	}

	@Test
	public void setBizKeySensitityRoundTrips() {
		doc.setBizKeySensitity(Sensitivity.confidential);
		assertThat(doc.getBizKeySensitity(), is(Sensitivity.confidential));
	}

	// ----------------------------------------------------------------
	// ordered
	// ----------------------------------------------------------------

	@Test
	public void isOrderedDefaultsToFalse() {
		assertFalse(doc.isOrdered());
	}

	@Test
	public void setOrderedRoundTrips() {
		doc.setOrdered(true);
		assertTrue(doc.isOrdered());
	}

	// ----------------------------------------------------------------
	// documentation
	// ----------------------------------------------------------------

	@Test
	public void getDocumentationDefaultsToNull() {
		assertThat(doc.getDocumentation(), nullValue());
	}

	@Test
	public void setDocumentationRoundTrips() {
		doc.setDocumentation("Some docs.");
		assertThat(doc.getDocumentation(), is("Some docs."));
	}

	// ----------------------------------------------------------------
	// properties
	// ----------------------------------------------------------------

	@Test
	public void getPropertiesInitiallyEmpty() {
		assertTrue(doc.getProperties().isEmpty());
	}

	@Test
	public void propertiesMapIsMutable() {
		Map<String, String> props = doc.getProperties();
		props.put("key", "value");
		assertThat(doc.getProperties(), hasKey("key"));
	}

	// ----------------------------------------------------------------
	// definedActionNames
	// ----------------------------------------------------------------

	@Test
	public void getDefinedActionNamesInitiallyEmpty() {
		assertThat(doc.getDefinedActionNames(), empty());
	}

	@Test
	public void definedActionNamesCanBePopulated() {
		Set<String> names = doc.getDefinedActionNames();
		names.add("Save");
		names.add("Delete");
		assertThat(doc.getDefinedActionNames(), containsInAnyOrder("Delete", "Save"));
	}

	// ----------------------------------------------------------------
	// uniqueConstraints
	// ----------------------------------------------------------------

	@Test
	public void getUniqueConstraintsInitiallyEmpty() {
		assertThat(doc.getUniqueConstraints(), empty());
	}

	@Test
	public void putUniqueConstraintAddsToList() {
		UniqueConstraintImpl uc = new UniqueConstraintImpl();
		uc.setName("uc1");
		doc.putUniqueConstraint(uc);
		assertEquals(1, doc.getUniqueConstraints().size());
	}

	@Test
	public void getUniqueConstraintByNameReturnsMatchingConstraint() {
		UniqueConstraintImpl uc = new UniqueConstraintImpl();
		uc.setName("uc1");
		doc.putUniqueConstraint(uc);
		UniqueConstraint result = doc.getUniqueConstraint("uc1");
		assertThat(result, is((UniqueConstraint) uc));
	}

	@Test
	public void getUniqueConstraintReturnsNullForUnknownName() {
		assertThat(doc.getUniqueConstraint("nope"), nullValue());
	}

	// ----------------------------------------------------------------
	// referenceNames / putRelation / getReferenceByName
	// ----------------------------------------------------------------

	@Test
	public void getReferenceNamesInitiallyEmpty() {
		assertThat(doc.getReferenceNames(), empty());
	}

	@Test
	public void putRelationExposesReferenceByName() {
		AssociationImpl assoc = new AssociationImpl();
		assoc.setName("contact");
		doc.putRelation(assoc);
		assertThat(doc.getReferenceByName("contact"), is(notNullValue()));
	}

	@Test
	public void putRelationAddsToReferenceNames() {
		AssociationImpl assoc = new AssociationImpl();
		assoc.setName("contact");
		doc.putRelation(assoc);
		assertThat(doc.getReferenceNames(), containsInAnyOrder("contact"));
	}

	@Test
	public void getReferenceByNameReturnsNullForUnknown() {
		assertThat(doc.getReferenceByName("unknown"), nullValue());
	}

	// ----------------------------------------------------------------
	// conditions
	// ----------------------------------------------------------------

	@Test
	public void getConditionsInitiallyEmpty() {
		assertTrue(doc.getConditions().isEmpty());
	}

	@Test
	public void getConditionNamesInitiallyEmpty() {
		assertThat(doc.getConditionNames(), empty());
	}

	@Test
	public void getConditionReturnsNullForUnknownName() {
		assertThat(doc.getCondition("nope"), nullValue());
	}

	@Test
	public void getConditionReturnsInsertedCondition() {
		ConditionImpl cond = new ConditionImpl();
		cond.setExpression("true");
		doc.getConditions().put("myCondition", cond);
		Condition result = doc.getCondition("myCondition");
		assertThat(result, is((Condition) cond));
	}

	@Test
	public void getConditionNamesReflectsInsertedCondition() {
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
	public void getBizKeyAttributeReturnsBizKeyFieldWithCorrectName() {
		assertThat(DocumentImpl.getBizKeyAttribute().getName(), is(Bean.BIZ_KEY));
	}

	@SuppressWarnings("static-method")
	@Test
	public void getBizKeyAttributeHasTextType() {
		assertThat(DocumentImpl.getBizKeyAttribute().getAttributeType(), is(AttributeType.text));
	}

	@SuppressWarnings("static-method")
	@Test
	public void getBizOrdinalAttributeReturnsOrdinalFieldWithCorrectName() {
		assertThat(DocumentImpl.getBizOrdinalAttribute().getName(), is(Bean.ORDINAL_NAME));
	}

	@SuppressWarnings("static-method")
	@Test
	public void getBizOrdinalAttributeHasIntegerType() {
		assertThat(DocumentImpl.getBizOrdinalAttribute().getAttributeType(), is(AttributeType.integer));
	}
}
