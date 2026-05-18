package org.skyve.impl.metadata.model.document;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;

import org.junit.jupiter.api.Test;
import org.skyve.metadata.model.Attribute.Sensitivity;
import org.skyve.metadata.model.Attribute.UsageType;
import org.skyve.metadata.model.document.Association.AssociationType;
import org.skyve.metadata.model.document.Collection.CollectionType;

@SuppressWarnings({ "static-method", "boxing" })
class DocumentImplModelTest {

	// --- ConditionImpl ---

	@Test
	void conditionImplDocumentationNullByDefault() {
		ConditionImpl condition = new ConditionImpl();
		assertThat(condition.getDocumentation(), nullValue());
	}

	@Test
	void conditionImplSetAndGetDocumentation() {
		ConditionImpl condition = new ConditionImpl();
		condition.setDocumentation("Test documentation");
		assertThat(condition.getDocumentation(), is("Test documentation"));
	}

	@Test
	void conditionImplDescriptionNullByDefault() {
		ConditionImpl condition = new ConditionImpl();
		assertThat(condition.getDescription(), nullValue());
	}

	@Test
	void conditionImplSetAndGetDescription() {
		ConditionImpl condition = new ConditionImpl();
		condition.setDescription("Active condition");
		assertThat(condition.getDescription(), is("Active condition"));
	}

	@Test
	void conditionImplExpressionNullByDefault() {
		ConditionImpl condition = new ConditionImpl();
		assertThat(condition.getExpression(), nullValue());
	}

	@Test
	void conditionImplSetAndGetExpression() {
		ConditionImpl condition = new ConditionImpl();
		condition.setExpression("status == 'active'");
		assertThat(condition.getExpression(), is("status == 'active'"));
	}

	@Test
	void conditionImplUsageNullByDefault() {
		ConditionImpl condition = new ConditionImpl();
		assertThat(condition.getUsage(), nullValue());
	}

	@Test
	void conditionImplSetAndGetUsage() {
		ConditionImpl condition = new ConditionImpl();
		condition.setUsage(UsageType.both);
		assertThat(condition.getUsage(), is(UsageType.both));
	}

	@Test
	void conditionImplPropertiesNotNull() {
		ConditionImpl condition = new ConditionImpl();
		assertThat(condition.getProperties(), notNullValue());
	}

	@Test
	void conditionImplPropertiesCanBePopulated() {
		ConditionImpl condition = new ConditionImpl();
		condition.getProperties().put("key", "value");
		assertThat(condition.getProperties().get("key"), is("value"));
	}

	// --- UniqueConstraintImpl ---

	@Test
	void uniqueConstraintImplNameNullByDefault() {
		UniqueConstraintImpl constraint = new UniqueConstraintImpl();
		assertThat(constraint.getName(), nullValue());
	}

	@Test
	void uniqueConstraintImplSetAndGetName() {
		UniqueConstraintImpl constraint = new UniqueConstraintImpl();
		constraint.setName("uniqueEmail");
		assertThat(constraint.getName(), is("uniqueEmail"));
	}

	@Test
	void uniqueConstraintImplDescriptionNullByDefault() {
		UniqueConstraintImpl constraint = new UniqueConstraintImpl();
		assertThat(constraint.getDescription(), nullValue());
	}

	@Test
	void uniqueConstraintImplSetAndGetDescription() {
		UniqueConstraintImpl constraint = new UniqueConstraintImpl();
		constraint.setDescription("Email must be unique");
		assertThat(constraint.getDescription(), is("Email must be unique"));
	}

	@Test
	void uniqueConstraintImplMessageNullByDefault() {
		UniqueConstraintImpl constraint = new UniqueConstraintImpl();
		assertThat(constraint.getMessage(), nullValue());
	}

	@Test
	void uniqueConstraintImplSetAndGetMessage() {
		UniqueConstraintImpl constraint = new UniqueConstraintImpl();
		constraint.setMessage("This email already exists");
		assertThat(constraint.getMessage(), is("This email already exists"));
	}

	@Test
	void uniqueConstraintImplScopeNullByDefault() {
		UniqueConstraintImpl constraint = new UniqueConstraintImpl();
		assertThat(constraint.getScope(), nullValue());
	}

	@Test
	void uniqueConstraintImplSetAndGetScope() {
		UniqueConstraintImpl constraint = new UniqueConstraintImpl();
		constraint.setScope(UniqueConstraintImpl.DocumentScope.customer);
		assertThat(constraint.getScope(), is(UniqueConstraintImpl.DocumentScope.customer));
	}

	@Test
	void uniqueConstraintImplFieldNamesNotNull() {
		UniqueConstraintImpl constraint = new UniqueConstraintImpl();
		assertThat(constraint.getFieldNames(), notNullValue());
	}

	@Test
	void uniqueConstraintImplFieldNamesCanBePopulated() {
		UniqueConstraintImpl constraint = new UniqueConstraintImpl();
		constraint.getFieldNames().add("email");
		constraint.getFieldNames().add("username");
		assertThat(constraint.getFieldNames().size(), is(2));
		assertThat(constraint.getFieldNames().get(0), is("email"));
	}

	@Test
	void uniqueConstraintImplPropertiesNotNull() {
		UniqueConstraintImpl constraint = new UniqueConstraintImpl();
		assertThat(constraint.getProperties(), notNullValue());
	}

	// --- AssociationImpl ---

	@Test
	void associationImplTypeNullByDefault() {
		AssociationImpl assoc = new AssociationImpl();
		assertThat(assoc.getType(), nullValue());
	}

	@Test
	void associationImplSetAndGetType() {
		AssociationImpl assoc = new AssociationImpl();
		assoc.setType(AssociationType.aggregation);
		assertThat(assoc.getType(), is(AssociationType.aggregation));
	}

	@Test
	void associationImplCompositionType() {
		AssociationImpl assoc = new AssociationImpl();
		assoc.setType(AssociationType.composition);
		assertThat(assoc.getType(), is(AssociationType.composition));
	}

	@Test
	void associationImplRequiredDefaultIsFalse() {
		AssociationImpl assoc = new AssociationImpl();
		assertThat(assoc.isRequired(), is(false));
	}

	@Test
	void associationImplSetAndGetRequired() {
		AssociationImpl assoc = new AssociationImpl();
		assoc.setRequired(true);
		assertThat(assoc.isRequired(), is(true));
	}

	@Test
	void associationImplRequiredMessageNullByDefault() {
		AssociationImpl assoc = new AssociationImpl();
		assertThat(assoc.getRequiredMessage(), nullValue());
	}

	@Test
	void associationImplSetAndGetRequiredMessage() {
		AssociationImpl assoc = new AssociationImpl();
		assoc.setRequiredMessage("Association is required");
		assertThat(assoc.getRequiredMessage(), is("Association is required"));
	}

	@Test
	void associationImplEmbeddedColumnsPrefixNullByDefault() {
		AssociationImpl assoc = new AssociationImpl();
		assertThat(assoc.getEmbeddedColumnsPrefix(), nullValue());
	}

	@Test
	void associationImplSetAndGetEmbeddedColumnsPrefix() {
		AssociationImpl assoc = new AssociationImpl();
		assoc.setEmbeddedColumnsPrefix("emb_");
		assertThat(assoc.getEmbeddedColumnsPrefix(), is("emb_"));
	}

	// --- CollectionImpl ---

	@Test
	void collectionImplTypeNullByDefault() {
		CollectionImpl coll = new CollectionImpl();
		assertThat(coll.getType(), nullValue());
	}

	@Test
	void collectionImplSetAndGetType() {
		CollectionImpl coll = new CollectionImpl();
		coll.setType(CollectionType.composition);
		assertThat(coll.getType(), is(CollectionType.composition));
	}

	@Test
	void collectionImplAggregationType() {
		CollectionImpl coll = new CollectionImpl();
		coll.setType(CollectionType.aggregation);
		assertThat(coll.getType(), is(CollectionType.aggregation));
	}

	@Test
	void collectionImplOrderedNullByDefault() {
		CollectionImpl coll = new CollectionImpl();
		assertThat(coll.getOrdered(), nullValue());
	}

	@Test
	void collectionImplSetAndGetOrdered() {
		CollectionImpl coll = new CollectionImpl();
		coll.setOrdered(Boolean.TRUE);
		assertThat(coll.getOrdered(), is(Boolean.TRUE));
	}

	@Test
	void collectionImplMinCardinalityDefaultZero() {
		CollectionImpl coll = new CollectionImpl();
		assertThat(coll.getMinCardinality(), is(0));
	}

	@Test
	void collectionImplSetAndGetMinCardinality() {
		CollectionImpl coll = new CollectionImpl();
		coll.setMinCardinality(1);
		assertThat(coll.getMinCardinality(), is(1));
	}

	@Test
	void collectionImplMaxCardinalityNullByDefault() {
		CollectionImpl coll = new CollectionImpl();
		assertThat(coll.getMaxCardinality(), nullValue());
	}

	@Test
	void collectionImplSetAndGetMaxCardinality() {
		CollectionImpl coll = new CollectionImpl();
		coll.setMaxCardinality(Integer.valueOf(10));
		assertThat(coll.getMaxCardinality(), is(Integer.valueOf(10)));
	}

	@Test
	void collectionImplOrderingNotNull() {
		CollectionImpl coll = new CollectionImpl();
		assertThat(coll.getOrdering(), notNullValue());
	}

	@Test
	void collectionImplUniqueConstraintsNotNull() {
		CollectionImpl coll = new CollectionImpl();
		assertThat(coll.getUniqueConstraints(), notNullValue());
	}

	@Test
	void collectionImplCacheNameNullByDefault() {
		CollectionImpl coll = new CollectionImpl();
		assertThat(coll.getCacheName(), nullValue());
	}

	@Test
	void collectionImplSetAndGetCacheName() {
		CollectionImpl coll = new CollectionImpl();
		coll.setCacheName("myCache");
		assertThat(coll.getCacheName(), is("myCache"));
	}

	// --- DocumentImpl simple getters/setters ---

	@Test
	void documentImplLastModifiedMillisRoundtrips() {
		DocumentImpl doc = new DocumentImpl();
		doc.setLastModifiedMillis(99999L);
		assertThat(doc.getLastModifiedMillis(), is(99999L));
	}

	@Test
	void documentImplLastCheckedMillisRoundtrips() {
		DocumentImpl doc = new DocumentImpl();
		doc.setLastCheckedMillis(12345L);
		assertThat(doc.getLastCheckedMillis(), is(12345L));
	}

	@Test
	void documentImplGetUniqueConstraintReturnsNullForUnknown() {
		DocumentImpl doc = new DocumentImpl();
		assertThat(doc.getUniqueConstraint("nonExistent"), nullValue());
	}

	@Test
	void documentImplGetUniqueConstraintsIsEmptyByDefault() {
		DocumentImpl doc = new DocumentImpl();
		assertThat(doc.getUniqueConstraints().isEmpty(), is(true));
	}

	@Test
	void documentImplGetReferenceByNameReturnsNullForUnknown() {
		DocumentImpl doc = new DocumentImpl();
		assertThat(doc.getReferenceByName("nonExistent"), nullValue());
	}

	@Test
	void documentImplParentDocumentNameNullByDefault() {
		DocumentImpl doc = new DocumentImpl();
		assertThat(doc.getParentDocumentName(), nullValue());
	}

	@Test
	void documentImplSetAndGetParentDocumentName() {
		DocumentImpl doc = new DocumentImpl();
		doc.setParentDocumentName("ParentDoc");
		assertThat(doc.getParentDocumentName(), is("ParentDoc"));
	}

	@Test
	void documentImplParentDatabaseIndexNullByDefault() {
		DocumentImpl doc = new DocumentImpl();
		assertThat(doc.getParentDatabaseIndex(), nullValue());
	}

	@Test
	void documentImplSetAndGetParentDatabaseIndex() {
		DocumentImpl doc = new DocumentImpl();
		doc.setParentDatabaseIndex(Boolean.TRUE);
		assertThat(doc.getParentDatabaseIndex(), is(Boolean.TRUE));
	}

	@Test
	void documentImplBizKeyMethodCodeNullByDefault() {
		DocumentImpl doc = new DocumentImpl();
		assertThat(doc.getBizKeyMethodCode(), nullValue());
	}

	@Test
	void documentImplSetAndGetBizKeyMethodCode() {
		DocumentImpl doc = new DocumentImpl();
		doc.setBizKeyMethodCode("return getName();");
		assertThat(doc.getBizKeyMethodCode(), is("return getName();"));
	}

	@Test
	void documentImplBizKeyExpressionNullByDefault() {
		DocumentImpl doc = new DocumentImpl();
		assertThat(doc.getBizKeyExpression(), nullValue());
	}

	@Test
	void documentImplSetAndGetBizKeyExpression() {
		DocumentImpl doc = new DocumentImpl();
		doc.setBizKeyExpression("{name}");
		assertThat(doc.getBizKeyExpression(), is("{name}"));
	}

	@Test
	void documentImplOrderedDefaultIsFalse() {
		DocumentImpl doc = new DocumentImpl();
		assertThat(doc.isOrdered(), is(false));
	}

	@Test
	void documentImplSetOrdered() {
		DocumentImpl doc = new DocumentImpl();
		doc.setOrdered(true);
		assertThat(doc.isOrdered(), is(true));
	}

	@Test
	void documentImplGetConditionsIsEmptyByDefault() {
		DocumentImpl doc = new DocumentImpl();
		assertThat(doc.getConditions().isEmpty(), is(true));
	}

	@Test
	void documentImplGetDocumentationNullByDefault() {
		DocumentImpl doc = new DocumentImpl();
		assertThat(doc.getDocumentation(), nullValue());
	}

	@Test
	void documentImplSetAndGetDocumentation() {
		DocumentImpl doc = new DocumentImpl();
		doc.setDocumentation("Test docs");
		assertThat(doc.getDocumentation(), is("Test docs"));
	}

	@Test
	void documentImplGetPropertiesNotNull() {
		DocumentImpl doc = new DocumentImpl();
		assertThat(doc.getProperties(), notNullValue());
		assertThat(doc.getProperties().isEmpty(), is(true));
	}

	@Test
	void documentImplGetReferenceNamesEmptyByDefault() {
		DocumentImpl doc = new DocumentImpl();
		assertThat(doc.getReferenceNames().isEmpty(), is(true));
	}

	@Test
	void documentImplGetDefinedActionNamesEmptyByDefault() {
		DocumentImpl doc = new DocumentImpl();
		assertThat(doc.getDefinedActionNames().isEmpty(), is(true));
	}

	@Test
	void documentImplGetConditionNamesEmptyByDefault() {
		DocumentImpl doc = new DocumentImpl();
		assertThat(doc.getConditionNames().isEmpty(), is(true));
	}

	@Test
	void documentImplGetConditionReturnsNullForUnknown() {
		DocumentImpl doc = new DocumentImpl();
		assertThat(doc.getCondition("nonExistent"), nullValue());
	}

        // --- DocumentImpl put/get methods ---

        @Test
        void documentImplPutAndGetUniqueConstraint() {
                DocumentImpl doc = new DocumentImpl();
                UniqueConstraintImpl constraint = new UniqueConstraintImpl();
                constraint.setName("myConstraint");
                doc.putUniqueConstraint(constraint);
                assertThat(doc.getUniqueConstraint("myConstraint"), is(constraint));
        }

        @Test
        void documentImplPutUniqueConstraintAppearsInList() {
                DocumentImpl doc = new DocumentImpl();
                UniqueConstraintImpl constraint = new UniqueConstraintImpl();
                constraint.setName("uc1");
                doc.putUniqueConstraint(constraint);
                assertThat(doc.getUniqueConstraints().size(), is(1));
                assertThat(doc.getUniqueConstraints().get(0), is(constraint));
        }

        @Test
        void documentImplPutRelationAddsToReferenceNames() {
                DocumentImpl doc = new DocumentImpl();
                AssociationImpl assoc = new AssociationImpl();
                assoc.setName("relatedDoc");
                assoc.setDocumentName("SomeDoc");
                doc.putRelation(assoc);
                assertThat(doc.getReferenceNames().contains("relatedDoc"), is(true));
        }

        @Test
        void documentImplGetReferenceByNameAfterPut() {
                DocumentImpl doc = new DocumentImpl();
                AssociationImpl assoc = new AssociationImpl();
                assoc.setName("myRef");
                assoc.setDocumentName("Other");
                doc.putRelation(assoc);
                assertThat(doc.getReferenceByName("myRef"), is(assoc));
        }

        @Test
        void documentImplBizKeySensitityNullByDefault() {
                DocumentImpl doc = new DocumentImpl();
                assertThat(doc.getBizKeySensitity(), nullValue());
        }

        @Test
        void documentImplSetAndGetBizKeySensitity() {
                DocumentImpl doc = new DocumentImpl();
                doc.setBizKeySensitity(Sensitivity.restricted);
                assertThat(doc.getBizKeySensitity(), is(Sensitivity.restricted));
        }

        @Test
        void documentImplDefinedActionNamesCanBePopulated() {
                DocumentImpl doc = new DocumentImpl();
                doc.getDefinedActionNames().add("MyAction");
                assertThat(doc.getDefinedActionNames().contains("MyAction"), is(true));
                assertThat(doc.getDefinedActionNames().size(), is(1));
        }

        @Test
        void documentImplConditionNamesAfterPut() {
                DocumentImpl doc = new DocumentImpl();
                ConditionImpl cond = new ConditionImpl();
                doc.getConditions().put("isActive", cond);
                assertThat(doc.getConditionNames().contains("isActive"), is(true));
        }

	@Test
	void documentImplGetConditionAfterPut() {
		DocumentImpl doc = new DocumentImpl();
		ConditionImpl cond = new ConditionImpl();
		doc.getConditions().put("myCondition", cond);
		assertThat(doc.getCondition("myCondition"), is(cond));
	}

	@Test
	void documentImplGetBizKeyAttributeNotNull() {
		assertThat(DocumentImpl.getBizKeyAttribute(), notNullValue());
	}

	@Test
	void documentImplGetBizOrdinalAttributeNotNull() {
		assertThat(DocumentImpl.getBizOrdinalAttribute(), notNullValue());
	}
}
