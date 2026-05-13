package org.skyve.impl.metadata.model.document;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;

import org.junit.jupiter.api.Test;
import org.skyve.metadata.model.Attribute.UsageType;
import org.skyve.metadata.model.document.Association.AssociationType;
import org.skyve.metadata.model.document.Collection.CollectionType;

@SuppressWarnings("static-method")
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
}
