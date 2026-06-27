package org.skyve.impl.metadata.repository.document;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;
import org.skyve.metadata.model.Attribute.Sensitivity;
import org.skyve.metadata.model.Attribute.UsageType;
import org.skyve.metadata.model.document.UniqueConstraint.DocumentScope;

@SuppressWarnings("static-method")
class DocumentRepositoryMetaDataTest {

	// ConditionMetaData

	@Test
	void conditionNameRoundTrips() {
		ConditionMetaData c = new ConditionMetaData();
		c.setName("myCondition");
		assertThat(c.getName(), is("myCondition"));
	}

	@Test
	void conditionDescriptionRoundTrips() {
		ConditionMetaData c = new ConditionMetaData();
		c.setDescription("some description");
		assertThat(c.getDescription(), is("some description"));
	}

	@Test
	void conditionDocumentationRoundTrips() {
		ConditionMetaData c = new ConditionMetaData();
		c.setDocumentation("some docs");
		assertThat(c.getDocumentation(), is("some docs"));
	}

	@Test
	void conditionExpressionRoundTrips() {
		ConditionMetaData c = new ConditionMetaData();
		c.setExpression("bean.active");
		assertThat(c.getExpression(), is("bean.active"));
	}

	@Test
	void conditionUsageRoundTrips() {
		ConditionMetaData c = new ConditionMetaData();
		c.setUsage(UsageType.both);
		assertThat(c.getUsage(), is(UsageType.both));
	}

	@Test
	void conditionUsageViewOnly() {
		ConditionMetaData c = new ConditionMetaData();
		c.setUsage(UsageType.view);
		assertThat(c.getUsage(), is(UsageType.view));
	}

	@Test
	void conditionPropertiesNotNull() {
		ConditionMetaData c = new ConditionMetaData();
		assertThat(c.getProperties(), notNullValue());
	}

	@Test
	void conditionPropertiesCanBePopulated() {
		ConditionMetaData c = new ConditionMetaData();
		c.getProperties().put("key", "value");
		assertThat(c.getProperties().get("key"), is("value"));
	}

	@Test
	void conditionBlankNameBecomesNull() {
		ConditionMetaData c = new ConditionMetaData();
		c.setName("  ");
		assertThat(c.getName(), nullValue());
	}

	// UniqueConstraint

	@Test
	void uniqueConstraintNameRoundTrips() {
		UniqueConstraint uc = new UniqueConstraint();
		uc.setName("uc1");
		assertThat(uc.getName(), is("uc1"));
	}

	@Test
	void uniqueConstraintDescriptionRoundTrips() {
		UniqueConstraint uc = new UniqueConstraint();
		uc.setDescription("no duplicates");
		assertThat(uc.getDescription(), is("no duplicates"));
	}

	@Test
	void uniqueConstraintScopeRoundTrips() {
		UniqueConstraint uc = new UniqueConstraint();
		uc.setScope(DocumentScope.customer);
		assertThat(uc.getScope(), is(DocumentScope.customer));
	}

	@Test
	void uniqueConstraintMessageRoundTrips() {
		UniqueConstraint uc = new UniqueConstraint();
		uc.setMessage("must be unique");
		assertThat(uc.getMessage(), is("must be unique"));
	}

	@Test
	void uniqueConstraintFieldReferencesNotNull() {
		UniqueConstraint uc = new UniqueConstraint();
		assertThat(uc.getFieldReferences(), notNullValue());
	}

	@Test
	void uniqueConstraintFieldReferencesCanBePopulated() {
		UniqueConstraint uc = new UniqueConstraint();
		FieldReference fr = new FieldReference();
		fr.setRef("myField");
		uc.getFieldReferences().add(fr);
		assertEquals(1, uc.getFieldReferences().size());
		assertThat(uc.getFieldReferences().get(0).getRef(), is("myField"));
	}

	@Test
	void uniqueConstraintPropertiesNotNull() {
		UniqueConstraint uc = new UniqueConstraint();
		assertThat(uc.getProperties(), notNullValue());
	}

	// BizKey

	@Test
	void bizKeyExpressionRoundTrips() {
		BizKey bk = new BizKey();
		bk.setExpression("{name}");
		assertThat(bk.getExpression(), is("{name}"));
	}

	@Test
	void bizKeyCodeRoundTrips() {
		BizKey bk = new BizKey();
		bk.setCode("return bean.getName();");
		assertThat(bk.getCode(), is("return bean.getName();"));
	}

	@Test
	void bizKeySensitivityRoundTrips() {
		BizKey bk = new BizKey();
		bk.setSensitivity(Sensitivity.internal);
		assertThat(bk.getSensitivity(), is(Sensitivity.internal));
	}

	@Test
	void bizKeyDefaultsAreNull() {
		BizKey bk = new BizKey();
		assertThat(bk.getExpression(), nullValue());
		assertThat(bk.getCode(), nullValue());
		assertThat(bk.getSensitivity(), nullValue());
	}

	// ParentDocument

	@Test
	void parentDocumentNameRoundTrips() {
		ParentDocument pd = new ParentDocument();
		pd.setParentDocumentName("Contact");
		assertThat(pd.getParentDocumentName(), is("Contact"));
	}

	@Test
	void parentDocumentDatabaseIndexRoundTrips() {
		ParentDocument pd = new ParentDocument();
		pd.setDatabaseIndex(Boolean.TRUE);
		assertThat(pd.getDatabaseIndex(), is(Boolean.TRUE));
	}

	@Test
	void parentDocumentDatabaseIndexDefaultIsNull() {
		ParentDocument pd = new ParentDocument();
		assertThat(pd.getDatabaseIndex(), nullValue());
	}

	// FieldReference

	@Test
	void fieldReferenceRefRoundTrips() {
		FieldReference fr = new FieldReference();
		fr.setRef("someField");
		assertThat(fr.getRef(), is("someField"));
	}

	@Test
	void fieldReferenceBlankRefBecomesNull() {
		FieldReference fr = new FieldReference();
		fr.setRef("  ");
		assertThat(fr.getRef(), nullValue());
	}
}
