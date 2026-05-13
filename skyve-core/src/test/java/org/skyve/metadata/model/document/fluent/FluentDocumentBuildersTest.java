package org.skyve.metadata.model.document.fluent;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;

import org.junit.jupiter.api.Test;
import org.skyve.metadata.model.Attribute.UsageType;

/** Unit tests for fluent document builder classes. */
@SuppressWarnings("static-method")
class FluentDocumentBuildersTest {

	// ---- FluentDocument ----

	@Test
	void documentDefaultConstructorCreatesInstance() {
		FluentDocument d = new FluentDocument();
		assertThat(d.get(), is(notNullValue()));
	}

	@Test
	void documentNameSetsValue() {
		FluentDocument d = new FluentDocument();
		d.name("Contact");
		assertThat(d.get().getName(), is("Contact"));
	}

	@Test
	void documentAbstractDocumentSetsTrue() {
		FluentDocument d = new FluentDocument();
		d.abstractDocument(true);
		assertThat(d.get().getAbstract(), is(Boolean.TRUE));
	}

	@Test
	void documentAbstractDocumentSetsFalse() {
		FluentDocument d = new FluentDocument();
		d.abstractDocument(false);
		assertThat(d.get().getAbstract(), is(Boolean.FALSE));
	}

	@Test
	void documentDocumentationSetsValue() {
		FluentDocument d = new FluentDocument();
		d.documentation("Some docs");
		assertThat(d.get().getDocumentation(), is("Some docs"));
	}

	@Test
	void documentSingularAliasSetsValue() {
		FluentDocument d = new FluentDocument();
		d.singularAlias("Contact");
		assertThat(d.get().getSingularAlias(), is("Contact"));
	}

	@Test
	void documentPluralAliasSetsValue() {
		FluentDocument d = new FluentDocument();
		d.pluralAlias("Contacts");
		assertThat(d.get().getPluralAlias(), is("Contacts"));
	}

	@Test
	void documentDescriptionSetsValue() {
		FluentDocument d = new FluentDocument();
		d.description("A contact");
		assertThat(d.get().getDescription(), is("A contact"));
	}

	@Test
	void documentIconStyleClassSetsValue() {
		FluentDocument d = new FluentDocument();
		d.iconStyleClass("fa fa-user");
		assertThat(d.get().getIconStyleClass(), is("fa fa-user"));
	}

	@Test
	void documentIcon16x16RelativeFilePathSetsValue() {
		FluentDocument d = new FluentDocument();
		d.icon16x16RelativeFilePath("icons/contact16.png");
		assertThat(d.get().getIcon16x16RelativeFilePath(), is("icons/contact16.png"));
	}

	@Test
	void documentIcon32x32RelativeFilePathSetsValue() {
		FluentDocument d = new FluentDocument();
		d.icon32x32RelativeFilePath("icons/contact32.png");
		assertThat(d.get().getIcon32x32RelativeFilePath(), is("icons/contact32.png"));
	}

	@Test
	void documentAuditedSetsFalse() {
		FluentDocument d = new FluentDocument();
		d.audited(false);
		assertThat(d.get().getAudited(), is(Boolean.FALSE));
	}

	@Test
	void documentAuditedSetsTrue() {
		FluentDocument d = new FluentDocument();
		d.audited(true);
		assertThat(d.get().getAudited(), is(Boolean.TRUE));
	}

	@Test
	void documentAddTextAddsAttribute() {
		FluentDocument d = new FluentDocument();
		d.addText(new FluentText().name("name").length(100));
		assertThat(d.get().getAttributes().size(), is(1));
	}

	@Test
	void documentAddIntegerAddsAttribute() {
		FluentDocument d = new FluentDocument();
		d.addInteger(new FluentInteger().name("count"));
		assertThat(d.get().getAttributes().size(), is(1));
	}

	@Test
	void documentAddBooleanAddsAttribute() {
		FluentDocument d = new FluentDocument();
		d.addBoolean(new FluentBoolean().name("active"));
		assertThat(d.get().getAttributes().size(), is(1));
	}

	@Test
	void documentAddDecimal2AddsAttribute() {
		FluentDocument d = new FluentDocument();
		d.addDecimal2(new FluentDecimal2().name("amount"));
		assertThat(d.get().getAttributes().size(), is(1));
	}

	@Test
	void documentAddDecimal5AddsAttribute() {
		FluentDocument d = new FluentDocument();
		d.addDecimal5(new FluentDecimal5().name("ratio"));
		assertThat(d.get().getAttributes().size(), is(1));
	}

	@Test
	void documentAddDecimal10AddsAttribute() {
		FluentDocument d = new FluentDocument();
		d.addDecimal10(new FluentDecimal10().name("precise"));
		assertThat(d.get().getAttributes().size(), is(1));
	}

	@Test
	void documentAddDateAddsAttribute() {
		FluentDocument d = new FluentDocument();
		d.addDate(new FluentDate().name("birthDate"));
		assertThat(d.get().getAttributes().size(), is(1));
	}

	@Test
	void documentAddTimeAddsAttribute() {
		FluentDocument d = new FluentDocument();
		d.addTime(new FluentTime().name("startTime"));
		assertThat(d.get().getAttributes().size(), is(1));
	}

	@Test
	void documentAddDateTimeAddsAttribute() {
		FluentDocument d = new FluentDocument();
		d.addDateTime(new FluentDateTime().name("createdAt"));
		assertThat(d.get().getAttributes().size(), is(1));
	}

	@Test
	void documentAddTimestampAddsAttribute() {
		FluentDocument d = new FluentDocument();
		d.addTimestamp(new FluentTimestamp().name("modifiedAt"));
		assertThat(d.get().getAttributes().size(), is(1));
	}

	@Test
	void documentAddLongIntegerAddsAttribute() {
		FluentDocument d = new FluentDocument();
		d.addLongInteger(new FluentLongInteger().name("bigNum"));
		assertThat(d.get().getAttributes().size(), is(1));
	}

	@Test
	void documentAddMemoAddsAttribute() {
		FluentDocument d = new FluentDocument();
		d.addMemo(new FluentMemo().name("notes"));
		assertThat(d.get().getAttributes().size(), is(1));
	}

	@Test
	void documentAddMarkupAddsAttribute() {
		FluentDocument d = new FluentDocument();
		d.addMarkup(new FluentMarkup().name("body"));
		assertThat(d.get().getAttributes().size(), is(1));
	}

	@Test
	void documentAddConditionAddsCondition() {
		FluentDocument d = new FluentDocument();
		d.addCondition(new FluentCondition().name("isActive").expression("active"));
		assertThat(d.get().getConditions().size(), is(1));
	}

	// ---- FluentCondition ----

	@Test
	void conditionDefaultConstructorCreatesInstance() {
		FluentCondition c = new FluentCondition();
		assertThat(c.get(), is(notNullValue()));
	}

	@Test
	void conditionNameSetsValue() {
		FluentCondition c = new FluentCondition();
		c.name("isActive");
		assertThat(c.get().getName(), is("isActive"));
	}

	@Test
	void conditionExpressionSetsValue() {
		FluentCondition c = new FluentCondition();
		c.expression("active == true");
		assertThat(c.get().getExpression(), is("active == true"));
	}

	@Test
	void conditionDocumentationSetsValue() {
		FluentCondition c = new FluentCondition();
		c.documentation("Condition docs");
		assertThat(c.get().getDocumentation(), is("Condition docs"));
	}

	@Test
	void conditionDescriptionSetsValue() {
		FluentCondition c = new FluentCondition();
		c.description("Active condition");
		assertThat(c.get().getDescription(), is("Active condition"));
	}

	@Test
	void conditionUsageSetsValue() {
		FluentCondition c = new FluentCondition();
		c.usage(UsageType.view);
		assertThat(c.get().getUsage(), is(UsageType.view));
	}

	// ---- FluentText ----

	@Test
	void textDefaultConstructorCreatesInstance() {
		FluentText t = new FluentText();
		assertThat(t.get(), is(notNullValue()));
	}

	@Test
	void textNameSetsValue() {
		FluentText t = new FluentText();
		t.name("firstName");
		assertThat(t.get().getName(), is("firstName"));
	}

	@Test
	void textLengthSetsValue() {
		FluentText t = new FluentText();
		t.length(100);
		assertThat(t.get().getLength(), is(100));
	}

	@Test
	void textDisplayNameSetsValue() {
		FluentText t = new FluentText();
		t.displayName("First Name");
		assertThat(t.get().getDisplayName(), is("First Name"));
	}

	// ---- FluentInteger ----

	@Test
	void integerDefaultConstructorCreatesInstance() {
		FluentInteger i = new FluentInteger();
		assertThat(i.get(), is(notNullValue()));
	}

	@Test
	void integerNameSetsValue() {
		FluentInteger i = new FluentInteger();
		i.name("count");
		assertThat(i.get().getName(), is("count"));
	}

	// ---- FluentBoolean ----

	@Test
	void booleanDefaultConstructorCreatesInstance() {
		FluentBoolean b = new FluentBoolean();
		assertThat(b.get(), is(notNullValue()));
	}

	@Test
	void booleanNameSetsValue() {
		FluentBoolean b = new FluentBoolean();
		b.name("active");
		assertThat(b.get().getName(), is("active"));
	}

	// ---- FluentDecimal2 ----

	@Test
	void decimal2DefaultConstructorCreatesInstance() {
		FluentDecimal2 d = new FluentDecimal2();
		assertThat(d.get(), is(notNullValue()));
	}

	@Test
	void decimal2NameSetsValue() {
		FluentDecimal2 d = new FluentDecimal2();
		d.name("amount");
		assertThat(d.get().getName(), is("amount"));
	}

	// ---- FluentDecimal5 ----

	@Test
	void decimal5DefaultConstructorCreatesInstance() {
		FluentDecimal5 d = new FluentDecimal5();
		assertThat(d.get(), is(notNullValue()));
	}

	@Test
	void decimal5NameSetsValue() {
		FluentDecimal5 d = new FluentDecimal5();
		d.name("ratio");
		assertThat(d.get().getName(), is("ratio"));
	}

	// ---- FluentDecimal10 ----

	@Test
	void decimal10DefaultConstructorCreatesInstance() {
		FluentDecimal10 d = new FluentDecimal10();
		assertThat(d.get(), is(notNullValue()));
	}

	@Test
	void decimal10NameSetsValue() {
		FluentDecimal10 d = new FluentDecimal10();
		d.name("precise");
		assertThat(d.get().getName(), is("precise"));
	}

	// ---- FluentDate ----

	@Test
	void dateDefaultConstructorCreatesInstance() {
		FluentDate d = new FluentDate();
		assertThat(d.get(), is(notNullValue()));
	}

	@Test
	void dateNameSetsValue() {
		FluentDate d = new FluentDate();
		d.name("birthDate");
		assertThat(d.get().getName(), is("birthDate"));
	}

	// ---- FluentTime ----

	@Test
	void timeDefaultConstructorCreatesInstance() {
		FluentTime t = new FluentTime();
		assertThat(t.get(), is(notNullValue()));
	}

	@Test
	void timeNameSetsValue() {
		FluentTime t = new FluentTime();
		t.name("startTime");
		assertThat(t.get().getName(), is("startTime"));
	}

	// ---- FluentDateTime ----

	@Test
	void dateTimeDefaultConstructorCreatesInstance() {
		FluentDateTime dt = new FluentDateTime();
		assertThat(dt.get(), is(notNullValue()));
	}

	@Test
	void dateTimeNameSetsValue() {
		FluentDateTime dt = new FluentDateTime();
		dt.name("createdAt");
		assertThat(dt.get().getName(), is("createdAt"));
	}

	// ---- FluentTimestamp ----

	@Test
	void timestampDefaultConstructorCreatesInstance() {
		FluentTimestamp ts = new FluentTimestamp();
		assertThat(ts.get(), is(notNullValue()));
	}

	@Test
	void timestampNameSetsValue() {
		FluentTimestamp ts = new FluentTimestamp();
		ts.name("modifiedAt");
		assertThat(ts.get().getName(), is("modifiedAt"));
	}

	// ---- FluentLongInteger ----

	@Test
	void longIntegerDefaultConstructorCreatesInstance() {
		FluentLongInteger li = new FluentLongInteger();
		assertThat(li.get(), is(notNullValue()));
	}

	@Test
	void longIntegerNameSetsValue() {
		FluentLongInteger li = new FluentLongInteger();
		li.name("bigNum");
		assertThat(li.get().getName(), is("bigNum"));
	}

	// ---- FluentMemo ----

	@Test
	void memoDefaultConstructorCreatesInstance() {
		FluentMemo m = new FluentMemo();
		assertThat(m.get(), is(notNullValue()));
	}

	@Test
	void memoNameSetsValue() {
		FluentMemo m = new FluentMemo();
		m.name("notes");
		assertThat(m.get().getName(), is("notes"));
	}

	// ---- FluentMarkup ----

	@Test
	void markupDefaultConstructorCreatesInstance() {
		FluentMarkup m = new FluentMarkup();
		assertThat(m.get(), is(notNullValue()));
	}

	@Test
	void markupNameSetsValue() {
		FluentMarkup m = new FluentMarkup();
		m.name("body");
		assertThat(m.get().getName(), is("body"));
	}
}
