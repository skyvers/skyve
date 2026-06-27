package org.skyve.metadata.model.document.fluent;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.model.document.field.Enumeration;

@SuppressWarnings("static-method")
class FluentEnumerationTest {

	@Test
	void defaultConstructorCreatesInstance() {
		assertThat(new FluentEnumeration().get(), is(notNullValue()));
	}

	@Test
	void wrappingConstructorPreservesInstance() {
		Enumeration e = new Enumeration();
		assertThat(new FluentEnumeration(e).get(), is(e));
	}

	@Test
	void typeNameSetsValue() {
		assertThat(new FluentEnumeration().typeName("MyEnum").get().getTypeName(), is("MyEnum"));
	}

	@Test
	void implementingEnumClassNameSetsValue() {
		assertThat(new FluentEnumeration().implementingEnumClassName("com.Foo").get().getImplementingEnumClassName(), is("com.Foo"));
	}

	@Test
	void moduleRefSetsValue() {
		assertThat(new FluentEnumeration().moduleRef("admin").get().getModuleRef(), is("admin"));
	}

	@Test
	void documentRefSetsValue() {
		assertThat(new FluentEnumeration().documentRef("Contact").get().getDocumentRef(), is("Contact"));
	}

	@Test
	void attributeRefSetsValue() {
		assertThat(new FluentEnumeration().attributeRef("status").get().getAttributeRef(), is("status"));
	}

	@Test
	void addValueAddsEntry() {
		FluentEnumeration fe = new FluentEnumeration().addValue(new FluentEnumeratedValue().name("Active"));
		assertEquals(1, fe.get().getXmlValues().size());
	}

	@Test
	void removeValueByNameRemovesEntry() {
		FluentEnumeration fe = new FluentEnumeration()
				.addValue(new FluentEnumeratedValue().name("Active"))
				.removeValueByName("Active");
		assertEquals(0, fe.get().getXmlValues().size());
	}

	@Test
	void findValueByNameReturnsMatch() {
		FluentEnumeration fe = new FluentEnumeration().addValue(new FluentEnumeratedValue().name("Active"));
		assertThat(fe.findValueByName("Active"), is(notNullValue()));
	}

	@Test
	void findValueByNameReturnsNullWhenMissing() {
		assertThat(new FluentEnumeration().findValueByName("Missing"), is(nullValue()));
	}

	@Test
	void removeValueByCodeRemovesEntry() {
		FluentEnumeration fe = new FluentEnumeration()
				.addValue(new FluentEnumeratedValue().code("ACT"))
				.removeValueByCode("ACT");
		assertEquals(0, fe.get().getXmlValues().size());
	}

	@Test
	void findValueByCodeReturnsMatch() {
		FluentEnumeration fe = new FluentEnumeration().addValue(new FluentEnumeratedValue().code("ACT"));
		assertThat(fe.findValueByCode("ACT"), is(notNullValue()));
	}

	@Test
	void findValueByCodeReturnsNullWhenMissing() {
		assertThat(new FluentEnumeration().findValueByCode("MISSING"), is(nullValue()));
	}

	@Test
	void removeValueByDescriptionRemovesEntry() {
		FluentEnumeration fe = new FluentEnumeration()
				.addValue(new FluentEnumeratedValue().description("Active"))
				.removeValueByDescription("Active");
		assertEquals(0, fe.get().getXmlValues().size());
	}

	@Test
	void findValueByDescriptionReturnsMatch() {
		FluentEnumeration fe = new FluentEnumeration().addValue(new FluentEnumeratedValue().description("Active"));
		assertThat(fe.findValueByDescription("Active"), is(notNullValue()));
	}

	@Test
	void findValueByDescriptionReturnsNullWhenMissing() {
		assertThat(new FluentEnumeration().findValueByDescription("Missing"), is(nullValue()));
	}

	@Test
	void clearValuesRemovesAll() {
		FluentEnumeration fe = new FluentEnumeration()
				.addValue(new FluentEnumeratedValue().name("A"))
				.addValue(new FluentEnumeratedValue().name("B"))
				.clearValues();
		assertEquals(0, fe.get().getXmlValues().size());
	}

	@Test
	void fromCopiesTypeName() {
		Enumeration src = new Enumeration();
		src.setXmlTypeName("StatusEnum");
		assertThat(new FluentEnumeration().from(src).get().getTypeName(), is("StatusEnum"));
	}

	@Test
	void fromCopiesValues() {
		Enumeration src = new Enumeration();
		src.getXmlValues().add(new FluentEnumeratedValue().name("Active").get());
		assertEquals(1, new FluentEnumeration().from(src).get().getXmlValues().size());
	}
}
