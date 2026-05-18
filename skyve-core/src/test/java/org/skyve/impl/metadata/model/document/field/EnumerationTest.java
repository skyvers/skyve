package org.skyve.impl.metadata.model.document.field;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.model.document.field.Enumeration.EnumeratedValue;

class EnumerationTest {

	// ---- Enumeration constructor and type ----

	@Test
	@SuppressWarnings("static-method")
	void defaultConstructorSetsAttributeTypeToEnumeration() {
		Enumeration e = new Enumeration();
		assertThat(e.getAttributeType(), is(org.skyve.metadata.model.Attribute.AttributeType.enumeration));
	}

	// ---- moduleRef / documentRef / attributeRef ----

	@Test
	@SuppressWarnings("static-method")
	void setModuleRefAndGet() {
		Enumeration e = new Enumeration();
		e.setModuleRef("admin");
		assertThat(e.getModuleRef(), is("admin"));
	}

	@Test
	@SuppressWarnings("static-method")
	void setModuleRefBlankBecomesNull() {
		Enumeration e = new Enumeration();
		e.setModuleRef("  ");
		assertNull(e.getModuleRef());
	}

	@Test
	@SuppressWarnings("static-method")
	void setDocumentRefAndGet() {
		Enumeration e = new Enumeration();
		e.setDocumentRef("User");
		assertThat(e.getDocumentRef(), is("User"));
	}

	@Test
	@SuppressWarnings("static-method")
	void setDocumentRefBlankBecomesNull() {
		Enumeration e = new Enumeration();
		e.setDocumentRef("  ");
		assertNull(e.getDocumentRef());
	}

	@Test
	@SuppressWarnings("static-method")
	void setAttributeRefAndGet() {
		Enumeration e = new Enumeration();
		e.setAttributeRef("status");
		assertThat(e.getAttributeRef(), is("status"));
	}

	@Test
	@SuppressWarnings("static-method")
	void setAttributeRefBlankBecomesNull() {
		Enumeration e = new Enumeration();
		e.setAttributeRef("  ");
		assertNull(e.getAttributeRef());
	}

	// ---- xmlTypeName / xmlImplementingEnumClassName ----

	@Test
	@SuppressWarnings("static-method")
	void setXmlTypeNameAndGetXmlTypeName() {
		Enumeration e = new Enumeration();
		e.setXmlTypeName("MyEnum");
		assertThat(e.getXmlTypeName(), is("MyEnum"));
	}

	@Test
	@SuppressWarnings("static-method")
	void setXmlImplementingEnumClassNameAndGet() {
		Enumeration e = new Enumeration();
		e.setXmlImplementingEnumClassName("com.example.MyEnum");
		assertThat(e.getXmlImplementingEnumClassName(), is("com.example.MyEnum"));
	}

	// ---- getTarget returns self when attributeRef is null ----

	@Test
	@SuppressWarnings("static-method")
	void getTargetReturnsSelfWhenAttributeRefIsNull() {
		Enumeration e = new Enumeration();
		assertThat(e.getTarget(), is(e));
	}

	// ---- getXmlValues / values list ----

	@Test
	@SuppressWarnings("static-method")
	void getXmlValuesInitiallyEmpty() {
		Enumeration e = new Enumeration();
		assertNotNull(e.getXmlValues());
		assertEquals(0, e.getXmlValues().size());
	}

	@Test
	@SuppressWarnings("static-method")
	void getXmlValuesCanAddEntries() {
		Enumeration e = new Enumeration();
		EnumeratedValue val = new EnumeratedValue();
		val.setCode("ACTIVE");
		e.getXmlValues().add(val);
		assertEquals(1, e.getXmlValues().size());
	}

	// ---- isDynamic (from Field.isDynamic) ----

	@Test
	@SuppressWarnings("static-method")
	void isDynamicFalseByDefault() {
		Enumeration e = new Enumeration();
		assertFalse(e.isDynamic());
	}

	@Test
	@SuppressWarnings("static-method")
	void isDynamicTrueWhenSetDynamic() {
		Enumeration e = new Enumeration();
		e.setDynamic(true);
		assertTrue(e.isDynamic());
	}

	// ---- EnumeratedValue getters/setters ----

	@Test
	@SuppressWarnings("static-method")
	void enumeratedValueSetNameAndGet() {
		EnumeratedValue val = new EnumeratedValue();
		val.setName("active");
		assertThat(val.getName(), is("active"));
	}

	@Test
	@SuppressWarnings("static-method")
	void enumeratedValueSetNameBlankBecomesNull() {
		EnumeratedValue val = new EnumeratedValue();
		val.setName("  ");
		assertNull(val.getName());
	}

	@Test
	@SuppressWarnings("static-method")
	void enumeratedValueSetCodeAndGet() {
		EnumeratedValue val = new EnumeratedValue();
		val.setCode("ACTIVE");
		assertThat(val.getCode(), is("ACTIVE"));
	}

	@Test
	@SuppressWarnings("static-method")
	void enumeratedValueSetDescriptionAndGet() {
		EnumeratedValue val = new EnumeratedValue();
		val.setDescription("Active status");
		assertThat(val.getDescription(), is("Active status"));
	}

	@Test
	@SuppressWarnings("static-method")
	void enumeratedValueSetDescriptionBlankBecomesNull() {
		EnumeratedValue val = new EnumeratedValue();
		val.setDescription("  ");
		assertNull(val.getDescription());
	}

	// ---- EnumeratedValue.toJavaIdentifier ----

	@Test
	@SuppressWarnings("static-method")
	void toJavaIdentifierReturnsNameWhenSet() {
		EnumeratedValue val = new EnumeratedValue();
		val.setName("active");
		val.setCode("ACTIVE");
		assertThat(val.toJavaIdentifier(), is("active"));
	}

	@Test
	@SuppressWarnings("static-method")
	void toJavaIdentifierDerivedFromCodeWhenNameNull() {
		EnumeratedValue val = new EnumeratedValue();
		val.setCode("ACTIVE");
		// name is null → toJavaInstanceIdentifier of code
		assertNotNull(val.toJavaIdentifier());
	}

	@Test
	@SuppressWarnings("static-method")
	void toJavaIdentifierDerivedFromDescriptionWhenNameNullAndDescriptionSet() {
		EnumeratedValue val = new EnumeratedValue();
		val.setCode("ACT");
		val.setDescription("Active Status");
		// name is null, description is used
		assertNotNull(val.toJavaIdentifier());
	}

	// ---- isEnumClassLoadingFailure (static) ----

	@Test
	@SuppressWarnings("static-method")
	void isEnumClassLoadingFailureTrueForClassNotFoundException() {
		assertTrue(Enumeration.isEnumClassLoadingFailure(new ClassNotFoundException("test")));
	}

	@Test
	@SuppressWarnings("static-method")
	void isEnumClassLoadingFailureTrueForNoClassDefFoundError() {
		assertTrue(Enumeration.isEnumClassLoadingFailure(new NoClassDefFoundError("test")));
	}

	@Test
	@SuppressWarnings("static-method")
	void isEnumClassLoadingFailureFalseForOtherException() {
		assertFalse(Enumeration.isEnumClassLoadingFailure(new RuntimeException("test")));
	}

	@Test
	@SuppressWarnings("static-method")
	void isEnumClassLoadingFailureTrueForWrappedClassNotFoundException() {
		RuntimeException wrapper = new RuntimeException("wrapper", new ClassNotFoundException("inner"));
		assertTrue(Enumeration.isEnumClassLoadingFailure(wrapper));
	}

	@Test
	@SuppressWarnings("static-method")
	void isEnumClassLoadingFailureFalseForNullCause() {
		assertFalse(Enumeration.isEnumClassLoadingFailure(new IllegalStateException("no cause")));
	}
}
