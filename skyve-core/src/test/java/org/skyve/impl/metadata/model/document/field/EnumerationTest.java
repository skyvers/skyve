package org.skyve.impl.metadata.model.document.field;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import org.junit.jupiter.api.Test;
import org.skyve.domain.types.Enumeration.DomainValueSortByDescription;
import org.skyve.impl.metadata.model.document.field.Enumeration.EnumeratedValue;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.Bizlet.DomainValue;

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

	@Test
	@SuppressWarnings("static-method")
	void setAndGetOwningDocumentRoundTrips() {
		Enumeration e = new Enumeration();
		Document doc = mock(Document.class);
		e.setOwningDocument(doc);
		assertEquals(doc, e.getOwningDocument());
	}

	@Test
	@SuppressWarnings("static-method")
	void toJavaIdentifierReturnsTypeNameWhenSet() {
		Enumeration e = new Enumeration();
		e.setXmlTypeName("MyCustomEnum");
		assertEquals("MyCustomEnum", e.toJavaIdentifier());
	}

	@Test
	@SuppressWarnings("static-method")
	void toJavaIdentifierDerivedFromNameWhenTypeNameNull() {
		Enumeration e = new Enumeration();
		e.setName("statusValue");
		// typeName is null → toJavaIdentifier calls BindUtil.toJavaTypeIdentifier(name)
		assertNotNull(e.toJavaIdentifier());
	}

	// ---- DomainValueSortByDescription ----

	@Test
	@SuppressWarnings("static-method")
	void domainValueSortByDescriptionComparesAscending() {
		DomainValueSortByDescription comparator = new DomainValueSortByDescription();
		DomainValue a = new DomainValue("code1", "Apple");
		DomainValue b = new DomainValue("code2", "Banana");
		assertTrue(comparator.compare(a, b) < 0);
	}

	@Test
	@SuppressWarnings("static-method")
	void domainValueSortByDescriptionEqualDescriptions() {
		DomainValueSortByDescription comparator = new DomainValueSortByDescription();
		DomainValue a = new DomainValue("code1", "Same");
		DomainValue b = new DomainValue("code2", "Same");
		assertEquals(0, comparator.compare(a, b));
	}

        @Test
        @SuppressWarnings("static-method")
        void toJavaIdentifierAppendsValueWhenCodeIsJavaReservedWord() {
                Enumeration.EnumeratedValue val = new Enumeration.EnumeratedValue();
                // "new" is a Java reserved word; when name is null the code/description is
                // converted to a Java identifier and "new" should become "newValue"
                val.setCode("new");
                String result = val.toJavaIdentifier();
                assertTrue(result.endsWith("Value"), "Expected identifier to end with 'Value' but got: " + result);
        }

	// ---- getConverter ----

	@Test
	@SuppressWarnings("static-method")
	void getConverterReturnsNullWhenNotDynamic() {
		Enumeration e = new Enumeration();
		assertNull(e.getConverter());
	}

	@Test
	@SuppressWarnings("static-method")
	void getConverterReturnsDynamicEnumerationConverterWhenDynamic() {
		Enumeration e = new Enumeration();
		e.setDynamic(true);
		assertNotNull(e.getConverter());
	}

	// ---- getTarget returns self when no attributeRef ----

	@Test
	@SuppressWarnings("static-method")
	void getTargetReturnsSelfWhenNoAttributeRef() {
		Enumeration e = new Enumeration();
		assertEquals(e, e.getTarget());
	}

	// ---- getEncapsulatingClassName ----

	@Test
	@SuppressWarnings("static-method")
	void getEncapsulatingClassNameUsesModuleAndDocumentRef() {
		Enumeration e = new Enumeration();
		e.setModuleRef("admin");
		e.setDocumentRef("User");
		Document doc = mock(Document.class);
		when(doc.getOwningModuleName()).thenReturn("admin");
		when(doc.getName()).thenReturn("User");
		e.setOwningDocument(doc);
		String result = e.getEncapsulatingClassName();
		assertTrue(result.contains("admin"), "Should contain module name");
		assertTrue(result.contains("User"), "Should contain document name");
	}

	@Test
	@SuppressWarnings("static-method")
	void getEncapsulatingClassNameFallsBackToOwningDocumentWhenRefsNull() {
		Enumeration e = new Enumeration();
		Document doc = mock(Document.class);
		when(doc.getOwningModuleName()).thenReturn("myModule");
		when(doc.getName()).thenReturn("MyDocument");
		e.setOwningDocument(doc);
		String result = e.getEncapsulatingClassName();
		assertTrue(result.contains("myModule"), "Should contain owning module");
		assertTrue(result.contains("MyDocument"), "Should contain owning document");
	}

        @Test
        @SuppressWarnings("static-method")
        void getTypeNameReturnsNullWhenNoTypeNameSet() {
                Enumeration e = new Enumeration();
                assertNull(e.getTypeName());
        }

        @Test
        @SuppressWarnings("static-method")
        void getTypeNameReturnSetValue() {
                Enumeration e = new Enumeration();
                e.setXmlTypeName("MyEnum");
                assertEquals("MyEnum", e.getTypeName());
        }

        @Test
        @SuppressWarnings("static-method")
        void getValuesReturnsEmptyListByDefault() {
                Enumeration e = new Enumeration();
                assertNotNull(e.getValues());
                assertTrue(e.getValues().isEmpty());
        }

        @Test
        @SuppressWarnings("static-method")
        void getValuesReturnsSameEntriesAsXmlValues() {
                Enumeration e = new Enumeration();
                EnumeratedValue v = new EnumeratedValue();
                v.setCode("code1");
                e.getXmlValues().add(v);
                assertEquals(1, e.getValues().size());
                assertEquals("code1", e.getValues().get(0).getCode());
        }
	}
