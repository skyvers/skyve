package org.skyve.util;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.instanceOf;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.withSettings;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.List;
import java.util.TreeMap;

import org.junit.jupiter.api.Test;
import org.skyve.domain.Bean;
import org.skyve.impl.domain.AbstractBean;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.impl.bind.BindUtil;
import org.skyve.metadata.user.User;
import org.skyve.domain.types.DateOnly;
import org.skyve.domain.types.TimeOnly;
import org.skyve.domain.types.DateTime;
import org.skyve.domain.types.Timestamp;
import java.math.BigDecimal;

@SuppressWarnings({"static-method", "boxing", "java:S5778"})
class BinderTest {
	public abstract static class TestBeanBase extends AbstractBean {
		private static final long serialVersionUID = 1L;
		private String bizCustomer;
		private String bizDataGroupId;
		private String bizUserId;
		private String bizKey;

		@Override
		public String getBizCustomer() {
			return bizCustomer;
		}

		@Override
		public void setBizCustomer(String bizCustomer) {
			this.bizCustomer = bizCustomer;
		}

		@Override
		public String getBizDataGroupId() {
			return bizDataGroupId;
		}

		@Override
		public void setBizDataGroupId(String bizDataGroupId) {
			this.bizDataGroupId = bizDataGroupId;
		}

		@Override
		public String getBizUserId() {
			return bizUserId;
		}

		@Override
		public void setBizUserId(String bizUserId) {
			this.bizUserId = bizUserId;
		}

		@Override
		public String getBizKey() {
			return bizKey;
		}

		void setBizKey(String bizKey) {
			this.bizKey = bizKey;
		}
	}

	public static class SimpleChild extends TestBeanBase {
		private static final long serialVersionUID = 1L;
		private String bizId;

		public SimpleChild(String bizId) {
			this.bizId = bizId;
		}

		@Override
		public String getBizId() {
			return bizId;
		}

		@Override
		public String getBizModule() {
			return "admin";
		}

		@Override
		public String getBizDocument() {
			return "Child";
		}
	}

	public static class SimpleOwner extends TestBeanBase {
		private static final long serialVersionUID = 1L;
		private String bizId;
		private Bean manager;
		private List<SimpleChild> children = new ArrayList<>();

		public SimpleOwner(String bizId) {
			this.bizId = bizId;
		}

		@Override
		public String getBizId() {
			return bizId;
		}

		@Override
		public String getBizModule() {
			return "admin";
		}

		@Override
		public String getBizDocument() {
			return "Owner";
		}

		public Bean getManager() {
			return manager;
		}

		public void setManager(Bean manager) {
			this.manager = manager;
		}

		public List<SimpleChild> getChildren() {
			return children;
		}

		public boolean addChildrenElement(SimpleChild element) {
			return children.add(element);
		}

		public void addChildrenElement(int index, SimpleChild element) {
			children.add(index, element);
		}

		public boolean removeChildrenElement(SimpleChild element) {
			return children.remove(element);
		}

		public SimpleChild removeChildrenElement(int index) {
			return children.remove(index);
		}
	}
	public static class Address {
		private String city;

		public String getCity() {
			return city;
		}

		public void setCity(String city) {
			this.city = city;
		}
	}

	public static class Person {
		private String name;
		private Address address = new Address();

		public String getName() {
			return name;
		}

		public void setName(String name) {
			this.name = name;
		}

		public Address getAddress() {
			return address;
		}

		public void setAddress(Address address) {
			this.address = address;
		}
	}

	private static void withThreadLocalUser(User user, Runnable run) {
		AbstractPersistence persistence = mock(AbstractPersistence.class, withSettings().defaultAnswer(CALLS_REAL_METHODS));
		persistence.setUser(user);
		persistence.setForThread();
		try {
			run.run();
		}
		finally {
			clearPersistenceThreadLocal();
		}
	}

	private static void clearPersistenceThreadLocal() {
		try {
			Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
			field.setAccessible(true);
			@SuppressWarnings("unchecked")
			ThreadLocal<AbstractPersistence> threadLocal = (ThreadLocal<AbstractPersistence>) field.get(null);
			threadLocal.remove();
		}
		catch (ReflectiveOperationException e) {
			throw new AssertionError(e);
		}
	}

	@Test
	void findElementInCollectionReturnsMatchWhenPresent() {
		List<Bean> list = new ArrayList<>();
		Bean a = mock(Bean.class);
		Bean b = mock(Bean.class);
		when(a.getBizId()).thenReturn("a");
		when(b.getBizId()).thenReturn("b");
		list.add(a);
		list.add(b);

		Bean result = Binder.findElementInCollection(list, "b");
		assertThat(result, is(b));
	}

	@Test
	void findElementInCollectionReturnsNullWhenMissing() {
		List<Bean> list = new ArrayList<>();
		Bean a = mock(Bean.class);
		when(a.getBizId()).thenReturn("a");
		list.add(a);
		assertNull(Binder.findElementInCollection(list, "x"));
	}

	@Test
	void setElementInCollectionReplacesExistingMatchingBizId() {
		List<Bean> list = new ArrayList<>();
		Bean oldBean = mock(Bean.class);
		Bean replacement = mock(Bean.class);
		when(oldBean.getBizId()).thenReturn("A");
		when(replacement.getBizId()).thenReturn("A");
		list.add(oldBean);

		Binder.setElementInCollection(list, replacement);
		assertEquals(1, list.size());
		assertThat(list.get(0), is(replacement));
	}

	@Test
	void javaIdentifierHelpersDelegateToBindUtil() {
		assertThat(Binder.toJavaTypeIdentifier("my value"), is("MyValue"));
		assertThat(Binder.toJavaInstanceIdentifier("my value"), is("myValue"));
		assertThat(Binder.toJavaStaticIdentifier("my value"), is("MY_VALUE"));
		assertThat(Binder.toJavaPropertyName("getMyValue"), is("myValue"));
		assertThat(Binder.toTitleCase("my_value"), is("My Value"));
	}

	@Test
	void bindingHelpersDelegateToBindUtil() {
		assertThat(Binder.createCompoundBinding("a", "b"), is("a.b"));
		assertThat(Binder.createIndexedBinding("a", 2), is("a[2]"));
		assertThat(Binder.createIdBinding("a", "id-1"), is("aElementById(id-1)"));
		assertThat(Binder.negateCondition("x"), is("notX"));
	}

	@Test
	void negateConditionReturnsNullForNullInput() {
		assertNull(Binder.negateCondition(null));
	}

	@Test
	void negateConditionNegatesTrue() {
		assertThat(Binder.negateCondition("true"), is("false"));
	}

	@Test
	void negateConditionNegatesFalse() {
		assertThat(Binder.negateCondition("false"), is("true"));
	}

	@Test
	void negateConditionRemovesNotPrefix() {
		assertThat(Binder.negateCondition("notActive"), is("active"));
	}

	@Test
	void expressionHelpersDelegateToBindUtil() {
		assertTrue(Binder.isSkyveExpression("{USER}"));
		assertTrue(Binder.containsSkyveExpressions("Hi {USER}"));
		assertTrue(Binder.isImplicit("bizKey"));
	}

	@Test
	void targetMetaDataConstructorStoresValues() {
		Document document = mock(Document.class);
		Attribute attribute = mock(Attribute.class);
		Binder.TargetMetaData meta = new Binder.TargetMetaData(document, attribute, String.class);

		assertThat(meta.getDocument(), is(document));
		assertThat(meta.getAttribute(), is(attribute));
		assertThat(meta.getType(), is((Class<?>) String.class));
	}

	@Test
	void getAndSetSimpleAndCompoundProperties() {
		Person person = new Person();

		Binder.set(person, "name", "Alice");
		Binder.set(person, "address.city", "Sydney");

		assertThat(Binder.get(person, "name"), is((Object) "Alice"));
		assertThat(Binder.get(person, "address.city"), is((Object) "Sydney"));
	}

	@Test
	void convertAndSetUpdatesPropertyValue() {
		Person person = new Person();
		Binder.convertAndSet(person, "name", "Bob");
		assertThat(person.getName(), is("Bob"));
	}

	@Test
	void propertyTypeAndMutabilityHelpersWorkForPojoProperties() {
		Person person = new Person();
		assertThat(Binder.getPropertyType(person, "name"), is((Class<?>) String.class));
		assertTrue(Binder.isMutable(person, "name"));
	}

	@Test
	void scalarAndImplicitHelpersReturnExpectedValues() {
		assertTrue(Binder.isAScalarType(String.class));
		assertFalse(Binder.isAScalarType(List.class));
		assertTrue(Binder.isImplicit("bizKey"));
		assertFalse(Binder.isImplicit("name"));
	}

	@Test
	void convertAndSerialisationHelpersHandleSimpleTypes() {
		assertNull(Binder.convert(String.class, null));
		assertThat(Binder.nullSafeConvert(String.class, "value"), is((Object) "value"));
		assertThat(Binder.fromSerialised(Integer.class, "42"), is((Object) Integer.valueOf(42)));
		assertThat(Binder.fromSerialised(null, Integer.class, "7"), is((Object) Integer.valueOf(7)));
	}

	@Test
	void fromStringConvertsSimpleValues() {
		Object value = Binder.fromString(null, null, Integer.class, "9");
		assertThat(value, instanceOf(Integer.class));
		assertThat(value, is((Object) Integer.valueOf(9)));
	}

	@Test
	void formatMessageOverloadsReturnInterpolatedResult() {
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		Bean bean = mock(Bean.class);
		when(user.getCustomer()).thenReturn(customer);
		when(bean.getBizKey()).thenReturn("Record 1");

		withThreadLocalUser(user, () -> {
			String plain = Binder.formatMessage("Hello {bizKey}", bean);
			String upper = Binder.formatMessage("Hello {bizKey}", String::toUpperCase, bean);

			assertThat(plain, is("Hello Record 1"));
			assertThat(upper, is("Hello RECORD 1"));
		});
	}

	@Test
	void validateMessageWithNoDocumentNamesReturnsNull() {
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		when(user.getCustomer()).thenReturn(customer);
		when(customer.getModule("admin")).thenReturn(module);

		withThreadLocalUser(user, () -> {
			String result = Binder.validateMessage("plain text", "admin");
			assertNull(result);
		});
	}

	@Test
	void validateMessageWithOneAndManyDocumentsReturnsNullForPlainText() {
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document d1 = mock(Document.class);
		Document d2 = mock(Document.class);
		when(user.getCustomer()).thenReturn(customer);
		when(customer.getModule("admin")).thenReturn(module);
		when(module.getDocument(customer, "Contact")).thenReturn(d1);
		when(module.getDocument(customer, "Address")).thenReturn(d2);

		withThreadLocalUser(user, () -> {
			assertNull(Binder.validateMessage("plain text", "admin", "Contact"));
			assertNull(Binder.validateMessage("plain text", "admin", "Contact", "Address"));
		});
	}

	@Test
	void orderAcceptsNullListWithoutThrowing() {
		assertDoesNotThrow(() -> Binder.order(null));
	}

	@Test
	void lightweightCollectionAndAssociationWrappersWork() {
		SimpleOwner owner = new SimpleOwner("O1");
		SimpleChild c1 = new SimpleChild("C1");
		SimpleChild c2 = new SimpleChild("C2");
		owner.getChildren().add(c1);
		owner.getChildren().add(c2);

		Bean found = Binder.getElementInCollection(owner, "children", "C2");
		assertThat(found, is((Bean) c2));
	}

	@Test
	void wrappersRequiringMetadataOrContextThrowWithoutRuntimeContext() {
		SimpleOwner owner = new SimpleOwner("O1");
		SimpleChild child = new SimpleChild("C1");

		assertThrows(RuntimeException.class, () -> Binder.validateMessage("{bizKey}", mock(Document.class)));
		assertThrows(RuntimeException.class,
				() -> Binder.validateBinding(mock(Customer.class), mock(Module.class), mock(Document.class), "x"));
		assertThrows(RuntimeException.class, () -> Binder.getDisplay(mock(Customer.class), owner, "manager"));
		assertThrows(RuntimeException.class, () -> Binder.ensureElementIsInCollection(owner, "children", child));
		assertThrows(RuntimeException.class, () -> Binder.setAssociation(owner, "manager", child));
		assertThrows(RuntimeException.class, () -> Binder.addElementToCollection(owner, "children", child));
		assertThrows(RuntimeException.class, () -> Binder.addElementToCollection(owner, "children", 0, child));
		assertThrows(RuntimeException.class, () -> Binder.removeElementFromCollection(owner, "children", child));
		assertThrows(RuntimeException.class, () -> Binder.removeElementFromCollection(owner, "children", 0));
		assertThrows(RuntimeException.class, () -> Binder.orderByMetaData(owner, "children"));
	}

	@Test
	void metadataAndPopulateHelpersAreInvokable() {
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		Attribute attribute = mock(Attribute.class);

		assertFalse(Binder.isDynamic(customer, module, document, attribute));
		assertFalse(Binder.isDynamic(customer, module, attribute));
		assertThrows(RuntimeException.class, () -> Binder.isDynamic(customer, module,
				(org.skyve.metadata.model.document.Relation) mock(org.skyve.metadata.model.document.Relation.class)));

		assertDoesNotThrow(() -> Binder.populateProperties(user, null, new TreeMap<>(), false));
		assertThrows(RuntimeException.class, () -> Binder.populateProperty(user, new SimpleOwner("O2"), "bizKey", "K", false));
		assertThrows(RuntimeException.class, () -> Binder.copy(new SimpleOwner("F1"), new SimpleOwner("T1")));

		assertThrows(RuntimeException.class,
				() -> Binder.getMetaDataForBinding(customer, module, document, "children[0].bizId"));
		assertThrows(RuntimeException.class,
				() -> Binder.instantiateAndGet(user, module, document, new SimpleOwner("I1"), "manager.bizId"));
	}

	@Test
	void validateMessageDocumentOverloadWithPlainTextAndThreadLocalCustomerReturnsNull() {
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		when(user.getCustomer()).thenReturn(customer);
		withThreadLocalUser(user, () -> {
			Document document = mock(Document.class);
			assertNull(Binder.validateMessage("plain text", document));
		});
	}

	@Test
	void ensureElementAlreadyInCollectionReturnsSameElement() {
		SimpleOwner owner = new SimpleOwner("O1");
		SimpleChild child = new SimpleChild("C1");
		owner.getChildren().add(child);
		Bean result = Binder.ensureElementIsInCollection(owner, "children", child);
		assertSame(child, result);
	}

	@Test
	void addElementToCollectionAddsToStaticCollection() {
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		Attribute collectionAttr = mock(Attribute.class); // not Field, not Relation -> not dynamic

		when(user.getCustomer()).thenReturn(customer);
		when(customer.getModule("admin")).thenReturn(module);
		when(module.getDocument(customer, "Owner")).thenReturn(document);
		when(document.getPolymorphicAttribute(customer, "children")).thenReturn(collectionAttr);

		SimpleOwner owner = new SimpleOwner("O1");
		SimpleChild child = new SimpleChild("C1");

		withThreadLocalUser(user, () -> {
			boolean added = Binder.addElementToCollection(owner, "children", child);
			assertTrue(added, "Element should be added");
			assertTrue(owner.getChildren().contains(child), "Child should be in collection");
		});
	}

	@Test
	void addElementToCollectionAtIndexAddsToStaticCollection() {
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		Attribute collectionAttr = mock(Attribute.class);

		when(user.getCustomer()).thenReturn(customer);
		when(customer.getModule("admin")).thenReturn(module);
		when(module.getDocument(customer, "Owner")).thenReturn(document);
		when(document.getPolymorphicAttribute(customer, "children")).thenReturn(collectionAttr);

		SimpleOwner owner = new SimpleOwner("O1");
		SimpleChild c1 = new SimpleChild("C1");
		SimpleChild c2 = new SimpleChild("C2");
		owner.getChildren().add(c1);

		withThreadLocalUser(user, () -> {
			Binder.addElementToCollection(owner, "children", 0, c2);
			assertEquals(c2, owner.getChildren().get(0), "Element should be inserted at index 0");
		});
	}

	@Test
	void removeElementFromCollectionByBeanRemovesFromStaticCollection() {
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		Attribute collectionAttr = mock(Attribute.class);

		when(user.getCustomer()).thenReturn(customer);
		when(customer.getModule("admin")).thenReturn(module);
		when(module.getDocument(customer, "Owner")).thenReturn(document);
		when(document.getPolymorphicAttribute(customer, "children")).thenReturn(collectionAttr);

		SimpleOwner owner = new SimpleOwner("O1");
		SimpleChild child = new SimpleChild("C1");
		owner.getChildren().add(child);

		withThreadLocalUser(user, () -> {
			boolean removed = Binder.removeElementFromCollection(owner, "children", child);
			assertTrue(removed, "Element should be removed");
			assertFalse(owner.getChildren().contains(child), "Child should not be in collection");
		});
	}

	@Test
	void removeElementFromCollectionByIndexRemovesFromStaticCollection() {
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		Attribute collectionAttr = mock(Attribute.class);

		when(user.getCustomer()).thenReturn(customer);
		when(customer.getModule("admin")).thenReturn(module);
		when(module.getDocument(customer, "Owner")).thenReturn(document);
		when(document.getPolymorphicAttribute(customer, "children")).thenReturn(collectionAttr);

		SimpleOwner owner = new SimpleOwner("O1");
		SimpleChild child = new SimpleChild("C1");
		owner.getChildren().add(child);

		withThreadLocalUser(user, () -> {
			Bean removed = Binder.removeElementFromCollection(owner, "children", 0);
			assertSame(child, removed, "Removed element should be the child");
			assertTrue(owner.getChildren().isEmpty(), "Collection should be empty after removal");
		});
	}

	@Test
	void setAssociationSetsManagerOnOwner() {
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		Attribute assocAttr = mock(Attribute.class);

		when(user.getCustomer()).thenReturn(customer);
		when(customer.getModule("admin")).thenReturn(module);
		when(module.getDocument(customer, "Owner")).thenReturn(document);
		when(document.getPolymorphicAttribute(customer, "manager")).thenReturn(assocAttr);

		SimpleOwner owner = new SimpleOwner("O1");
		SimpleChild manager = new SimpleChild("M1");

		withThreadLocalUser(user, () -> {
			Binder.setAssociation(owner, "manager", manager);
			assertSame(manager, owner.getManager(), "Manager should be set");
		});
	}

	@Test
	void setAssociationSetsManagerToNull() {
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		Attribute assocAttr = mock(Attribute.class);

		when(user.getCustomer()).thenReturn(customer);
		when(customer.getModule("admin")).thenReturn(module);
		when(module.getDocument(customer, "Owner")).thenReturn(document);
		when(document.getPolymorphicAttribute(customer, "manager")).thenReturn(assocAttr);

		SimpleOwner owner = new SimpleOwner("O1");
		owner.setManager(new SimpleChild("M1")); // set initially

		withThreadLocalUser(user, () -> {
			Binder.setAssociation(owner, "manager", null);
			assertNull(owner.getManager(), "Manager should be null after setAssociation(null)");
		});
	}

	@Test
	void ensureElementNotInCollectionAddsThenReturnsIt() {
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		Attribute collectionAttr = mock(Attribute.class);

		when(user.getCustomer()).thenReturn(customer);
		when(customer.getModule("admin")).thenReturn(module);
		when(module.getDocument(customer, "Owner")).thenReturn(document);
		when(document.getPolymorphicAttribute(customer, "children")).thenReturn(collectionAttr);

		SimpleOwner owner = new SimpleOwner("O1");
		SimpleChild child = new SimpleChild("C2");

		withThreadLocalUser(user, () -> {
			Bean result = Binder.ensureElementIsInCollection(owner, "children", child);
			assertSame(child, result, "ensureElement should return the element");
			assertTrue(owner.getChildren().contains(child), "Child should now be in collection");
		});
	}

	// ---- nullSafeConvert extended paths ------------------------------------

	@Test
	void nullSafeConvertLongFromInteger() {
		Object result = Binder.nullSafeConvert(Long.class, Integer.valueOf(42));
		assertThat(result, instanceOf(Long.class));
		assertEquals(42L, result);
	}

	@Test
	void nullSafeConvertIntegerFromLong() {
		Object result = Binder.nullSafeConvert(Integer.class, Long.valueOf(42L));
		assertThat(result, instanceOf(Integer.class));
		assertEquals(42, result);
	}

	@Test
	void nullSafeConvertDoubleFromInteger() {
		Object result = Binder.nullSafeConvert(Double.class, Integer.valueOf(5));
		assertThat(result, instanceOf(Double.class));
		assertEquals(5.0, (Double) result, 0.0001);
	}

	@Test
	void nullSafeConvertFloatFromInteger() {
		Object result = Binder.nullSafeConvert(Float.class, Integer.valueOf(3));
		assertThat(result, instanceOf(Float.class));
		assertEquals(3.0f, (Float) result, 0.0001f);
	}

	@Test
	void nullSafeConvertBigDecimalFromString() {
		Object result = Binder.nullSafeConvert(BigDecimal.class, "1.23");
		assertThat(result, instanceOf(BigDecimal.class));
		assertEquals(new BigDecimal("1.23"), result);
	}

	@Test
	void nullSafeConvertDateOnlyFromJavaDate() {
		java.util.Date javaDate = new java.util.Date(1000000L);
		Object result = Binder.nullSafeConvert(DateOnly.class, javaDate);
		assertThat(result, instanceOf(DateOnly.class));
	}

	@Test
	void nullSafeConvertTimeOnlyFromJavaDate() {
		java.util.Date javaDate = new java.util.Date(1000000L);
		Object result = Binder.nullSafeConvert(TimeOnly.class, javaDate);
		assertThat(result, instanceOf(TimeOnly.class));
	}

	@Test
	void nullSafeConvertDateTimeFromJavaDate() {
		java.util.Date javaDate = new java.util.Date(1000000L);
		Object result = Binder.nullSafeConvert(DateTime.class, javaDate);
		assertThat(result, instanceOf(DateTime.class));
	}

	@Test
	void nullSafeConvertTimestampFromJavaDate() {
		java.util.Date javaDate = new java.util.Date(1000000L);
		Object result = Binder.nullSafeConvert(Timestamp.class, javaDate);
		assertThat(result, instanceOf(Timestamp.class));
	}

	@Test
	void nullSafeConvertSameTypeReturnsSameObject() {
		Long val = Long.valueOf(99L);
		Object result = Binder.nullSafeConvert(Long.class, val);
		assertSame(val, result);
	}

	// ---- toDisplay ----------------------------------------------------------

	@Test
	void toDisplayNullValueReturnsEmptyString() {
		Customer customer = mock(Customer.class);
		assertEquals("", BindUtil.toDisplay(customer, null));
	}

	@Test
	void toDisplayBooleanTrueReturnsYes() {
		Customer customer = mock(Customer.class);
		assertEquals("Yes", BindUtil.toDisplay(customer, Boolean.TRUE));
	}

	@Test
	void toDisplayBooleanFalseReturnsNo() {
		Customer customer = mock(Customer.class);
		assertEquals("No", BindUtil.toDisplay(customer, Boolean.FALSE));
	}

	@Test
	void toDisplayStringValueUsesToString() {
		Customer customer = mock(Customer.class);
		assertEquals("hello", BindUtil.toDisplay(customer, "hello"));
	}

	@Test
	void toDisplayIntegerUsesToString() {
		Customer customer = mock(Customer.class);
		assertEquals("42", BindUtil.toDisplay(customer, 42));
	}

	// ---- sanitiseBinding / unsanitiseBinding ---------------------------------

	@Test
	void sanitiseBindingReturnsNullForNull() {
		assertNull(BindUtil.sanitiseBinding(null));
	}

	@Test
	void sanitiseBindingReplacesDotWithUnderscore() {
		assertEquals("a_b_c", BindUtil.sanitiseBinding("a.b.c"));
	}

	@Test
	void sanitiseBindingReplacesSquareBracketsWithUnderscore() {
		assertEquals("a_0_", BindUtil.sanitiseBinding("a[0]"));
	}

	@Test
	void unsanitiseBindingReturnsNullForNull() {
		assertNull(BindUtil.unsanitiseBinding(null));
	}

	@Test
	void unsanitiseBindingConvertsUnderscoreToDot() {
		assertEquals("a.b.c", BindUtil.unsanitiseBinding("a_b_c"));
	}

	@Test
	void unsanitiseBindingConvertsIndexedUnderscoresToBrackets() {
		assertEquals("a[0]", BindUtil.unsanitiseBinding("a_0_"));
	}

	// ---- prefixMessageExpressions -------------------------------------------

	@Test
	void prefixMessageExpressionsReturnsNullForNull() {
		assertNull(BindUtil.prefixMessageExpressions(null, "prefix"));
	}

	@Test
	void prefixMessageExpressionsWithNoExpressionsReturnsSameString() {
		assertEquals("Hello World", BindUtil.prefixMessageExpressions("Hello World", "contact"));
	}

	@Test
	void prefixMessageExpressionsWithExpressionPrefixesBinding() {
		String result = BindUtil.prefixMessageExpressions("{name}", "contact");
		assertTrue(result.contains("contact"), "Result should contain prefix: " + result);
	}

	// ---- evaluateCondition --------------------------------------------------

	@Test
	void evaluateConditionReturnsTrueForTrueString() {
		SimpleOwner bean = new SimpleOwner("O1");
		assertTrue(BindUtil.evaluateCondition(bean, "true"));
	}

	@Test
	void evaluateConditionReturnsFalseForFalseString() {
		SimpleOwner bean = new SimpleOwner("O1");
		assertFalse(BindUtil.evaluateCondition(bean, "false"));
	}
}
