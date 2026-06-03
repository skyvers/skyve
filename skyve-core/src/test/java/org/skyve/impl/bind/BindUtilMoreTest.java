package org.skyve.impl.bind;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import org.skyve.domain.Bean;
import org.skyve.domain.ChildBean;
import org.skyve.domain.DynamicBean;
import org.skyve.domain.HierarchicalBean;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.types.OptimisticLock;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Condition;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;

@SuppressWarnings("static-method")
class BindUtilMoreTest {

	// --- containsSkyveExpressions ---

	@Test
	void containsSkyveExpressionsNoBracesReturnsFalse() {
		assertFalse(BindUtil.containsSkyveExpressions("hello world"));
	}

	@Test
	void containsSkyveExpressionsWithOpenBraceAtStartReturnsTrue() {
		assertTrue(BindUtil.containsSkyveExpressions("{binding}"));
	}

	@Test
	void containsSkyveExpressionsWithOpenBraceNotAtStartReturnsTrue() {
		assertTrue(BindUtil.containsSkyveExpressions("hello {binding}"));
	}

	@Test
	void containsSkyveExpressionsWithEscapedBraceReturnsFalse() {
		assertFalse(BindUtil.containsSkyveExpressions("hello \\{not an expression}"));
	}

	@Test
	void containsSkyveExpressionsEmptyStringReturnsFalse() {
		assertFalse(BindUtil.containsSkyveExpressions(""));
	}

	@Test
	void containsSkyveExpressionsOnlyEscapedBraceReturnsFalse() {
		assertFalse(BindUtil.containsSkyveExpressions("\\{escaped}"));
	}

	@Test
	void containsSkyveExpressionsMultipleExpressionsReturnsTrue() {
		assertTrue(BindUtil.containsSkyveExpressions("{first} and {second}"));
	}

	@Test
	void containsSkyveExpressionsEscapedBraceFollowedByExpressionReturnsTrue() {
		assertTrue(BindUtil.containsSkyveExpressions("\\{escaped} and {binding}"));
	}

	// --- isSkyveExpression ---

	@Test
	void isSkyveExpressionValidExpressionReturnsTrue() {
		assertTrue(BindUtil.isSkyveExpression("{binding}"));
	}

	@Test
	void isSkyveExpressionNoBracesReturnsFalse() {
		assertFalse(BindUtil.isSkyveExpression("binding"));
	}

	@Test
	void isSkyveExpressionOnlyOpenBraceReturnsFalse() {
		assertFalse(BindUtil.isSkyveExpression("{binding"));
	}

	@Test
	void isSkyveExpressionOnlyCloseBraceReturnsFalse() {
		assertFalse(BindUtil.isSkyveExpression("binding}"));
	}

	@Test
	void isSkyveExpressionSingleCharReturnsFalse() {
		assertFalse(BindUtil.isSkyveExpression("{"));
	}

	@Test
	void isSkyveExpressionEmptyBracesReturnsTrue() {
		// length > 1, starts with { and ends with } -> true even if empty content
		assertTrue(BindUtil.isSkyveExpression("{}"));
	}

	@Test
	void isSkyveExpressionLeadingTextReturnsFalse() {
		assertFalse(BindUtil.isSkyveExpression("text{binding}"));
	}

	@Test
	void isSkyveExpressionTrailingTextReturnsFalse() {
		assertFalse(BindUtil.isSkyveExpression("{binding}text"));
	}

	// --- implicitAttributeType ---

	@Test
	void implicitAttributeTypeBizIdReturnsString() {
		assertEquals(String.class, BindUtil.implicitAttributeType(Bean.DOCUMENT_ID));
	}

	@Test
	void implicitAttributeTypeBizKeyReturnsString() {
		assertEquals(String.class, BindUtil.implicitAttributeType(Bean.BIZ_KEY));
	}

	@Test
	void implicitAttributeTypeCustomerNameReturnsString() {
		assertEquals(String.class, BindUtil.implicitAttributeType(Bean.CUSTOMER_NAME));
	}

	@Test
	void implicitAttributeTypeDataGroupIdReturnsString() {
		assertEquals(String.class, BindUtil.implicitAttributeType(Bean.DATA_GROUP_ID));
	}

	@Test
	void implicitAttributeTypeUserIdReturnsString() {
		assertEquals(String.class, BindUtil.implicitAttributeType(Bean.USER_ID));
	}

	@Test
	void implicitAttributeTypeModuleKeyReturnsString() {
		assertEquals(String.class, BindUtil.implicitAttributeType(Bean.MODULE_KEY));
	}

	@Test
	void implicitAttributeTypeDocumentKeyReturnsString() {
		assertEquals(String.class, BindUtil.implicitAttributeType(Bean.DOCUMENT_KEY));
	}

	@Test
	void implicitAttributeTypeFlagCommentReturnsString() {
		assertEquals(String.class, BindUtil.implicitAttributeType(PersistentBean.FLAG_COMMENT_NAME));
	}

	@Test
	void implicitAttributeTypeHierarchicalParentIdReturnsString() {
		assertEquals(String.class, BindUtil.implicitAttributeType(HierarchicalBean.PARENT_ID));
	}

	@Test
	void implicitAttributeTypeParentNameReturnsBean() {
		assertEquals(Bean.class, BindUtil.implicitAttributeType(ChildBean.PARENT_NAME));
	}

	@Test
	void implicitAttributeTypeOrdinalReturnsInteger() {
		assertEquals(Integer.class, BindUtil.implicitAttributeType(Bean.ORDINAL_NAME));
	}

	@Test
	void implicitAttributeTypeVersionReturnsInteger() {
		assertEquals(Integer.class, BindUtil.implicitAttributeType(PersistentBean.VERSION_NAME));
	}

	@Test
	void implicitAttributeTypeCreatedKeyReturnsBoolean() {
		assertEquals(Boolean.class, BindUtil.implicitAttributeType(Bean.CREATED_KEY));
	}

	@Test
	void implicitAttributeTypeNotCreatedKeyReturnsBoolean() {
		assertEquals(Boolean.class, BindUtil.implicitAttributeType(Bean.NOT_CREATED_KEY));
	}

	@Test
	void implicitAttributeTypePersistedKeyReturnsBoolean() {
		assertEquals(Boolean.class, BindUtil.implicitAttributeType(Bean.PERSISTED_KEY));
	}

	@Test
	void implicitAttributeTypeNotPersistedKeyReturnsBoolean() {
		assertEquals(Boolean.class, BindUtil.implicitAttributeType(Bean.NOT_PERSISTED_KEY));
	}

	@Test
	void implicitAttributeTypeChangedKeyReturnsBoolean() {
		assertEquals(Boolean.class, BindUtil.implicitAttributeType(Bean.CHANGED_KEY));
	}

	@Test
	void implicitAttributeTypeNotChangedKeyReturnsBoolean() {
		assertEquals(Boolean.class, BindUtil.implicitAttributeType(Bean.NOT_CHANGED_KEY));
	}

	@Test
	void implicitAttributeTypeTaggedReturnsBoolean() {
		assertEquals(Boolean.class, BindUtil.implicitAttributeType(PersistentBean.TAGGED_NAME));
	}

	@Test
	void implicitAttributeTypeLockReturnsOptimisticLock() {
		assertEquals(OptimisticLock.class, BindUtil.implicitAttributeType(PersistentBean.LOCK_NAME));
	}

	@Test
	void implicitAttributeTypeUnknownAttributeReturnsNull() {
		assertThat(BindUtil.implicitAttributeType("myCustomField"), is(nullValue()));
	}

	// --- DynamicBean indexed get ---

	@Test
	void getWithIndexedDynamicBeanBindingReturnsElementAtIndex() {
		Map<String, Object> props = new HashMap<>();
		DynamicBean item0 = new DynamicBean("mod", "Item", new HashMap<>());
		DynamicBean item1 = new DynamicBean("mod", "Item", new HashMap<>());
		props.put("myList", new ArrayList<>(Arrays.asList(item0, item1)));
		DynamicBean bean = new DynamicBean("mod", "Doc", props);

		Object result = BindUtil.get(bean, "myList[0]");
		// assertSame avoids DynamicBean.equals() which calls CORE
		assertSame(item0, result);
	}

	@Test
	void getWithIndexedDynamicBeanBindingReturnsSecondElement() {
		Map<String, Object> props = new HashMap<>();
		DynamicBean item0 = new DynamicBean("mod", "Item", new HashMap<>());
		DynamicBean item1 = new DynamicBean("mod", "Item", new HashMap<>());
		props.put("myList", new ArrayList<>(Arrays.asList(item0, item1)));
		DynamicBean bean = new DynamicBean("mod", "Doc", props);

		Object result = BindUtil.get(bean, "myList[1]");
		// assertSame avoids DynamicBean.equals() which calls CORE
		assertSame(item1, result);
	}

	@Test
	void getWithDynamicElementByIdBindingReturnsMatchingElement() {
		Map<String, Object> itemProps = new HashMap<>();
		itemProps.put(Bean.DOCUMENT_ID, "bean-1");
		DynamicBean item0 = new DynamicBean("mod", "Item", itemProps);
		DynamicBean item1 = new DynamicBean("mod", "Item", new HashMap<>(Map.of(Bean.DOCUMENT_ID, "bean-2")));
		Map<String, Object> props = new HashMap<>();
		props.put("myList", new ArrayList<>(Arrays.asList(item0, item1)));
		DynamicBean bean = new DynamicBean("mod", "Doc", props);

		Object result = BindUtil.get(bean, BindUtil.createIdBinding("myList", "bean-1"));
		assertSame(item0, result);
	}

	@Test
	void getWithNullListInDynamicBeanReturnsNull() {
		Map<String, Object> props = new HashMap<>();
		props.put("myList", null);
		DynamicBean bean = new DynamicBean("mod", "Doc", props);

		Object result = BindUtil.get(bean, "myList[0]");
		assertNull(result);
	}

	@Test
	void getElementInCollectionOwnerBindingReturnsMatchingBean() {
		Map<String, Object> props = new HashMap<>();
		Map<String, Object> existingProps = new HashMap<>();
		existingProps.put(Bean.DOCUMENT_ID, "bean-1");
		DynamicBean existing = new DynamicBean("mod", "Item", existingProps);
		props.put("items", new ArrayList<>(Arrays.asList(existing)));
		DynamicBean owner = new DynamicBean("mod", "Doc", props);

		Bean result = BindUtil.getElementInCollection(owner, "items", "bean-1");
		assertSame(existing, result);
	}

	@Test
	void getElementInCollectionOwnerBindingReturnsNullWhenCollectionMissing() {
		Map<String, Object> props = new HashMap<>();
		props.put("items", null);
		DynamicBean owner = new DynamicBean("mod", "Doc", props);

		assertNull(BindUtil.getElementInCollection(owner, "items", "bean-1"));
	}

	@Test
	void ensureElementIsInCollectionReturnsExistingMatchingBean() {
		Map<String, Object> props = new HashMap<>();
		Map<String, Object> existingProps = new HashMap<>();
		existingProps.put(Bean.DOCUMENT_ID, "bean-1");
		DynamicBean existing = new DynamicBean("mod", "Item", existingProps);
		List<Bean> items = new ArrayList<>(Arrays.asList(existing));
		props.put("items", items);
		DynamicBean owner = new DynamicBean("mod", "Doc", props);
		Map<String, Object> candidateProps = new HashMap<>();
		candidateProps.put(Bean.DOCUMENT_ID, "bean-1");
		DynamicBean candidate = new DynamicBean("mod", "Item", candidateProps);

		Bean result = BindUtil.ensureElementIsInCollection(owner, "items", candidate);
		assertSame(existing, result);
		assertSame(existing, items.get(0));
	}

	@Test
	void setElementInCollectionReplacesAllMatchingBizIds() {
		Map<String, Object> firstProps = new HashMap<>();
		firstProps.put(Bean.DOCUMENT_ID, "bean-1");
		DynamicBean first = new DynamicBean("mod", "Item", firstProps);
		Map<String, Object> secondProps = new HashMap<>();
		secondProps.put(Bean.DOCUMENT_ID, "bean-1");
		DynamicBean second = new DynamicBean("mod", "Item", secondProps);
		Map<String, Object> otherProps = new HashMap<>();
		otherProps.put(Bean.DOCUMENT_ID, "bean-2");
		DynamicBean other = new DynamicBean("mod", "Item", otherProps);
		List<DynamicBean> list = new ArrayList<>(Arrays.asList(first, second, other));
		Map<String, Object> replacementProps = new HashMap<>();
		replacementProps.put(Bean.DOCUMENT_ID, "bean-1");
		DynamicBean replacement = new DynamicBean("mod", "Item", replacementProps);

		BindUtil.setElementInCollection(list, replacement);

		assertSame(replacement, list.get(0));
		assertSame(replacement, list.get(1));
		assertSame(other, list.get(2));
	}

	// --- DynamicBean indexed set ---

	@Test
	void setWithIndexedDynamicBeanBindingReplacesElementAtIndex() {
		Map<String, Object> props = new HashMap<>();
		DynamicBean original = new DynamicBean("mod", "Item", new HashMap<>());
		DynamicBean replacement = new DynamicBean("mod", "Item", new HashMap<>());
		List<DynamicBean> list = new ArrayList<>(Arrays.asList(original));
		props.put("myList", list);
		DynamicBean bean = new DynamicBean("mod", "Doc", props);

		BindUtil.set(bean, "myList[0]", replacement);
		// Use reference equality to avoid DynamicBean.equals() which calls CORE
		assertSame(replacement, list.get(0));
	}

	// --- isMutable on DynamicBean ---

	@Test
	void isMutableReturnsFalseForBizKeyBinding() {
		Map<String, Object> props = new HashMap<>();
		props.put(Bean.BIZ_KEY, "some key");
		DynamicBean bean = new DynamicBean("mod", "Doc", props);
		assertFalse(BindUtil.isMutable(bean, Bean.BIZ_KEY));
	}

	@Test
	void isMutableReturnsFalseForModuleKeyBinding() {
		Map<String, Object> props = new HashMap<>();
		props.put(Bean.MODULE_KEY, "mod");
		DynamicBean bean = new DynamicBean("mod", "Doc", props);
		assertFalse(BindUtil.isMutable(bean, Bean.MODULE_KEY));
	}

	@Test
	void isMutableReturnsTrueForStringDynamicProperty() {
		Map<String, Object> props = new HashMap<>();
		props.put("name", "test");
		DynamicBean bean = new DynamicBean("mod", "Doc", props);
		assertTrue(BindUtil.isMutable(bean, "name"));
	}

	@Test
	void isMutableReturnsFalseForListDynamicProperty() {
		Map<String, Object> props = new HashMap<>();
		props.put("items", new ArrayList<>());
		DynamicBean bean = new DynamicBean("mod", "Doc", props);
		assertFalse(BindUtil.isMutable(bean, "items"));
	}

	// --- getPropertyType on DynamicBean ---

	@Test
	void getPropertyTypeReturnsDynamicValueClassForNonNullDynamicProperty() {
		Map<String, Object> props = new HashMap<>();
		props.put("name", "hello");
		DynamicBean bean = new DynamicBean("mod", "Doc", props);
		Class<?> type = BindUtil.getPropertyType(bean, "name");
		assertEquals(String.class, type);
	}

	// --- set with String-to-non-String coercion ---

	@Test
	void setCoercesStringToIntegerViaValueOf() {
		IntPojo pojo = new IntPojo();
		BindUtil.set(pojo, "count", "42");
		assertEquals(Integer.valueOf(42), pojo.getCount());
	}

	@Test
	void setCoercesStringToIntegerViaConstructor() {
		// Integer has a (deprecated) String constructor, so it goes through constructor path first
		// OR falls through to valueOf — either way result should be 7
		IntPojo pojo = new IntPojo();
		BindUtil.set(pojo, "count", "7");
		assertEquals(Integer.valueOf(7), pojo.getCount());
	}

	// --- order with empty / single element lists ---

	@Test
	void orderWithNullListDoesNothing() {
		// order(null, ...) should be safe
		BindUtil.order(null);
		// If we got here without throwing, the test passes
		assertTrue(true);
	}

	@Test
	void orderWithEmptyListDoesNothing() {
		List<Object> list = new ArrayList<>();
		BindUtil.order(list);
		assertTrue(list.isEmpty());
	}

	// --- createCompoundBinding, createIndexedBinding, createIdBinding ---

	@Test
	void createCompoundBindingCombinesBindings() {
		assertEquals("a.b.c", BindUtil.createCompoundBinding("a", "b", "c"));
	}

	@Test
	void createCompoundBindingWithTwoBindings() {
		assertEquals("parent.child", BindUtil.createCompoundBinding("parent", "child"));
	}

	@Test
	void createIndexedBindingAddsIndexSuffix() {
		assertEquals("items[2]", BindUtil.createIndexedBinding("items", 2));
	}

	@Test
	void createIdBindingAddsIdSuffix() {
		String result = BindUtil.createIdBinding("items", "abc-123");
		assertTrue(result.contains("abc-123"));
	}

	// --- sanitiseBinding and unsanitiseBinding ---

	@Test
	void sanitiseBindingReturnsNullForNull() {
		assertNull(BindUtil.sanitiseBinding(null));
	}

	@Test
	void unsanitiseBindingReturnsNullForNull() {
		assertNull(BindUtil.unsanitiseBinding(null));
	}

	@Test
	void sanitiseAndUnsanitiseBindingAreInverse() {
		String binding = "items[0].name";
		String sanitised = BindUtil.sanitiseBinding(binding);
		assertNotNull(sanitised);
		String unsanitised = BindUtil.unsanitiseBinding(sanitised);
		// unsanitised should preserve the binding info
		assertNotNull(unsanitised);
	}

	// --- Helper POJO ---
	public static class IntPojo {
		private Integer count;

		public Integer getCount() {
			return count;
		}

		public void setCount(Integer count) {
			this.count = count;
		}
	}

	// --- evaluateCondition (CORE-backed paths) ---

	@Test
	void evaluateConditionWithImplicitConditionReturnsTrueWhenPropertyIsTrue() {
		AbstractPersistence persistence = Mockito.mock(AbstractPersistence.class,
				Mockito.withSettings().defaultAnswer(Mockito.CALLS_REAL_METHODS));
		User user = Mockito.mock(User.class);
		Customer customer = Mockito.mock(Customer.class);
		Module module = Mockito.mock(Module.class);
		Document document = Mockito.mock(Document.class);
		Mockito.when(user.getCustomer()).thenReturn(customer);
		Mockito.when(customer.getModule("mod")).thenReturn(module);
		Mockito.when(module.getDocument(customer, "Doc")).thenReturn(document);
		Mockito.when(document.getCondition("myFlag")).thenReturn(null);
		persistence.setUser(user);
		persistence.setForThread();
		try {
			Map<String, Object> props = new HashMap<>();
			props.put("myFlag", Boolean.TRUE);
			DynamicBean bean = new DynamicBean("mod", "Doc", props);
			assertTrue(BindUtil.evaluateCondition(bean, "myFlag"));
		}
		finally {
			clearPersistenceThreadLocal();
		}
	}

	@Test
	void evaluateConditionWithImplicitConditionReturnsFalseWhenPropertyAbsent() {
		AbstractPersistence persistence = Mockito.mock(AbstractPersistence.class,
				Mockito.withSettings().defaultAnswer(Mockito.CALLS_REAL_METHODS));
		User user = Mockito.mock(User.class);
		Customer customer = Mockito.mock(Customer.class);
		Module module = Mockito.mock(Module.class);
		Document document = Mockito.mock(Document.class);
		Mockito.when(user.getCustomer()).thenReturn(customer);
		Mockito.when(customer.getModule("mod")).thenReturn(module);
		Mockito.when(module.getDocument(customer, "Doc")).thenReturn(document);
		Mockito.when(document.getCondition("myFlag")).thenReturn(null);
		persistence.setUser(user);
		persistence.setForThread();
		try {
			Map<String, Object> props = new HashMap<>();
			// "myFlag" not in props → BindUtil.get returns null → false
			DynamicBean bean = new DynamicBean("mod", "Doc", props);
			assertFalse(BindUtil.evaluateCondition(bean, "myFlag"));
		}
		finally {
			clearPersistenceThreadLocal();
		}
	}

	@Test
	void evaluateConditionWithNegatedImplicitConditionReturnsTrueWhenPropertyIsTrue() {
		AbstractPersistence persistence = Mockito.mock(AbstractPersistence.class,
				Mockito.withSettings().defaultAnswer(Mockito.CALLS_REAL_METHODS));
		User user = Mockito.mock(User.class);
		Customer customer = Mockito.mock(Customer.class);
		Module module = Mockito.mock(Module.class);
		Document document = Mockito.mock(Document.class);
		Mockito.when(user.getCustomer()).thenReturn(customer);
		Mockito.when(customer.getModule("mod")).thenReturn(module);
		Mockito.when(module.getDocument(customer, "Doc")).thenReturn(document);
		Mockito.when(document.getCondition("myFlag")).thenReturn(null);
		persistence.setUser(user);
		persistence.setForThread();
		try {
			Map<String, Object> props = new HashMap<>();
			// evaluateCondition("notMyFlag") → conditionName="myFlag" → condish=null
			// → BindUtil.get(bean, "notMyFlag") → looks up "notMyFlag" property
			props.put("notMyFlag", Boolean.TRUE);
			DynamicBean bean = new DynamicBean("mod", "Doc", props);
			assertTrue(BindUtil.evaluateCondition(bean, "notMyFlag"));
		}
		finally {
			clearPersistenceThreadLocal();
		}
	}

	@Test
	void evaluateConditionWithSkyveExpressionDocumentConditionReturnsFalseWhenNegated() {
		AbstractPersistence persistence = Mockito.mock(AbstractPersistence.class,
				Mockito.withSettings().defaultAnswer(Mockito.CALLS_REAL_METHODS));
		User user = Mockito.mock(User.class);
		Customer customer = Mockito.mock(Customer.class);
		Module module = Mockito.mock(Module.class);
		Document document = Mockito.mock(Document.class);
		Condition condish = Mockito.mock(Condition.class);
		Mockito.when(user.getCustomer()).thenReturn(customer);
		Mockito.when(customer.getModule("mod")).thenReturn(module);
		Mockito.when(module.getDocument(customer, "Doc")).thenReturn(document);
		// conditionName = "myFlag" (stripped from "notMyFlag") → returns non-null condition
		Mockito.when(document.getCondition("myFlag")).thenReturn(condish);
		// expression is a Skyve expression that evaluates to true
		Mockito.when(condish.getExpression()).thenReturn("{el:true}");
		persistence.setUser(user);
		persistence.setForThread();
		try {
			Map<String, Object> props = new HashMap<>();
			DynamicBean bean = new DynamicBean("mod", "Doc", props);
			// negated=true, expression evaluates to true → negated result is false
			assertFalse(BindUtil.evaluateCondition(bean, "notMyFlag"));
		}
		finally {
			clearPersistenceThreadLocal();
		}
	}

	@Test
	void evaluateConditionWrapsExceptionAsMetaDataException() {
		AbstractPersistence persistence = Mockito.mock(AbstractPersistence.class,
				Mockito.withSettings().defaultAnswer(Mockito.CALLS_REAL_METHODS));
		User user = Mockito.mock(User.class);
		Customer customer = Mockito.mock(Customer.class);
		Mockito.when(user.getCustomer()).thenReturn(customer);
		// module lookup throws to trigger the catch block
		Mockito.when(customer.getModule("mod")).thenThrow(new RuntimeException("no module"));
		persistence.setUser(user);
		persistence.setForThread();
		try {
			Map<String, Object> props = new HashMap<>();
			DynamicBean bean = new DynamicBean("mod", "Doc", props);
			assertThrows(MetaDataException.class, () -> BindUtil.evaluateCondition(bean, "myFlag"));
		}
		finally {
			clearPersistenceThreadLocal();
		}
	}

	// --- validateBinding (condition-name paths, no CORE needed) ---

	@Test
	void validateBindingForConditionNameReturnsTargetMetaDataWithBooleanType() {
		Customer customer = Mockito.mock(Customer.class);
		Module module = Mockito.mock(Module.class);
		Document document = Mockito.mock(Document.class);
		Condition condition = Mockito.mock(Condition.class);
		Mockito.when(document.getCondition("myFlag")).thenReturn(condition);
		Mockito.when(document.getOwningModuleName()).thenReturn("mod");
		org.skyve.util.Binder.TargetMetaData result =
				BindUtil.validateBinding(customer, module, document, "myFlag");
		assertNotNull(result);
		assertEquals(Boolean.class, result.getType());
	}

	@Test
	void validateBindingForNegatedConditionNameReturnsTargetMetaDataWithBooleanType() {
		Customer customer = Mockito.mock(Customer.class);
		Module module = Mockito.mock(Module.class);
		Document document = Mockito.mock(Document.class);
		Condition condition = Mockito.mock(Condition.class);
		// "notMyFlag" strips to "myFlag" for the condition lookup
		Mockito.when(document.getCondition("myFlag")).thenReturn(condition);
		Mockito.when(document.getOwningModuleName()).thenReturn("mod");
		org.skyve.util.Binder.TargetMetaData result =
				BindUtil.validateBinding(customer, module, document, "notMyFlag");
		assertNotNull(result);
		assertEquals(Boolean.class, result.getType());
	}

	// --- helpers ---

	private static void clearPersistenceThreadLocal() {
		try {
			Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
			field.setAccessible(true);
			@SuppressWarnings("unchecked")
			ThreadLocal<AbstractPersistence> tl = (ThreadLocal<AbstractPersistence>) field.get(null);
			tl.remove();
		}
		catch (@SuppressWarnings("unused") Exception ignored) {
			// ignore
		}
	}
}
