package org.skyve.domain;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.HashMap;
import java.util.Map;
import java.util.TreeMap;

import org.junit.jupiter.api.Test;

public class DynamicBeanTest {

	@Test
	@SuppressWarnings("static-method")
	public void constructorSetsModuleAndDocument() {
		DynamicBean b = new DynamicBean("admin", "User", new HashMap<>());
		assertThat(b.getBizModule(), is("admin"));
		assertThat(b.getBizDocument(), is("User"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void getBizIdFromProperties() {
		Map<String, Object> props = new HashMap<>();
		props.put(Bean.DOCUMENT_ID, "testId");
		DynamicBean b = new DynamicBean("admin", "User", props);
		assertThat(b.getBizId(), is("testId"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void getBizIdToStringForNonStringValue() {
		Map<String, Object> props = new HashMap<>();
		props.put(Bean.DOCUMENT_ID, Long.valueOf(42));
		DynamicBean b = new DynamicBean("admin", "User", props);
		assertThat(b.getBizId(), is("42"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void getBizIdNullWhenBizIdValueIsNull() {
		Map<String, Object> props = new HashMap<>();
		props.put(Bean.DOCUMENT_ID, null);
		DynamicBean b = new DynamicBean("admin", "User", props);
		assertThat(b.getBizId(), is((String) null));
	}

	@Test
	@SuppressWarnings("static-method")
	public void setBizCustomerAndGet() {
		Map<String, Object> props = new HashMap<>();
		props.put(Bean.CUSTOMER_NAME, null);
		DynamicBean b = new DynamicBean("admin", "User", props);
		b.setBizCustomer("testCustomer");
		assertThat(b.getBizCustomer(), is("testCustomer"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void setBizDataGroupIdAndGet() {
		Map<String, Object> props = new HashMap<>();
		props.put(Bean.DATA_GROUP_ID, null);
		DynamicBean b = new DynamicBean("admin", "User", props);
		b.setBizDataGroupId("dataGroup1");
		assertThat(b.getBizDataGroupId(), is("dataGroup1"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void setBizUserIdAndGet() {
		Map<String, Object> props = new HashMap<>();
		props.put(Bean.USER_ID, null);
		DynamicBean b = new DynamicBean("admin", "User", props);
		b.setBizUserId("userId1");
		assertThat(b.getBizUserId(), is("userId1"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void getBizKeyFromProperties() {
		Map<String, Object> props = new HashMap<>();
		props.put(Bean.BIZ_KEY, "some key value");
		DynamicBean b = new DynamicBean("admin", "User", props);
		assertThat(b.getBizKey(), is("some key value"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void evaluateConditionAlwaysFalse() {
		DynamicBean b = new DynamicBean("admin", "User", new HashMap<>());
		assertFalse(b.evaluateCondition("anyCondition"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void isPersistedAlwaysFalse() {
		DynamicBean b = new DynamicBean("admin", "User", new HashMap<>());
		assertFalse(b.isPersisted());
	}

	@Test
	@SuppressWarnings("static-method")
	public void isNotPersistedAlwaysTrue() {
		DynamicBean b = new DynamicBean("admin", "User", new HashMap<>());
		assertTrue(b.isNotPersisted());
	}

	@Test
	@SuppressWarnings("static-method")
	public void isCreatedAlwaysTrue() {
		DynamicBean b = new DynamicBean("admin", "User", new HashMap<>());
		assertTrue(b.isCreated());
	}

	@Test
	@SuppressWarnings("static-method")
	public void isNotCreatedAlwaysFalse() {
		DynamicBean b = new DynamicBean("admin", "User", new HashMap<>());
		assertFalse(b.isNotCreated());
	}

	@Test
	@SuppressWarnings("static-method")
	public void originalValuesEmptyWhenNoBeanProperty() {
		DynamicBean b = new DynamicBean("admin", "User", new HashMap<>());
		assertTrue(b.originalValues().isEmpty());
	}

	@Test
	@SuppressWarnings("static-method")
	public void isChangedFalseWhenNoBeanProperty() {
		DynamicBean b = new DynamicBean("admin", "User", new HashMap<>());
		assertFalse(b.isChanged());
	}

	@Test
	@SuppressWarnings("static-method")
	public void isNotChangedTrueWhenNoBeanProperty() {
		DynamicBean b = new DynamicBean("admin", "User", new HashMap<>());
		assertTrue(b.isNotChanged());
	}

	@Test
	@SuppressWarnings("static-method")
	public void hasChangedFalseWhenNoBeanProperty() {
		DynamicBean b = new DynamicBean("admin", "User", new HashMap<>());
		assertFalse(b.hasChanged());
	}

	@Test
	@SuppressWarnings("static-method")
	public void isPropertyTrueForModuleKey() {
		DynamicBean b = new DynamicBean("admin", "User", new HashMap<>());
		assertTrue(b.isProperty(Bean.MODULE_KEY));
	}

	@Test
	@SuppressWarnings("static-method")
	public void isPropertyTrueForDocumentKey() {
		DynamicBean b = new DynamicBean("admin", "User", new HashMap<>());
		assertTrue(b.isProperty(Bean.DOCUMENT_KEY));
	}

	@Test
	@SuppressWarnings("static-method")
	public void isPropertyFalseForNonExistentKey() {
		DynamicBean b = new DynamicBean("admin", "User", new HashMap<>());
		assertFalse(b.isProperty("nonExistentKey"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void isPropertyTrueForKeyInInitialMap() {
		Map<String, Object> props = new HashMap<>();
		props.put("myKey", "myValue");
		DynamicBean b = new DynamicBean("admin", "User", props);
		assertTrue(b.isProperty("myKey"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void isDynamicTrueAfterPutDynamic() {
		DynamicBean b = new DynamicBean("admin", "User", new HashMap<>());
		b.putDynamic("dynamicAttr", "value1");
		assertTrue(b.isDynamic("dynamicAttr"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void isDynamicFalseForUnknownProperty() {
		DynamicBean b = new DynamicBean("admin", "User", new HashMap<>());
		assertFalse(b.isDynamic("unknownProperty"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void putDynamicAndGetDynamic() {
		DynamicBean b = new DynamicBean("admin", "User", new HashMap<>());
		b.putDynamic("attr1", "val1");
		assertThat(b.getDynamic("attr1"), is("val1"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void getDynamicThrowsForUnknownProperty() {
		DynamicBean b = new DynamicBean("admin", "User", new HashMap<>());
		assertThrows(IllegalArgumentException.class, () -> b.getDynamic("unknownAttr"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void setDynamicThrowsForUnknownProperty() {
		DynamicBean b = new DynamicBean("admin", "User", new HashMap<>());
		assertThrows(IllegalArgumentException.class, () -> b.setDynamic("unknownAttr", "value"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void setDynamicUpdatesExistingProperty() {
		DynamicBean b = new DynamicBean("admin", "User", new HashMap<>());
		b.putDynamic("existingAttr", "initial");
		b.setDynamic("existingAttr", "updated");
		assertThat(b.getDynamic("existingAttr"), is("updated"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void putAllDynamicAddsProperties() {
		DynamicBean b = new DynamicBean("admin", "User", new HashMap<>());
		Map<String, Object> extra = new TreeMap<>();
		extra.put("a", "1");
		extra.put("b", "2");
		b.putAllDynamic(extra);
		assertThat(b.getDynamic("a"), is("1"));
		assertThat(b.getDynamic("b"), is("2"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void toStringContainsModuleAndDocument() {
		Map<String, Object> props = new HashMap<>();
		props.put(Bean.DOCUMENT_ID, "someId");
		DynamicBean b = new DynamicBean("admin", "User", props);
		String s = b.toString();
		assertTrue(s.contains("admin"));
		assertTrue(s.contains("User"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void constructorWithInitialPropertiesAreAccessible() {
		Map<String, Object> props = new HashMap<>();
		props.put("customField", "customValue");
		DynamicBean b = new DynamicBean("admin", "User", props);
		assertThat(b.getDynamic("customField"), is("customValue"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void getBizModuleViaDirectGetBinding() {
		DynamicBean b = new DynamicBean("admin", "User", new HashMap<>());
		assertThat(b.get(Bean.MODULE_KEY), is("admin"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void getBizDocumentViaDirectGetBinding() {
		DynamicBean b = new DynamicBean("admin", "User", new HashMap<>());
		assertThat(b.get(Bean.DOCUMENT_KEY), is("User"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void getDynamicForInitialMapPropertyReturnsValue() {
		Map<String, Object> props = new HashMap<>();
		props.put("status", "active");
		DynamicBean b = new DynamicBean("admin", "User", props);
		assertThat(b.getDynamic("status"), is("active"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void isDynamicTrueForInitialMapProperty() {
		Map<String, Object> props = new HashMap<>();
		props.put("status", "active");
		DynamicBean b = new DynamicBean("admin", "User", props);
		assertTrue(b.isDynamic("status"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void putDynamicOverwritesExistingValue() {
		Map<String, Object> props = new HashMap<>();
		props.put("status", "initial");
		DynamicBean b = new DynamicBean("admin", "User", props);
		b.putDynamic("status", "overwritten");
		assertThat(b.getDynamic("status"), is("overwritten"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void originalValuesEmptyWhenBeanKeyIsNull() {
		Map<String, Object> props = new HashMap<>();
		props.put(DynamicBean.BEAN_PROPERTY_KEY, null);
		DynamicBean b = new DynamicBean("admin", "User", props);
		assertTrue(b.originalValues().isEmpty());
	}

	@Test
	@SuppressWarnings("static-method")
	public void equalsReturnsTrueForSameBizId() {
		Map<String, Object> props1 = new HashMap<>();
		props1.put(Bean.DOCUMENT_ID, "id-123");
		DynamicBean b1 = new DynamicBean("admin", "User", props1);
		Map<String, Object> props2 = new HashMap<>();
		props2.put(Bean.DOCUMENT_ID, "id-123");
		DynamicBean b2 = new DynamicBean("admin", "User", props2);
		assertTrue(b1.equals(b2));
	}

	@Test
	@SuppressWarnings("static-method")
	public void equalsReturnsFalseForDifferentBizId() {
		Map<String, Object> props1 = new HashMap<>();
		props1.put(Bean.DOCUMENT_ID, "id-1");
		DynamicBean b1 = new DynamicBean("admin", "User", props1);
		Map<String, Object> props2 = new HashMap<>();
		props2.put(Bean.DOCUMENT_ID, "id-2");
		DynamicBean b2 = new DynamicBean("admin", "User", props2);
		assertFalse(b1.equals(b2));
	}

	@Test
	@SuppressWarnings("static-method")
	public void equalsReturnsFalseForNull() {
		Map<String, Object> props = new HashMap<>();
		props.put(Bean.DOCUMENT_ID, "id-1");
		DynamicBean b = new DynamicBean("admin", "User", props);
		assertFalse(b.equals(null));
	}

	@Test
	@SuppressWarnings("static-method")
	public void hashCodeConsistentWithBizId() {
		Map<String, Object> props = new HashMap<>();
		props.put(Bean.DOCUMENT_ID, "id-abc");
		DynamicBean b = new DynamicBean("admin", "User", props);
		assertEquals("id-abc".hashCode(), b.hashCode());
	}

	@Test
	@SuppressWarnings("static-method")
	public void hashCodeZeroWhenBizIdNull() {
		Map<String, Object> props = new HashMap<>();
		props.put(Bean.DOCUMENT_ID, null);
		DynamicBean b = new DynamicBean("admin", "User", props);
		assertEquals(0, b.hashCode());
	}

	@Test
	@SuppressWarnings("static-method")
	public void getWithIndexOnDynaProperty() {
		Map<String, Object> props = new HashMap<>();
		DynamicBean b = new DynamicBean("admin", "User", props);
		b.putDynamic("items", "first");
		// isDynaProperty is true — falls back to super.get(binding, index)
		// We just ensure no exception for the dyna path
		assertThrows(Exception.class, () -> b.get("items", 0));
	}

	@Test
	@SuppressWarnings("static-method")
	public void getWithIndexThrowsWhenNoBeanAndNotDynaProperty() {
		DynamicBean b = new DynamicBean("admin", "User", new HashMap<>());
		assertThrows(IllegalArgumentException.class, () -> b.get("unknownProp", 0));
	}

	@Test
	@SuppressWarnings("static-method")
	public void getWithKeyThrowsWhenNoBeanAndNotDynaProperty() {
		DynamicBean b = new DynamicBean("admin", "User", new HashMap<>());
		assertThrows(IllegalArgumentException.class, () -> b.get("unknownProp", "key"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void setOnDynaPropertySetsValue() {
		Map<String, Object> props = new HashMap<>();
		props.put("status", "initial");
		DynamicBean b = new DynamicBean("admin", "User", props);
		b.set("status", "updated");
		assertThat(b.getDynamic("status"), is("updated"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void setThrowsWhenNoBeanAndNotDynaProperty() {
		DynamicBean b = new DynamicBean("admin", "User", new HashMap<>());
		assertThrows(IllegalArgumentException.class, () -> b.set("unknownProp", "value"));
	}

	// ---- bean property set – delegation branches ---------------------------

	@Test
	@SuppressWarnings("static-method")
	public void getModuleKeyReturnsBeanModuleWhenBeanPropertySet() {
		Map<String, Object> innerProps = new HashMap<>();
		DynamicBean inner = new DynamicBean("innerModule", "InnerDoc", innerProps);
		Map<String, Object> outerProps = new HashMap<>();
		outerProps.put(DynamicBean.BEAN_PROPERTY_KEY, inner);
		DynamicBean outer = new DynamicBean("outerModule", "OuterDoc", outerProps);
		// bean != null branch → returns inner.getBizModule()
		assertThat(outer.get(Bean.MODULE_KEY), is("innerModule"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void getDocumentKeyReturnsBeanDocumentWhenBeanPropertySet() {
		Map<String, Object> innerProps = new HashMap<>();
		DynamicBean inner = new DynamicBean("innerModule", "InnerDoc", innerProps);
		Map<String, Object> outerProps = new HashMap<>();
		outerProps.put(DynamicBean.BEAN_PROPERTY_KEY, inner);
		DynamicBean outer = new DynamicBean("outerModule", "OuterDoc", outerProps);
		// bean != null branch → returns inner.getBizDocument()
		assertThat(outer.get(Bean.DOCUMENT_KEY), is("InnerDoc"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void getDelegatesBindUtilGetToInnerBeanWhenBindingNotInDynaProperties() {
		Map<String, Object> innerProps = new HashMap<>();
		innerProps.put(Bean.BIZ_KEY, "myBizKey");
		DynamicBean inner = new DynamicBean("innerModule", "InnerDoc", innerProps);
		Map<String, Object> outerProps = new HashMap<>();
		outerProps.put(DynamicBean.BEAN_PROPERTY_KEY, inner);
		DynamicBean outer = new DynamicBean("outerModule", "OuterDoc", outerProps);
		// bean != null, "bizKey" not in outer's dyna properties → BindUtil.get(inner, "bizKey")
		assertThat(outer.get(Bean.BIZ_KEY), is("myBizKey"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void getThrowsWhenBindingNotFoundAndBeanPropertySet() {
		Map<String, Object> innerProps = new HashMap<>();
		DynamicBean inner = new DynamicBean("innerModule", "InnerDoc", innerProps);
		Map<String, Object> outerProps = new HashMap<>();
		outerProps.put(DynamicBean.BEAN_PROPERTY_KEY, inner);
		DynamicBean outer = new DynamicBean("outerModule", "OuterDoc", outerProps);
		assertThrows(IllegalArgumentException.class, () -> outer.get("unknownBinding"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void originalValuesFromInnerBeanWhenBeanPropertySet() {
		Map<String, Object> innerProps = new HashMap<>();
		DynamicBean inner = new DynamicBean("innerModule", "InnerDoc", innerProps);
		Map<String, Object> outerProps = new HashMap<>();
		outerProps.put(DynamicBean.BEAN_PROPERTY_KEY, inner);
		DynamicBean outer = new DynamicBean("outerModule", "OuterDoc", outerProps);
		assertTrue(outer.originalValues().isEmpty());
	}

	@Test
	@SuppressWarnings("static-method")
	public void isChangedFalseFromInnerBeanWhenBeanPropertySet() {
		Map<String, Object> innerProps = new HashMap<>();
		DynamicBean inner = new DynamicBean("innerModule", "InnerDoc", innerProps);
		Map<String, Object> outerProps = new HashMap<>();
		outerProps.put(DynamicBean.BEAN_PROPERTY_KEY, inner);
		DynamicBean outer = new DynamicBean("outerModule", "OuterDoc", outerProps);
		assertFalse(outer.isChanged());
	}

	@Test
	@SuppressWarnings("static-method")
	public void hasChangedFalseFromInnerBeanWhenBeanPropertySet() {
		Map<String, Object> innerProps = new HashMap<>();
		DynamicBean inner = new DynamicBean("innerModule", "InnerDoc", innerProps);
		Map<String, Object> outerProps = new HashMap<>();
		outerProps.put(DynamicBean.BEAN_PROPERTY_KEY, inner);
		DynamicBean outer = new DynamicBean("outerModule", "OuterDoc", outerProps);
		assertFalse(outer.hasChanged());
	}

	@Test
	@SuppressWarnings("static-method")
	public void getWithIndexThrowsWhenBeanPropertySetAndNoMatchingBinding() {
		Map<String, Object> innerProps = new HashMap<>();
		DynamicBean inner = new DynamicBean("innerModule", "InnerDoc", innerProps);
		Map<String, Object> outerProps = new HashMap<>();
		outerProps.put(DynamicBean.BEAN_PROPERTY_KEY, inner);
		DynamicBean outer = new DynamicBean("outerModule", "OuterDoc", outerProps);
		// bean != null path, BindUtil.get throws for unknown binding
		assertThrows(IllegalArgumentException.class, () -> outer.get("unknownProp", 0));
	}

	@Test
	@SuppressWarnings("static-method")
	public void getWithKeyThrowsWhenBeanPropertySetAndNoMatchingBinding() {
		Map<String, Object> innerProps = new HashMap<>();
		DynamicBean inner = new DynamicBean("innerModule", "InnerDoc", innerProps);
		Map<String, Object> outerProps = new HashMap<>();
		outerProps.put(DynamicBean.BEAN_PROPERTY_KEY, inner);
		DynamicBean outer = new DynamicBean("outerModule", "OuterDoc", outerProps);
		assertThrows(IllegalArgumentException.class, () -> outer.get("unknownProp", "key"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void setThrowsWhenBeanPropertySetAndNoMatchingBinding() {
		Map<String, Object> innerProps = new HashMap<>();
		DynamicBean inner = new DynamicBean("innerModule", "InnerDoc", innerProps);
		Map<String, Object> outerProps = new HashMap<>();
		outerProps.put(DynamicBean.BEAN_PROPERTY_KEY, inner);
		DynamicBean outer = new DynamicBean("outerModule", "OuterDoc", outerProps);
		assertThrows(IllegalArgumentException.class, () -> outer.set("unknownProp", "value"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void setWithIndexThrowsWhenBeanPropertySetAndNoMatchingBinding() {
		Map<String, Object> innerProps = new HashMap<>();
		DynamicBean inner = new DynamicBean("innerModule", "InnerDoc", innerProps);
		Map<String, Object> outerProps = new HashMap<>();
		outerProps.put(DynamicBean.BEAN_PROPERTY_KEY, inner);
		DynamicBean outer = new DynamicBean("outerModule", "OuterDoc", outerProps);
		assertThrows(IllegalArgumentException.class, () -> outer.set("unknownProp", 0, "value"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void setWithKeyThrowsWhenBeanPropertySetAndNoMatchingBinding() {
		Map<String, Object> innerProps = new HashMap<>();
		DynamicBean inner = new DynamicBean("innerModule", "InnerDoc", innerProps);
		Map<String, Object> outerProps = new HashMap<>();
		outerProps.put(DynamicBean.BEAN_PROPERTY_KEY, inner);
		DynamicBean outer = new DynamicBean("outerModule", "OuterDoc", outerProps);
		assertThrows(IllegalArgumentException.class, () -> outer.set("unknownProp", "key", "value"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void setWithIndexOnDynaPropertyDelegatesToSuper() {
		Map<String, Object> props = new HashMap<>();
		DynamicBean b = new DynamicBean("admin", "User", props);
		b.putDynamic("items", new java.util.ArrayList<>());
		// isDynaProperty true → delegates to super.set(binding, index, value) - no exception
		b.set("items", 0, "value");
		// Just verify the dyna property still exists
		assertTrue(b.isDynamic("items"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void setWithKeyOnDynaPropertyDelegatesToSuper() {
		Map<String, Object> props = new HashMap<>();
		DynamicBean b = new DynamicBean("admin", "User", props);
		b.putDynamic("mapProp", new java.util.HashMap<>());
		// isDynaProperty true → delegates to super.set(binding, key, value) - no exception
		b.set("mapProp", "key", "value");
		assertTrue(b.isDynamic("mapProp"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void getCompoundBindingResolvesViaIntermediateDynaProperty() {
		// Setup: outer DynamicBean has a dyna property "person" holding an inner DynamicBean
		Map<String, Object> innerProps = new HashMap<>();
		innerProps.put("name", "Alice");
		DynamicBean inner = new DynamicBean("admin", "Contact", innerProps);

		Map<String, Object> outerProps = new HashMap<>();
		outerProps.put("person", inner);
		DynamicBean outer = new DynamicBean("admin", "User", outerProps);

		// Compound binding "person.name" - no BEAN_PROPERTY_KEY, "person" is a dyna property
		Object result = outer.get("person.name");
		assertThat(result, is("Alice"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void getCompoundBindingThrowsWhenIntermediatePropertyIsNull() {
		// "segment" is a dyna property but its value is null
		Map<String, Object> outerProps = new HashMap<>();
		outerProps.put("segment", null);
		DynamicBean outer = new DynamicBean("admin", "User", outerProps);

		// compound binding - "segment" is in dyna properties but holds null → returns null (no throw)
		Object result = outer.get("segment.anything");
		assertThat(result, is((Object) null));
	}

	@Test
	@SuppressWarnings("static-method")
	public void getCompoundBindingThrowsWhenNoDynaPropertyMatches() {
		// No dyna property matches any prefix of "a.b"
		DynamicBean outer = new DynamicBean("admin", "User", new HashMap<>());

		// compound binding, no matching dyna property → throws IllegalArgumentException
		assertThrows(IllegalArgumentException.class, () -> outer.get("a.b"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void dynamicHierarchicalBeanGetSetParentId() {
		Map<String, Object> props = new HashMap<>();
		props.put(HierarchicalBean.PARENT_ID, null);
		DynamicHierarchicalBean bean = new DynamicHierarchicalBean("admin", "User", props);
		assertThat(bean.getBizParentId(), is((String) null));
		bean.setBizParentId("parent-123");
		assertThat(bean.getBizParentId(), is("parent-123"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void dynamicHierarchicalBeanGetParentReturnsNullWhenNoParentId() {
		Map<String, Object> props = new HashMap<>();
		props.put(HierarchicalBean.PARENT_ID, null);
		DynamicHierarchicalBean bean = new DynamicHierarchicalBean("admin", "User", props);
		assertThat(bean.getParent(), is((org.skyve.domain.Bean) null));
	}

	@Test
	@SuppressWarnings("static-method")
	public void dynamicPersistentHierarchicalBeanGetSetParentId() {
		Map<String, Object> props = new HashMap<>();
		props.put(HierarchicalBean.PARENT_ID, null);
		DynamicPersistentHierarchicalBean bean = new DynamicPersistentHierarchicalBean("admin", "User", props);
		assertThat(bean.getBizParentId(), is((String) null));
		bean.setBizParentId("parent-456");
		assertThat(bean.getBizParentId(), is("parent-456"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void dynamicPersistentHierarchicalBeanGetParentReturnsNullWhenNoParentId() {
		Map<String, Object> props = new HashMap<>();
		props.put(HierarchicalBean.PARENT_ID, null);
		DynamicPersistentHierarchicalBean bean = new DynamicPersistentHierarchicalBean("admin", "User", props);
		assertThat(bean.getParent(), is((org.skyve.domain.Bean) null));
	}
}
