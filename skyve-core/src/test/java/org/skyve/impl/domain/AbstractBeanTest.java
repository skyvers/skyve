package org.skyve.impl.domain;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.withSettings;
import static org.mockito.Answers.CALLS_REAL_METHODS;

import java.io.Serializable;
import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.domain.Bean;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.user.User;

/**
 * Tests the pure-Java behaviour of {@link AbstractBean}:
 * dynamic-attribute map, identity, change tracking helpers, and list utilities.
 */
@SuppressWarnings("static-method")
class AbstractBeanTest {

	/** Minimal concrete subclass — implements only the abstract Bean contract. */
	private static class StubBean extends AbstractBean {
		private static final long serialVersionUID = 1L;

		private String bizId;
		private final String module;
		private final String document;
		private String customer = "test";
		private String dataGroupId;
		private String userId;

		StubBean(String module, String document, String bizId) {
			this.module = module;
			this.document = document;
			this.bizId = bizId;
		}

		@Override
		public String getBizId() {
			return bizId;
		}

		@Override
		public String getBizModule() {
			return module;
		}

		@Override
		public String getBizDocument() {
			return document;
		}

		@Override
		public String getBizCustomer() {
			return customer;
		}

		@Override
		public void setBizCustomer(String bizCustomer) {
			this.customer = bizCustomer;
		}

		@Override
		public String getBizDataGroupId() {
			return dataGroupId;
		}

		@Override
		public void setBizDataGroupId(String bizDataGroupId) {
			this.dataGroupId = bizDataGroupId;
		}

		@Override
		public String getBizUserId() {
			return userId;
		}

		@Override
		public void setBizUserId(String bizUserId) {
			this.userId = bizUserId;
		}

		@Override
		@SuppressWarnings("java:S4144")
		public String getBizKey() {
			return bizId;
		}

		void callPreset(String propertyName, Serializable value) {
			preset(propertyName, value);
		}
	}

	private StubBean bean;

	@BeforeEach
	void setUp() {
		bean = new StubBean("admin", "User", "id-001");
	}

	// ======== identity ========

	@Test
	void toStringIncludesBizId() {
		String s = bean.toString();
		assertTrue(s.contains("id-001"), "toString should contain bizId");
	}

	@Test
	void equalsReturnsTrueForSameBizId() {
		StubBean other = new StubBean("admin", "User", "id-001");
		assertEquals(bean, other);
	}

	@Test
	void equalsReturnsFalseForDifferentBizId() {
		StubBean other = new StubBean("admin", "User", "id-002");
		assertNotEquals(bean, other);
	}

	@Test
	void equalsReturnsFalseForNull() {
		assertNotEquals(null, bean);
	}

	@Test
	void equalsReturnsTrueForSameRef() {
		assertEquals(bean, bean);
	}

	@Test
	void hashCodeEqualsBizIdHashCode() {
		assertEquals("id-001".hashCode(), bean.hashCode());
	}

	@Test
	void compareToReturnsZeroForSameBizId() {
		StubBean other = new StubBean("x", "y", "id-001");
		assertEquals(0, bean.compareTo(other));
	}

	@Test
	void compareToReturnsNonZeroForDifferentBizId() {
		StubBean other = new StubBean("x", "y", "id-999");
		assertNotEquals(0, bean.compareTo(other));
	}

	@Test
	void staticCompareToHandlesBothNull() {
		assertEquals(0, AbstractBean.compareTo(null, null));
	}

	@Test
	void staticCompareToHandlesFirstNull() {
		StubBean b = new StubBean("m", "d", "id-1");
		assertTrue(AbstractBean.compareTo(null, b) < 0);
	}

	@Test
	void staticCompareToHandlesSecondNull() {
		StubBean b = new StubBean("m", "d", "id-1");
		assertTrue(AbstractBean.compareTo(b, null) > 0);
	}

	// ======== isCreated / isNotCreated ========

	@Test
	void isCreatedReturnsTrue() {
		assertTrue(bean.isCreated());
	}

	@Test
	void isNotCreatedReturnsFalse() {
		assertFalse(bean.isNotCreated());
	}

	// ======== originalValues ========

	@Test
	void originalValuesNotNullOnNewBean() {
		assertNotNull(bean.originalValues());
	}

	@Test
	void originalValuesEmptyOnNewBean() {
		assertTrue(bean.originalValues().isEmpty());
	}

	// ======== dynamic attribute map ========

	@Test
	void isDynamicFalseWhenNoDynamicAttributes() {
		assertFalse(bean.isDynamic("attr"));
	}

	@Test
	void putDynamicMakesDynamicTrue() {
		bean.putDynamic("color", "blue");
		assertTrue(bean.isDynamic("color"));
	}

	@Test
	void getDynamicReturnsValueAfterPut() {
		bean.putDynamic("size", "large");
		assertEquals("large", bean.getDynamic("size"));
	}

	@Test
	void getDynamicThrowsForUnknownAttribute() {
		assertThrows(IllegalArgumentException.class, () -> bean.getDynamic("unknown"));
	}

	@Test
	void setDynamicThrowsForUnknownAttribute() {
		assertThrows(IllegalArgumentException.class, () -> bean.setDynamic("unknown", "x"));
	}

	@Test
	void setDynamicUpdatesExistingAttribute() {
		bean.putDynamic("weight", "10kg");
		bean.setDynamic("weight", "20kg");
		assertEquals("20kg", bean.getDynamic("weight"));
	}

	@Test
	void getDynamicReturnsNullWhenValueIsNull() {
		bean.putDynamic("nullable", null);
		assertNull(bean.getDynamic("nullable"));
	}

	@Test
	void putDynamicAllowsOverwrite() {
		bean.putDynamic("key", "first");
		bean.putDynamic("key", "second");
		assertEquals("second", bean.getDynamic("key"));
	}

	@Test
	void putAllDynamicWithNullClearsDynamicMap() {
		bean.putDynamic("x", "1");
		bean.putAllDynamic(null);
		// after putAllDynamic(null), dynamic field is null → isDynamic returns false
		assertFalse(bean.isDynamic("x"));
	}

	@Test
	void putAllDynamicAddsAllEntries() {
		Map<String, Object> extra = new HashMap<>();
		extra.put("a", "alpha");
		extra.put("b", "beta");
		bean.putAllDynamic(extra);
		assertEquals("alpha", bean.getDynamic("a"));
		assertEquals("beta", bean.getDynamic("b"));
	}

	@Test
	void putAllDynamicMergesWithExisting() {
		bean.putDynamic("old", "oldVal");
		Map<String, Object> extra = new HashMap<>();
		extra.put("new", "newVal");
		bean.putAllDynamic(extra);
		assertEquals("oldVal", bean.getDynamic("old"));
		assertEquals("newVal", bean.getDynamic("new"));
	}

	// ======== getElementById / setElementById ========

	@Test
	void getElementByIdReturnsNullWhenNotFound() {
		List<StubBean> list = new ArrayList<>();
		list.add(new StubBean("m", "d", "a"));
		list.add(new StubBean("m", "d", "b"));
		Bean found = bean.getElementById(list, "nonexistent");
		assertNull(found);
	}

	@Test
	void getElementByIdReturnsMatchingBean() {
		StubBean target = new StubBean("m", "d", "target-id");
		List<StubBean> list = new ArrayList<>();
		list.add(new StubBean("m", "d", "other"));
		list.add(target);
		Bean found = bean.getElementById(list, "target-id");
		assertSame(target, found);
	}

	@Test
	void setElementByIdReplacesExistingElement() {
		StubBean original = new StubBean("m", "d", "same-id");
		StubBean replacement = new StubBean("m", "d", "same-id");
		List<StubBean> list = new ArrayList<>();
		list.add(original);
		bean.setElementById(list, replacement);
		assertSame(replacement, list.get(0));
	}

	@Test
	void setElementByIdDoesNotAddWhenNotPresent() {
		StubBean newBean = new StubBean("m", "d", "new-id");
		List<StubBean> list = new ArrayList<>();
		// setElementById only replaces; does not add if bizId not found
		bean.setElementById(list, newBean);
		assertEquals(0, list.size());
	}

	// ======== Serializable interface — originalValues is a Serializable field ========

	@Test
	void originalValuesIsSerializable() {
		assertTrue(bean.originalValues() instanceof Serializable);
	}

	// ======== evaluateCondition with literal booleans ========

	@Test
	void evaluateConditionReturnsTrueForLiteralTrue() {
		assertTrue(bean.evaluateCondition("true"));
	}

	@Test
	void evaluateConditionReturnsFalseForLiteralFalse() {
		assertFalse(bean.evaluateCondition("false"));
	}

	// ======== toString ========

	@Test
	void toStringContainsBizId() {
		assertTrue(bean.toString().contains("id-001"));
	}

	// ======== persistence-dependent: isChanged, isPersisted, isUserInDataGroup, isUserInRole ========

	private static void withMockPersistence(User mockUser, Runnable run) {
		AbstractPersistence persistence = mock(AbstractPersistence.class, withSettings().defaultAnswer(CALLS_REAL_METHODS));
		persistence.setUser(mockUser);
		persistence.setForThread();
		try {
			run.run();
		}
		finally {
			try {
				Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
				field.setAccessible(true);
				@SuppressWarnings("unchecked")
				ThreadLocal<AbstractPersistence> tl =
					(ThreadLocal<AbstractPersistence>) field.get(null);
				tl.remove();
			}
			catch (ReflectiveOperationException e) {
				throw new AssertionError(e);
			}
		}
	}

	@Test
	void isChangedReturnsTrueWhenOriginalValuesNonEmpty() {
		User mockUser = mock(User.class);
		Customer mockCustomer = mock(Customer.class);
		when(mockUser.getCustomer()).thenReturn(mockCustomer);
		withMockPersistence(mockUser, () -> {
			bean.callPreset("bizCustomer", "newValue");
			assertTrue(bean.isChanged());
		});
	}

	@Test
	void isNotChangedReturnsFalseWhenChanged() {
		User mockUser = mock(User.class);
		Customer mockCustomer = mock(Customer.class);
		when(mockUser.getCustomer()).thenReturn(mockCustomer);
		withMockPersistence(mockUser, () -> {
			bean.callPreset("bizCustomer", "newValue");
			assertFalse(bean.isNotChanged());
		});
	}

	@Test
	void isPersistedReturnsFalseForNonPersistentBean() {
		User mockUser = mock(User.class);
		withMockPersistence(mockUser, () -> {
			assertFalse(bean.isPersisted());
		});
	}

	@Test
	void isNotPersistedReturnsTrueForNonPersistentBean() {
		User mockUser = mock(User.class);
		withMockPersistence(mockUser, () -> {
			assertTrue(bean.isNotPersisted());
		});
	}

	@Test
	void isUserInDataGroupNullMatchesUserWithNullDataGroup() {
		User mockUser = mock(User.class);
		when(mockUser.getDataGroupId()).thenReturn(null);
		withMockPersistence(mockUser, () -> {
			assertTrue(bean.isUserInDataGroup(null));
		});
	}

	@Test
	void isUserInDataGroupStringDoesNotMatchUserWithNullDataGroup() {
		User mockUser = mock(User.class);
		when(mockUser.getDataGroupId()).thenReturn(null);
		withMockPersistence(mockUser, () -> {
			assertFalse(bean.isUserInDataGroup("groupA"));
		});
	}

	@Test
	void isUserInDataGroupMatchesWhenEqual() {
		User mockUser = mock(User.class);
		when(mockUser.getDataGroupId()).thenReturn("groupA");
		withMockPersistence(mockUser, () -> {
			assertTrue(bean.isUserInDataGroup("groupA"));
		});
	}

	@Test
	@SuppressWarnings("boxing")
	void isUserInRoleReturnsMockedValue() {
		User mockUser = mock(User.class);
		when(mockUser.isInRole("admin", "Administrator")).thenReturn(Boolean.TRUE);
		withMockPersistence(mockUser, () -> {
			assertTrue(bean.isUserInRole("admin", "Administrator"));
			assertFalse(bean.isUserInRole("admin", "UnknownRole"));
		});
	}

	@Test
	@SuppressWarnings("boxing")
	void isUserInOwningModuleRoleDelegatesToIsUserInRole() {
		User mockUser = mock(User.class);
		when(mockUser.isInRole("admin", "Manager")).thenReturn(Boolean.TRUE);
		withMockPersistence(mockUser, () -> {
			// bean.getBizModule() is "admin"
			assertTrue(bean.isUserInOwningModuleRole("Manager"));
			assertFalse(bean.isUserInOwningModuleRole("Developer"));
		});
	}

	// ======== preset() branches ========

	@Test
	void presetWithNullOldValueAndNonNullNewValueMarksOriginalValue() {
		// bizDataGroupId starts null; setting it to non-null should record the original (null) value
		bean.callPreset("bizDataGroupId", "groupX");
		assertTrue(bean.originalValues().containsKey("bizDataGroupId"));
		assertNull(bean.originalValues().get("bizDataGroupId"));
	}

	@Test
	void presetWithSameValueDoesNotMarkOriginalValue() {
		// bizCustomer starts "test"; presetting to the same value should not touch originalValues
		bean.callPreset("bizCustomer", "test");
		assertFalse(bean.originalValues().containsKey("bizCustomer"));
	}

	@Test
	void presetSecondCallForSamePropertyIsIgnored() {
		// First call records the original; second call must be a no-op
		bean.callPreset("bizCustomer", "first");
		bean.callPreset("bizCustomer", "second");
		// Original value should still be the one recorded on the first call ("test")
		assertTrue(bean.originalValues().containsKey("bizCustomer"));
		assertEquals("test", bean.originalValues().get("bizCustomer"));
	}
}
