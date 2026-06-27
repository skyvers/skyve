package org.skyve.impl.web.faces.models;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.mock;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.Map;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.skyve.domain.Bean;
import org.skyve.domain.DynamicBean;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.view.TextOutput.Sanitisation;
import org.skyve.metadata.user.User;

import jakarta.faces.context.FacesContext;
import jakarta.faces.context.PartialViewContext;

@SuppressWarnings("static-method")
class BeanMapAdapterTest {
	private static final class StubBean extends org.skyve.impl.domain.AbstractBean {
		private static final long serialVersionUID = 1L;
		private String bizId;
		private String bizCustomer;
		private String bizDataGroupId;
		private String bizUserId;
		private Bean name;
		private String label;

		@Override
		public String getBizId() {
			return bizId;
		}

		@SuppressWarnings("unused")
		public void setBizId(String bizId) {
			this.bizId = bizId;
		}

		@Override
		public String getBizModule() {
			return "sales";
		}

		@Override
		public String getBizDocument() {
			return "Order";
		}

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
			return null;
		}

		@SuppressWarnings("unused")
		public Bean getName() {
			return name;
		}

		@SuppressWarnings("unused")
		public void setName(Bean name) {
			this.name = name;
		}

		@SuppressWarnings("unused")
		public String getLabel() {
			return label;
		}

		@SuppressWarnings("unused")
		public void setLabel(String label) {
			this.label = label;
		}
	}
	private abstract static class FacesContextBridge extends FacesContext {
		static void setCurrent(FacesContext context) {
			setCurrentInstance(context);
		}
	}

	@AfterEach
	void tearDown() {
		FacesContextBridge.setCurrent(null);
		clearThreadPersistence();
	}

	@Test
	void mapOperationsUseDelegateState() {
		Bean bean = mock(Bean.class);
		BeanMapAdapter adapter = new BeanMapAdapter(bean, null);

		assertTrue(adapter.isEmpty());
		adapter.putAll(Map.of("a", "alpha", "b", "beta"));
		assertEquals(2, adapter.size());
		assertTrue(adapter.containsKey("a"));
		assertTrue(adapter.containsValue("beta"));
		assertTrue(adapter.keySet().contains("a"));
		assertTrue(adapter.values().contains("alpha"));
		assertEquals(2, adapter.entrySet().size());
		assertEquals("alpha", adapter.remove("a"));
		assertFalse(adapter.containsKey("a"));
		adapter.clear();
		assertTrue(adapter.isEmpty());
	}

	@Test
	void setBeanResetsDelegateCache() {
		Bean bean1 = mock(Bean.class);
		Bean bean2 = mock(Bean.class);
		BeanMapAdapter adapter = new BeanMapAdapter(bean1, null);
		adapter.putAll(Map.of("k", "v"));

		adapter.setBean(bean2);

		assertEquals(bean2, adapter.getBean());
		assertTrue(adapter.isEmpty());
	}

	@Test
	void equalsAndHashCodeDelegateToUnderlyingBean() {
		Bean bean1 = mock(Bean.class);
		Bean bean2 = mock(Bean.class);

		BeanMapAdapter adapter1 = new BeanMapAdapter(bean1, null);
		BeanMapAdapter adapter2 = new BeanMapAdapter(bean2, null);

		assertEquals(bean1.hashCode(), adapter1.hashCode());
		assertEquals(adapter1, adapter1);
		assertNotEquals(adapter1, adapter2);
	}

	@Test
	void getResolvesStringBeanAndListBindings() {
		FacesContextBridge.setCurrent(createFacesContext());
		bindPersistenceForUser(mock(User.class));

		HashMap<String, Object> rootValues = new HashMap<>();
		HashMap<String, Object> childValues = new HashMap<>();
		DynamicBean child = new DynamicBean("sales", "Child", childValues);
		childValues.put("name", "child-one");
		DynamicBean root = new DynamicBean("sales", "Order", rootValues);
		rootValues.put("name", "root-name");
		rootValues.put("child", child);
		rootValues.put("children", new ArrayList<>(java.util.List.of(child)));

		BeanMapAdapter adapter = new BeanMapAdapter(root, null);

		assertEquals("root-name", invokeGet(adapter, "name", false, Sanitisation.none));
		Object nested = invokeGet(adapter, "child", false, Sanitisation.none);
		assertTrue(nested instanceof BeanMapAdapter);
		assertSame(child, ((BeanMapAdapter) nested).getBean());

		Object list = invokeGet(adapter, "children", false, Sanitisation.none);
		assertTrue(list instanceof java.util.List<?>);
		java.util.List<?> adaptedList = (java.util.List<?>) list;
		assertEquals(1, adaptedList.size());
		assertTrue(adaptedList.get(0) instanceof BeanMapAdapter);
		assertSame(child, ((BeanMapAdapter) adaptedList.get(0)).getBean());
	}

	@Test
	void getCanEscapeStringValuesAndNormaliseEscapedOpeningBraces() {
		FacesContextBridge.setCurrent(createFacesContext());
		bindPersistenceForUser(mock(User.class));

		HashMap<String, Object> values = new HashMap<>();
		values.put("label", "<b>\\{status}</b>");
		DynamicBean bean = new DynamicBean("sales", "Order", values);
		BeanMapAdapter adapter = new BeanMapAdapter(bean, null);

		assertEquals("&lt;b&gt;{status}&lt;/b&gt;", adapter.get("label", true, "none"));
	}

	@Test
	void putConvertsBeanMapAdapterAndUpdatesUnderlyingBean() {
		FacesContextBridge.setCurrent(createFacesContext());
		bindPersistenceForUser(mock(User.class));

		StubBean root = new StubBean();
		StubBean child = new StubBean();
		BeanMapAdapter childAdapter = new BeanMapAdapter(child, null);
		BeanMapAdapter adapter = new BeanMapAdapter(root, null);

		invokeSet(adapter, "name", childAdapter);
		invokeSet(adapter, "label", "  updated-name  ");

		assertSame(root, adapter.getBean());
		assertSame(child, childAdapter.getBean());
	}

	private static Object invokeGet(BeanMapAdapter adapter, String key, boolean escape, Sanitisation sanitisation) {
		try {
			Method method = BeanMapAdapter.class.getDeclaredMethod("get", Object.class, boolean.class, Sanitisation.class);
			method.setAccessible(true);
			return method.invoke(adapter, key, Boolean.valueOf(escape), sanitisation);
		}
		catch (Exception e) {
			throw new IllegalStateException("Unable to invoke BeanMapAdapter.get", e);
		}
	}

	private static void invokeSet(BeanMapAdapter adapter, String binding, Object value) {
		try {
			Method method = BeanMapAdapter.class.getDeclaredMethod("set", String.class, Object.class);
			method.setAccessible(true);
			method.invoke(adapter, binding, value);
		}
		catch (Exception e) {
			throw new IllegalStateException("Unable to invoke BeanMapAdapter.set", e);
		}
	}

	private static void bindPersistenceForUser(User user) {
		Customer customer = mock(Customer.class);
		org.mockito.Mockito.when(user.getCustomer()).thenReturn(customer);
		AbstractPersistence persistence = mock(AbstractPersistence.class, CALLS_REAL_METHODS);
		persistence.setUser(user);
		persistence.setForThread();
	}

	@SuppressWarnings("unchecked")
	private static void clearThreadPersistence() {
		try {
			Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
			field.setAccessible(true);
			ThreadLocal<AbstractPersistence> threadLocal = (ThreadLocal<AbstractPersistence>) field.get(null);
			threadLocal.remove();
		}
		catch (Exception e) {
			throw new IllegalStateException("Unable to clear thread local persistence", e);
		}
	}

	private static FacesContext createFacesContext() {
		FacesContext context = mock(FacesContext.class);
		PartialViewContext partialViewContext = mock(PartialViewContext.class);
		org.mockito.Mockito.when(partialViewContext.getRenderIds()).thenReturn(new LinkedHashSet<>());
		org.mockito.Mockito.when(context.getPartialViewContext()).thenReturn(partialViewContext);
		return context;
	}
}
