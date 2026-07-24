package org.skyve.impl.web.faces.actions;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.skyve.domain.Bean;
import org.skyve.domain.DynamicBean;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.web.faces.views.FacesView;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;
import org.skyve.metadata.user.User;


@SuppressWarnings({ "static-method", "java:S1192" }) // Repeated values are deliberate navigation fixtures.
class ActionUtilTest {
	@AfterEach
	void tearDown() {
		clearThreadPersistence();
	}

	@Test
	void getTargetBeanForViewReturnsNullWhenViewBeanIsNull() {
		FacesView facesView = mock(FacesView.class);
		when(facesView.getBean()).thenReturn(null);

		assertNull(ActionUtil.getTargetBeanForView(facesView));
	}

	@Test
	void getTargetBeanForViewReturnsRootBeanWhenNoBindings() {
		FacesView facesView = mock(FacesView.class);
		Bean bean = mock(Bean.class);
		when(facesView.getBean()).thenReturn(bean);
		when(facesView.getViewBinding()).thenReturn(null);

		assertEquals(bean, ActionUtil.getTargetBeanForView(facesView));
	}

	@Test
	void getTargetBeanForViewResolvesViewBinding() {
		Map<String, Object> rootValues = new HashMap<>();
		DynamicBean child = new DynamicBean("admin", "Child", new HashMap<>());
		DynamicBean root = new DynamicBean("admin", "Root", rootValues);
		rootValues.put("child", child);
		FacesView facesView = mock(FacesView.class);

		when(facesView.getBean()).thenReturn(root);
		when(facesView.getViewBinding()).thenReturn("child");

		assertSame(child, ActionUtil.getTargetBeanForView(facesView));
	}

	@Test
	void getTargetBeanForViewAndReferenceBindingResolvesNestedReference() {
		Map<String, Object> rootValues = new HashMap<>();
		Map<String, Object> levelValues = new HashMap<>();
		DynamicBean child = new DynamicBean("admin", "Child", new HashMap<>());
		DynamicBean level = new DynamicBean("admin", "Level", levelValues);
		DynamicBean root = new DynamicBean("admin", "Root", rootValues);
		rootValues.put("level", level);
		levelValues.put("child", child);
		FacesView facesView = mock(FacesView.class);

		when(facesView.getBean()).thenReturn(root);
		when(facesView.getViewBinding()).thenReturn("level");

		assertSame(child, ActionUtil.getTargetBeanForViewAndReferenceBinding(facesView, "child"));
	}

	@Test
	void getTargetBeanForViewAndReferenceBindingResolvesCollectionElementByBizId() {
		Map<String, Object> rootValues = new HashMap<>();
		DynamicBean root = new DynamicBean("admin", "Root", rootValues);
		Bean one = mock(Bean.class);
		Bean two = mock(Bean.class);
		FacesView facesView = mock(FacesView.class);

		when(one.getBizId()).thenReturn("1");
		when(two.getBizId()).thenReturn("2");
		rootValues.put("items", List.of(one, two));
		when(facesView.getBean()).thenReturn(root);
		when(facesView.getViewBinding()).thenReturn(null);

		assertSame(two, ActionUtil.getTargetBeanForViewAndReferenceBinding(facesView, "items", "2"));
	}

	@Test
	void setTargetBeanForViewAndCollectionBindingSetsBeanWhenNoViewBinding() {
		FacesView facesView = mock(FacesView.class);
		DynamicBean oldBean = new DynamicBean("admin", "Root", new HashMap<>());
		DynamicBean newBean = new DynamicBean("admin", "Root", new HashMap<>());

		when(facesView.getBean()).thenReturn(oldBean);
		when(facesView.getViewBinding()).thenReturn(null);

		ActionUtil.setTargetBeanForViewAndCollectionBinding(facesView, null, newBean);

		verify(facesView).setBean(newBean);
	}

	@Test
	void setTargetBeanForViewAndCollectionBindingSetsNestedViewBinding() {
		Map<String, Object> rootValues = new HashMap<>();
		DynamicBean root = new DynamicBean("admin", "Root", rootValues);
		DynamicBean oldChild = new DynamicBean("admin", "Child", new HashMap<>());
		DynamicBean newChild = new DynamicBean("admin", "Child", new HashMap<>());
		FacesView facesView = mock(FacesView.class);
		rootValues.put("child", oldChild);

		when(facesView.getBean()).thenReturn(root);
		when(facesView.getViewBinding()).thenReturn("child");

		ActionUtil.setTargetBeanForViewAndCollectionBinding(facesView, null, newChild);

		assertSame(newChild, org.skyve.util.Binder.get(root, "child"));
	}

	@Test
	void setTargetBeanForViewAndCollectionBindingWithCollectionNameExecutesBranch() {
		Map<String, Object> rootValues = new HashMap<>();
		DynamicBean root = new DynamicBean("admin", "Root", rootValues);
		Bean oldElement = mock(Bean.class);
		Bean newElement = mock(Bean.class);
		FacesView facesView = mock(FacesView.class);

		when(oldElement.getBizId()).thenReturn("a1");
		when(newElement.getBizId()).thenReturn("a1");
		rootValues.put("items", new java.util.ArrayList<>(List.of(oldElement)));
		when(facesView.getBean()).thenReturn(root);
		when(facesView.getViewBinding()).thenReturn(null);

		assertThrows(Exception.class,
				() -> ActionUtil.setTargetBeanForViewAndCollectionBinding(facesView, "items", newElement));
	}

	@Test
	void getMetaDataQueryReturnsDirectQueryWhenPresent() {
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		MetaDataQueryDefinition query = mock(MetaDataQueryDefinition.class);
		bindPersistenceForUser(user);

		when(user.getCustomer()).thenReturn(customer);
		when(customer.getModule("admin")).thenReturn(module);
		when(module.getMetaDataQuery("q")).thenReturn(query);

		assertEquals(query, ActionUtil.getMetaDataQuery("admin", "q"));
	}

	@Test
	void getMetaDataQueryFallsBackToDefaultQueryWhenNamedMissing() {
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		MetaDataQueryDefinition query = mock(MetaDataQueryDefinition.class);
		bindPersistenceForUser(user);

		when(user.getCustomer()).thenReturn(customer);
		when(customer.getModule("admin")).thenReturn(module);
		when(module.getMetaDataQuery("q")).thenReturn(null);
		when(module.getDocumentDefaultQuery(customer, "q")).thenReturn(query);

		assertEquals(query, ActionUtil.getMetaDataQuery("admin", "q"));
	}

	private static void bindPersistenceForUser(User user) {
		AbstractPersistence persistence = mock(AbstractPersistence.class, CALLS_REAL_METHODS);
		persistence.setUser(user);
		persistence.setForThread();
	}

	@SuppressWarnings({ "unchecked", "java:S3011" }) // Reflection clears the private persistence thread-local after each test.
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
}
