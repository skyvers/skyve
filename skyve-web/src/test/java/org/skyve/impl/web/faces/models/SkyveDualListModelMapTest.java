package org.skyve.impl.web.faces.models;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.primefaces.model.DualListModel;
import org.skyve.domain.Bean;
import org.skyve.domain.DynamicBean;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.web.AbstractWebContext;
import org.skyve.impl.web.faces.views.FacesView;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.metadata.model.document.Collection;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;

import jakarta.faces.FacesException;

@SuppressWarnings("static-method")
class SkyveDualListModelMapTest {
	@AfterEach
	@SuppressWarnings({"unchecked", "java:S3011"}) // Test cleanup must access the framework-owned thread-local
	void clearThreadPersistence() throws ReflectiveOperationException {
		Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
		field.setAccessible(true);
		((ThreadLocal<AbstractPersistence>) field.get(null)).remove();
	}

	@Test
	void getWithNullViewCreatesEmptyDualListModel() {
		SkyveDualListModelMap map = new SkyveDualListModelMap(null);

		DualListModel<?> model = map.get("roles");

		assertNotNull(model);
		assertTrue(model.getSource().isEmpty());
		assertTrue(model.getTarget().isEmpty());
		assertSame(model, map.get("roles"));
	}

	@Test
	void getWithNullCurrentBeanCreatesEmptyDualListModel() {
		FacesView view = mock(FacesView.class);
		when(view.getCurrentBean()).thenReturn(null);
		SkyveDualListModelMap map = new SkyveDualListModelMap(view);

		DualListModel<?> model = map.get("memberships");

		assertTrue(model.getSource().isEmpty());
		assertTrue(model.getTarget().isEmpty());
		assertEquals(1, map.size());
	}

	@Test
	void getWithNullBeanCreatesEmptyDualListModel() {
		FacesView view = mock(FacesView.class);
		BeanMapAdapter adapter = new BeanMapAdapter(null, null);
		when(view.getCurrentBean()).thenReturn(adapter);
		SkyveDualListModelMap map = new SkyveDualListModelMap(view);

		DualListModel<?> model = map.get("tags");

		assertTrue(model.getSource().isEmpty());
		assertTrue(model.getTarget().isEmpty());
	}

	@Test
	void gatherReturnsImmediatelyWhenViewIsNull() {
		SkyveDualListModelMap map = new SkyveDualListModelMap(null);
		map.put("roles", new DualListModel<>());

		map.gather();

		assertEquals(1, map.size());
	}

	@Test
	void gatherReturnsImmediatelyWhenCurrentBeanIsNull() {
		FacesView view = mock(FacesView.class);
		when(view.getCurrentBean()).thenReturn(null);
		SkyveDualListModelMap map = new SkyveDualListModelMap(view);
		map.put("roles", new DualListModel<>());

		map.gather();

		assertEquals(1, map.size());
	}

	@Test
	void gatherReturnsImmediatelyWhenWrappedBeanIsNull() {
		FacesView view = mock(FacesView.class);
		BeanMapAdapter adapter = new BeanMapAdapter(null, null);
		when(view.getCurrentBean()).thenReturn(adapter);
		SkyveDualListModelMap map = new SkyveDualListModelMap(view);
		map.put("roles", new DualListModel<>());

		map.gather();

		assertEquals(1, map.size());
	}

	@Test
	void getUsesExistingEntryWithoutReScatter() {
		FacesView view = mock(FacesView.class);
		Bean bean = mock(Bean.class);
		BeanMapAdapter adapter = new BeanMapAdapter(bean, null);
		when(view.getCurrentBean()).thenReturn(adapter);
		when(bean.getBizModule()).thenReturn("admin");
		when(bean.getBizDocument()).thenReturn("User");

		SkyveDualListModelMap map = new SkyveDualListModelMap(view);
		DualListModel<DomainValue> existing = new DualListModel<>();
		map.put("roles", existing);

		DualListModel<?> result = map.get("roles");

		assertSame(existing, result);
	}

	@Test
	void getWrapsScatterFailuresWithBindingContext() {
		FacesView view = mock(FacesView.class);
		Bean bean = mock(Bean.class);
		BeanMapAdapter adapter = new BeanMapAdapter(bean, null);
		when(view.getCurrentBean()).thenReturn(adapter);
		when(bean.getBizModule()).thenReturn("admin");
		when(bean.getBizDocument()).thenReturn("User");
		SkyveDualListModelMap map = new SkyveDualListModelMap(view);

		FacesException thrown = assertThrows(FacesException.class, () -> map.get("roles"));

		assertTrue(thrown.getMessage().contains("Could not scatter list membership roles"));
	}

	@Test
	void gatherThrowsWhenANewMemberMustBeFoundButTheViewHasNoWebContext() {
		GatherFixture fixture = new GatherFixture();
		when(fixture.view.getWebContext()).thenReturn(null);

		FacesException thrown = assertThrows(FacesException.class, fixture.map::gather);

		assertEquals("Could not gather list membership roles", thrown.getMessage());
		IllegalStateException cause = assertInstanceOf(IllegalStateException.class, thrown.getCause());
		assertEquals("Cannot gather list membership roles as the FacesView has no web context", cause.getMessage());
		assertTrue(fixture.roles.isEmpty());
	}

	@Test
	void gatherAddsANewMemberFoundThroughTheViewWebContext() {
		GatherFixture fixture = new GatherFixture();
		when(fixture.view.getWebContext()).thenReturn(mock(AbstractWebContext.class));
		Bean role = mock(Bean.class);
		doReturn(role).when(fixture.persistence).retrieve(fixture.roleDocument, "r1");

		fixture.map.gather();

		assertEquals(List.of(role), fixture.roles);
		assertFalse(fixture.map.containsKey("roles"));
	}

	/**
	 * A view of an admin.User bean with an empty "roles" collection of admin.Role and a membership model
	 * whose target holds role "r1".
	 */
	private static final class GatherFixture {
		final FacesView view = mock(FacesView.class);
		final Document roleDocument = mock(Document.class);
		final List<Bean> roles = new ArrayList<>();
		final AbstractPersistence persistence = mock(AbstractPersistence.class, CALLS_REAL_METHODS);
		final SkyveDualListModelMap map;

		@SuppressWarnings("boxing")
		GatherFixture() {
			User user = mock(User.class);
			Customer customer = mock(Customer.class);
			Module module = mock(Module.class);
			Document userDocument = mock(Document.class);
			Collection collection = mock(Collection.class);
			when(user.getCustomer()).thenReturn(customer);
			when(user.canReadBean(any(), any(), any(), any(), any(), any())).thenReturn(Boolean.TRUE);
			when(customer.getModule("admin")).thenReturn(module);
			when(module.getDocument(customer, "User")).thenReturn(userDocument);
			when(module.getDocument(customer, "Role")).thenReturn(roleDocument);
			when(userDocument.getAttribute("roles")).thenReturn(collection);
			when(collection.getDocumentName()).thenReturn("Role");
			when(roleDocument.isPersistable()).thenReturn(Boolean.TRUE);
			persistence.setUser(user);
			persistence.setForThread();

			Map<String, Object> values = new HashMap<>();
			values.put("roles", roles);
			when(view.getCurrentBean()).thenReturn(new BeanMapAdapter(new DynamicBean("admin", "User", values), null));
			map = new SkyveDualListModelMap(view);
			List<DomainValue> target = new ArrayList<>();
			target.add(new DomainValue("r1", "Role 1"));
			map.put("roles", new DualListModel<>(new ArrayList<>(), target));
		}
	}
}
