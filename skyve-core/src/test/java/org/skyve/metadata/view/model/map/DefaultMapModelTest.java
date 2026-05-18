package org.skyve.metadata.view.model.map;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.withSettings;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.locationtech.jts.geom.Envelope;
import org.locationtech.jts.geom.Geometry;
import org.locationtech.jts.geom.GeometryFactory;
import org.skyve.impl.domain.AbstractPersistentBean;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;

@SuppressWarnings("static-method")
class DefaultMapModelTest {
	public static class TestBean extends AbstractPersistentBean {
		private static final long serialVersionUID = 1L;
		private String bizKey;
		private Geometry geometry;

		private TestBean(String id, String bizKey, Geometry geometry) {
			setBizId(id);
			this.bizKey = bizKey;
			this.geometry = geometry;
		}

		@Override
		public String getBizModule() {
			return "admin";
		}

		@Override
		public String getBizDocument() {
			return "Contact";
		}

		@Override
		public String getBizKey() {
			return bizKey;
		}

		@Override
		public void setBizKey(String bizKey) {
			this.bizKey = bizKey;
		}

		public Geometry getGeometry() {
			return geometry;
		}

		void setGeometry(Geometry geometry) {
			this.geometry = geometry;
		}
	}

	private static class TestDefaultMapModel extends DefaultMapModel<TestBean> {
		@Override
		public MapResult getResult(Geometry mapBounds) {
			return null;
		}

		void invokeAddItem(TestBean beanContainingGeometry, List<MapItem> itemsToAddTo, Envelope mapExtents)
		throws Exception {
			addItem(beanContainingGeometry, itemsToAddTo, mapExtents);
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

	@AfterEach
	void cleanup() {
		clearPersistenceThreadLocal();
	}

	@Test
	void geometryBindingRoundTrip() {
		TestDefaultMapModel model = new TestDefaultMapModel();
		model.setGeometryBinding("geometry");
		assertThat(model.getGeometryBinding(), is("geometry"));
	}

	@Test
	void addItemAddsFeatureWhenGeometryIntersectsMapExtents() throws Exception {
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		when(user.getCustomer()).thenReturn(customer);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		when(customer.getModule("admin")).thenReturn(module);
		when(module.getDocument(customer, "Contact")).thenReturn(document);

		AbstractPersistence persistence = mock(AbstractPersistence.class, withSettings().defaultAnswer(CALLS_REAL_METHODS));
		persistence.setUser(user);
		persistence.setForThread();

		GeometryFactory gf = new GeometryFactory();
		Geometry point = gf.createPoint(new org.locationtech.jts.geom.Coordinate(1, 1));
		TestBean bean = new TestBean("B1", "Bean Key", point);

		TestDefaultMapModel model = new TestDefaultMapModel();
		model.setGeometryBinding("geometry");

		List<MapItem> items = new ArrayList<>();
		model.invokeAddItem(bean, items, new Envelope(0, 2, 0, 2));

		assertEquals(1, items.size());
		MapItem item = items.get(0);
		assertThat(item.getBizId(), is("B1"));
		assertThat(item.getModuleName(), is("admin"));
		assertThat(item.getDocumentName(), is("Contact"));
		assertEquals(1, item.getFeatures().size());
	}

	@Test
	void addItemSkipsWhenGeometryOutsideMapExtents() throws Exception {
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		when(user.getCustomer()).thenReturn(customer);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		when(customer.getModule("admin")).thenReturn(module);
		when(module.getDocument(customer, "Contact")).thenReturn(document);

		AbstractPersistence persistence = mock(AbstractPersistence.class, withSettings().defaultAnswer(CALLS_REAL_METHODS));
		persistence.setUser(user);
		persistence.setForThread();

		GeometryFactory gf = new GeometryFactory();
		Geometry point = gf.createPoint(new org.locationtech.jts.geom.Coordinate(10, 10));
		TestBean bean = new TestBean("B2", "Bean Key", point);

		TestDefaultMapModel model = new TestDefaultMapModel();
		model.setGeometryBinding("geometry");

		List<MapItem> items = new ArrayList<>();
		model.invokeAddItem(bean, items, new Envelope(0, 2, 0, 2));

		assertEquals(0, items.size());
	}

	@Test
	void addItemSkipsWhenGeometryNull() throws Exception {
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		when(user.getCustomer()).thenReturn(customer);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		when(customer.getModule("admin")).thenReturn(module);
		when(module.getDocument(customer, "Contact")).thenReturn(document);

		AbstractPersistence persistence = mock(AbstractPersistence.class, withSettings().defaultAnswer(CALLS_REAL_METHODS));
		persistence.setUser(user);
		persistence.setForThread();

		TestBean bean = new TestBean("B3", "Bean Key", null);
		TestDefaultMapModel model = new TestDefaultMapModel();
		model.setGeometryBinding("geometry");

		List<MapItem> items = new ArrayList<>();
		model.invokeAddItem(bean, items, new Envelope(0, 2, 0, 2));

		assertEquals(0, items.size());
	}
}
