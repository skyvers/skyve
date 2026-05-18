package org.skyve.metadata.view.model.map;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.Test;
import org.locationtech.jts.geom.Geometry;
import org.locationtech.jts.geom.GeometryFactory;
import org.skyve.impl.domain.AbstractPersistentBean;

@SuppressWarnings("static-method")
class ReferenceMapModelTest {
	public static class TestBean extends AbstractPersistentBean {
		private static final long serialVersionUID = 1L;
		private String bizKey;
		private Object reference;

		private TestBean(String id, String bizKey) {
			setBizId(id);
			this.bizKey = bizKey;
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

		public Object getReference() {
			return reference;
		}

		void setReference(Object reference) {
			this.reference = reference;
		}
	}

	private static class TestReferenceMapModel extends ReferenceMapModel<TestBean> {
		private int addItemCalls;

		private TestReferenceMapModel(String referenceBinding) {
			super(referenceBinding);
		}

		@Override
		protected void addItem(org.skyve.domain.Bean beanContainingGeometry,
							List<MapItem> itemsToAddTo,
							org.locationtech.jts.geom.Envelope mapExtents) {
			itemsToAddTo.add(new MapItem());
			addItemCalls++;
		}

		public int getAddItemCalls() {
			return addItemCalls;
		}
	}

	@Test
	void getResultHandlesCollectionReferenceBinding() throws Exception {
		TestBean owner = new TestBean("O1", "Owner");
		List<org.skyve.domain.Bean> references = new ArrayList<>();
		references.add(new TestBean("B1", "A"));
		references.add(new TestBean("B2", "B"));
		owner.setReference(references);

		TestReferenceMapModel model = new TestReferenceMapModel("reference");
		model.setBean(owner);

		Geometry mapBounds = new GeometryFactory().createPoint(new org.locationtech.jts.geom.Coordinate(0, 0));
		MapResult result = model.getResult(mapBounds);

		assertEquals(2, result.getItems().size());
		assertEquals(2, model.getAddItemCalls());
	}

	@Test
	void getResultHandlesSingularBeanReferenceBinding() throws Exception {
		TestBean owner = new TestBean("O2", "Owner");
		owner.setReference(new TestBean("B3", "C"));

		TestReferenceMapModel model = new TestReferenceMapModel("reference");
		model.setBean(owner);

		Geometry mapBounds = new GeometryFactory().createPoint(new org.locationtech.jts.geom.Coordinate(0, 0));
		MapResult result = model.getResult(mapBounds);

		assertEquals(1, result.getItems().size());
		assertEquals(1, model.getAddItemCalls());
	}

	@Test
	void getResultReturnsEmptyWhenReferenceIsNeitherBeanNorList() throws Exception {
		TestBean owner = new TestBean("O3", "Owner");
		owner.setReference("not-a-bean");

		TestReferenceMapModel model = new TestReferenceMapModel("reference");
		model.setBean(owner);

		Geometry mapBounds = new GeometryFactory().createPoint(new org.locationtech.jts.geom.Coordinate(0, 0));
		MapResult result = model.getResult(mapBounds);

		assertEquals(0, result.getItems().size());
		assertEquals(0, model.getAddItemCalls());
	}
}
