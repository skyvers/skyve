package org.skyve.metadata.view.model.map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Iterator;
import java.util.List;

import org.junit.jupiter.api.Test;
import org.locationtech.jts.geom.Geometry;
import org.locationtech.jts.geom.GeometryFactory;
import org.skyve.domain.Bean;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;
import org.skyve.persistence.AutoClosingIterable;
import org.skyve.persistence.DocumentQuery;

@SuppressWarnings("static-method")
class DocumentQueryMapModelTest {
	private static class ListAutoClosingIterable<T> implements AutoClosingIterable<T> {
		private final List<T> values;

		private ListAutoClosingIterable(List<T> values) {
			this.values = values;
		}

		@Override
		public Iterator<T> iterator() {
			return values.iterator();
		}

		@Override
		public void close() {
			// no-op
		}
	}

	private static class TestDocumentQueryMapModel extends DocumentQueryMapModel<Bean> {
		private int addItemCalls;

		private TestDocumentQueryMapModel() {
			super();
		}

		private TestDocumentQueryMapModel(MetaDataQueryDefinition query) {
			super(query);
		}

		@Override
		protected void addItem(Bean beanContainingGeometry, List<MapItem> itemsToAddTo, org.locationtech.jts.geom.Envelope mapExtents) {
			itemsToAddTo.add(new MapItem());
			addItemCalls++;
		}

		public int getAddItemCalls() {
			return addItemCalls;
		}

		public void setQueryPublic(MetaDataQueryDefinition query) {
			setQuery(query);
		}
	}

	@Test
	@SuppressWarnings("resource")
	void getResultConstructsQueryOnceAndAddsItemsFromIterable() throws Exception {
		MetaDataQueryDefinition query = mock(MetaDataQueryDefinition.class);
		DocumentQuery documentQuery = mock(DocumentQuery.class);
		when(query.constructDocumentQuery(null, null)).thenReturn(documentQuery);

		Bean b1 = mock(Bean.class);
		Bean b2 = mock(Bean.class);
		when(documentQuery.projectedIterable()).thenReturn(new ListAutoClosingIterable<>(List.of(b1, b2)));

		TestDocumentQueryMapModel model = new TestDocumentQueryMapModel(query);
		Geometry mapBounds = new GeometryFactory().createPoint(new org.locationtech.jts.geom.Coordinate(0, 0));

		MapResult first = model.getResult(mapBounds);
		MapResult second = model.getResult(mapBounds);

		assertEquals(2, first.getItems().size());
		assertEquals(2, second.getItems().size());
		assertEquals(4, model.getAddItemCalls());
		verify(query).constructDocumentQuery(null, null);
	}

	@Test
	@SuppressWarnings("resource")
	void setQueryAllowsPostConstructStyleConfiguration() throws Exception {
		MetaDataQueryDefinition initialQuery = mock(MetaDataQueryDefinition.class);
		MetaDataQueryDefinition replacementQuery = mock(MetaDataQueryDefinition.class);
		DocumentQuery documentQuery = mock(DocumentQuery.class);
		when(replacementQuery.constructDocumentQuery(null, null)).thenReturn(documentQuery);
		when(documentQuery.projectedIterable()).thenReturn(new ListAutoClosingIterable<>(List.of()));

		TestDocumentQueryMapModel model = new TestDocumentQueryMapModel(initialQuery);
		model.setQueryPublic(replacementQuery);

		Geometry mapBounds = new GeometryFactory().createPoint(new org.locationtech.jts.geom.Coordinate(0, 0));
		MapResult result = model.getResult(mapBounds);

		assertEquals(0, result.getItems().size());
		verify(replacementQuery).constructDocumentQuery(null, null);
	}

	@Test
	@SuppressWarnings("resource")
	void protectedNoArgConstructorSupportsPostConstructSetQuery() throws Exception {
		MetaDataQueryDefinition query = mock(MetaDataQueryDefinition.class);
		DocumentQuery documentQuery = mock(DocumentQuery.class);
		when(query.constructDocumentQuery(null, null)).thenReturn(documentQuery);
		when(documentQuery.projectedIterable()).thenReturn(new ListAutoClosingIterable<>(List.of()));

		TestDocumentQueryMapModel model = new TestDocumentQueryMapModel();
		model.setQueryPublic(query);

		Geometry mapBounds = new GeometryFactory().createPoint(new org.locationtech.jts.geom.Coordinate(0, 0));
		MapResult result = model.getResult(mapBounds);

		assertEquals(0, result.getItems().size());
		verify(query).constructDocumentQuery(null, null);
	}
}
