package org.skyve.impl.persistence;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.List;

import org.junit.jupiter.api.Test;
import org.skyve.domain.Bean;
import org.skyve.domain.PersistentBean;
import org.skyve.metadata.model.document.Document;
import org.skyve.persistence.DocumentFilter;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.DocumentQuery.AggregateFunction;
import org.skyve.persistence.Persistence;

@SuppressWarnings("static-method")
class DocumentQueryProviderTest {
	@Test
	void getFiltersByBizIdAndReturnsSingleBean() {
		TestProvider provider = provider();
		PersistentBean bean = mock(PersistentBean.class);
		when(provider.query.beanResult()).thenReturn(bean);

		assertSame(bean, provider.get("BIZ-1"));
		verify(provider.filter).addEquals(Bean.DOCUMENT_ID, "BIZ-1");
	}

	@Test
	void getAllWithLimitSetsMaxResultsAndReturnsBeans() {
		TestProvider provider = provider();
		List<Bean> beans = List.of(mock(PersistentBean.class));
		when(provider.query.beanResults()).thenReturn(beans);

		assertSame(beans, provider.getAll(25));
		verify(provider.query).setMaxResults(25);
	}

	@Test
	void getAllReturnsBeansWithoutLimit() {
		TestProvider provider = provider();
		List<Bean> beans = List.of(mock(PersistentBean.class));
		when(provider.query.beanResults()).thenReturn(beans);

		assertSame(beans, provider.getAll());
		verify(provider.query, never()).setMaxResults(25);
	}

	@Test
	void countReturnsScalarCountOrZeroForNullResult() {
		TestProvider provider = provider();
		when(provider.query.scalarResult(Long.class)).thenReturn(Long.valueOf(7)).thenReturn(null);

		assertEquals(7L, provider.count());
		assertEquals(0L, provider.count());
		verify(provider.query, times(2)).addAggregateProjection(AggregateFunction.Count, Bean.DOCUMENT_ID, "count");
	}

	@Test
	void existsReflectsWhetherGetReturnsBean() {
		TestProvider provider = provider();
		when(provider.query.beanResult()).thenReturn(mock(PersistentBean.class)).thenReturn(null);

		assertTrue(provider.exists("BIZ-1"));
		assertFalse(provider.exists("BIZ-2"));
	}

	@Test
	void deleteDeletesExistingBeanOnly() throws Exception {
		TestProvider provider = provider();
		PersistentBean bean = mock(PersistentBean.class);
		when(provider.query.beanResult()).thenReturn(bean).thenReturn(null);

		provider.delete("BIZ-1");
		provider.delete("BIZ-2");

		verify(provider.persistence).delete(bean);
	}

	@Test
	void getDocumentQueryUsesConfiguredDocument() {
		TestProvider provider = provider();

		assertSame(provider.query, provider.getDocumentQuery());
		verify(provider.persistence).newDocumentQuery(provider.testDocument);
	}

	private static TestProvider provider() {
		Document document = mock(Document.class);
		Persistence persistence = mock(Persistence.class);
		DocumentQuery query = mock(DocumentQuery.class);
		DocumentFilter filter = mock(DocumentFilter.class);
		when(persistence.newDocumentQuery(document)).thenReturn(query);
		when(query.getFilter()).thenReturn(filter);
		return new TestProvider(document, persistence, query, filter);
	}

	private static final class TestProvider extends DocumentQueryProvider<PersistentBean> {
		private final Persistence persistence;
		private final Document testDocument;
		private final DocumentQuery query;
		private final DocumentFilter filter;

		private TestProvider(Document document, Persistence persistence, DocumentQuery query, DocumentFilter filter) {
			super(document);
			this.testDocument = document;
			this.persistence = persistence;
			this.query = query;
			this.filter = filter;
		}

		@Override
		protected Persistence getPersistence() {
			return persistence;
		}
	}
}
