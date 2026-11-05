package org.skyve.metadata.view.model.list;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.isNull;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.domain.Bean;
import org.skyve.impl.persistence.AbstractDocumentQuery;
import org.skyve.impl.web.SortParameterImpl;
import org.skyve.metadata.SortDirection;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.DocumentQuery.AggregateFunction;
import org.skyve.web.SortParameter;

/**
 * Tests {@link DocumentQueryListModel#fetch()} against mocked detail and summary queries.
 */
class DocumentQueryListModelTest {
	private DocumentQuery detailQuery;
	private AbstractDocumentQuery summaryQuery;
	private DocumentQueryListModel<Bean> model;

	@BeforeEach
	void setUp() {
		detailQuery = mock(DocumentQuery.class);
		summaryQuery = mock(AbstractDocumentQuery.class);
		MetaDataQueryDefinition query = mock(MetaDataQueryDefinition.class);
		when(query.getName()).thenReturn("qTest");
		when(query.constructDocumentQuery(isNull(), isNull())).thenReturn(detailQuery);
		when(query.constructDocumentQuery(AggregateFunction.Count, null)).thenReturn(summaryQuery);
		model = new DocumentQueryListModel<>(query);
		model.setStartRow(0);
		model.setEndRow(10);
	}

	@Test
	void fetchReportsNoRowsWhenTheSummaryQueryReturnsNoRow() throws Exception {
		List<Bean> rows = new ArrayList<>();
		when(detailQuery.<Bean>projectedResults()).thenReturn(rows);
		// projectedResult() is final and returns the first of projectedResults()
		when(summaryQuery.<Bean>projectedResults()).thenReturn(new ArrayList<>());

		Page page = model.fetch();

		assertEquals(0L, page.getTotalRows());
		assertSame(rows, page.getRows());
		assertNull(page.getSummary());
	}

	@Test
	void fetchSortsAscendingWhenTheSortParameterHasNoDirection() throws Exception {
		when(detailQuery.<Bean>projectedResults()).thenReturn(new ArrayList<>());
		SortParameterImpl sort = new SortParameterImpl();
		sort.setBy(Bean.BIZ_KEY);
		model.setSortParameters(new SortParameter[] {sort});

		model.fetch();

		verify(detailQuery).insertBoundOrdering(Bean.BIZ_KEY, SortDirection.ascending);
	}

	@Test
	void fetchThrowsWhenASortParameterHasNoBinding() {
		model.setSortParameters(new SortParameter[] {new SortParameterImpl()});

		IllegalStateException e = assertThrows(IllegalStateException.class, model::fetch);
		assertEquals("A sort parameter for query qTest has no binding to sort by", e.getMessage());
	}
}
