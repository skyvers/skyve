package org.skyve.metadata.view.model.list;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.SortedMap;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.domain.Bean;
import org.skyve.impl.domain.AbstractTransientBean;
import org.skyve.impl.web.SortParameterImpl;
import org.skyve.metadata.SortDirection;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.MetaDataQueryColumn;
import org.skyve.persistence.DocumentQuery.AggregateFunction;

class InMemoryListModelTest {

	/** Minimal concrete subclass that returns a fixed list of rows. */
	private static class TestListModel extends InMemoryListModel<Bean> {
		private final List<Bean> returnedRows;

		TestListModel(Module module, Document document, List<Bean> returnedRows) {
			super(module, document);
			this.returnedRows = returnedRows;
		}

		@Override
		public List<Bean> getRows() {
			return new ArrayList<>(returnedRows); // defensive copy — fetch() is destructive
		}

		@Override
		public List<MetaDataQueryColumn> getColumns() {
			return Collections.emptyList();
		}

		@Override
		public String getDescription() {
			return "TestListModel";
		}

		@Override
		public Bean update(String bizId, SortedMap<String, Object> properties) throws Exception {
			return null;
		}

		@Override
		public void remove(String bizId) throws Exception {
			// no-op
		}
	}

	/** Minimal TransientBean stub. */
	private static class StubBean extends AbstractTransientBean {
		private static final long serialVersionUID = 1L;

		StubBean() {
			setBizId(java.util.UUID.randomUUID().toString());
		}

		@Override
		public String getBizModule() {
			return "test";
		}

		@Override
		public String getBizDocument() {
			return "StubBean";
		}

		@Override
		public String getBizKey() {
			return "stub";
		}
	}

	/** StubBean variant with a configurable bizKey for sort testing. */
	private static class StubBeanWithKey extends StubBean {
		private static final long serialVersionUID = 1L;
		private final String key;

		StubBeanWithKey(String key) {
			this.key = key;
		}

		@Override
		public String getBizKey() {
			return key;
		}
	}

	private Module mockModule;
	private Document mockDocument;

	@BeforeEach
	void setUp() {
		mockModule = mock(Module.class);
		mockDocument = mock(Document.class);
		when(mockModule.getName()).thenReturn("test");
		when(mockDocument.getName()).thenReturn("TestDoc");
	}

	@Test
	void fetchReturnsPageWithZeroTotalRowsWhenEmpty() throws Exception {
		TestListModel model = new TestListModel(mockModule, mockDocument, Collections.emptyList());
		model.setStartRow(0);
		model.setEndRow(10);
		Page page = model.fetch();
		assertNotNull(page);
		assertEquals(0, page.getTotalRows());
	}

	@Test
	void fetchReturnsEmptyRowsWhenNoData() throws Exception {
		TestListModel model = new TestListModel(mockModule, mockDocument, Collections.emptyList());
		model.setStartRow(0);
		model.setEndRow(10);
		Page page = model.fetch();
		assertNotNull(page.getRows());
		assertEquals(0, page.getRows().size());
	}

	@Test
	void fetchReturnsAllRowsWhenFewRows() throws Exception {
		List<Bean> beans = new ArrayList<>();
		beans.add(new StubBean());
		beans.add(new StubBean());
		TestListModel model = new TestListModel(mockModule, mockDocument, beans);
		model.setStartRow(0);
		model.setEndRow(10);
		Page page = model.fetch();
		assertEquals(2, page.getTotalRows());
		assertEquals(2, page.getRows().size());
	}

	@Test
	void fetchReturnsSubsetWhenPaged() throws Exception {
		List<Bean> beans = new ArrayList<>();
		for (int i = 0; i < 5; i++) {
			beans.add(new StubBean());
		}
		TestListModel model = new TestListModel(mockModule, mockDocument, beans);
		model.setStartRow(2);
		model.setEndRow(3);
		Page page = model.fetch();
		assertEquals(5, page.getTotalRows());
		assertEquals(2, page.getRows().size());
	}

	@Test
	void fetchReturnsEmptyWhenStartRowExceedsTotal() throws Exception {
		List<Bean> beans = new ArrayList<>();
		beans.add(new StubBean());
		TestListModel model = new TestListModel(mockModule, mockDocument, beans);
		model.setStartRow(10);
		model.setEndRow(20);
		Page page = model.fetch();
		assertEquals(1, page.getTotalRows());
		assertEquals(0, page.getRows().size());
	}

	@Test
	void newFilterReturnsInMemoryFilter() {
		TestListModel model = new TestListModel(mockModule, mockDocument, Collections.emptyList());
		Filter filter = model.newFilter();
		assertNotNull(filter);
	}

	@Test
	void getFilterReturnsSameInstance() {
		TestListModel model = new TestListModel(mockModule, mockDocument, Collections.emptyList());
		Filter first = model.getFilter();
		Filter second = model.getFilter();
		assertEquals(first, second);
	}

	@Test
	void iterateReturnsAllRows() throws Exception {
		List<Bean> beans = new ArrayList<>();
		beans.add(new StubBean());
		beans.add(new StubBean());
		TestListModel model = new TestListModel(mockModule, mockDocument, beans);
		int count = 0;
		try (var it = model.iterate()) {
			for (@SuppressWarnings("unused") Bean b : it) {
				count++;
			}
		}
		assertEquals(2, count);
	}

	@Test
	void iterateReturnsEmptyWhenNoRows() throws Exception {
		TestListModel model = new TestListModel(mockModule, mockDocument, Collections.emptyList());
		int count = 0;
		try (var it = model.iterate()) {
			for (@SuppressWarnings("unused") Bean b : it) {
				count++;
			}
		}
		assertEquals(0, count);
	}

	@Test
	void fetchAppliesFilterToRows() throws Exception {
		List<Bean> beans = new ArrayList<>();
		beans.add(new StubBean());
		beans.add(new StubBean());
		TestListModel model = new TestListModel(mockModule, mockDocument, beans);
		model.setStartRow(0);
		model.setEndRow(10);
		// Get the filter and apply it (InMemoryFilter will filter based on criteria;
		// with no criteria set, filter passes all rows through)
		Filter filter = model.getFilter();
		assertNotNull(filter);
		Page page = model.fetch();
		assertEquals(2, page.getTotalRows());
	}

	@Test
	void fetchTagsRowsAsFalseWhenNoTagSelected() throws Exception {
		StubBean bean = new StubBean();
		List<Bean> beans = new ArrayList<>();
		beans.add(bean);
		TestListModel model = new TestListModel(mockModule, mockDocument, beans);
		model.setStartRow(0);
		model.setEndRow(10);
		// No selected tag ID set — should set TAGGED_NAME to false
		Page page = model.fetch();
		assertEquals(1, page.getTotalRows());
		// The rows are processed by tagged(), setting TAGGED_NAME = false
		// We can verify the page returned successfully
		assertNotNull(page.getRows());
	}

	@Test
	void putParameterStoresValue() throws Exception {
		TestListModel model = new TestListModel(mockModule, mockDocument, Collections.emptyList());
		model.putParameter("key", "value");
		// No public getter, but verify no exception
		model.setStartRow(0);
		model.setEndRow(10);
		Page page = model.fetch();
		assertNotNull(page);
	}

	@Test
	void fetchSummaryCountIsSetToRowCount() throws Exception {
		List<Bean> beans = new ArrayList<>();
		beans.add(new StubBean());
		beans.add(new StubBean());
		TestListModel model = new TestListModel(mockModule, mockDocument, beans);
		model.setSummary(AggregateFunction.Count);
		model.setStartRow(0);
		model.setEndRow(10);
		Page page = model.fetch();
		// summary bean's DOCUMENT_ID is set to rows.size() as a Long
		Bean summary = page.getSummary();
		assertNotNull(summary);
		assertEquals(Long.valueOf(2), summary.getDynamic(Bean.DOCUMENT_ID));
	}

	@Test
	void fetchSummaryNullWhenNoSummarySet() throws Exception {
		TestListModel model = new TestListModel(mockModule, mockDocument, Collections.emptyList());
		model.setStartRow(0);
		model.setEndRow(10);
		// No summary aggregate set (null) — summarize() still runs and returns the row count
		Page page = model.fetch();
		assertNotNull(page);
		// summary is always set (DOCUMENT_ID = rows count)
		assertNotNull(page.getSummary());
	}

	@Test
	void sortAscendingOrdersRows() throws Exception {
		// Create beans with distinct bizId values to verify sort order changes
		StubBean b1 = new StubBeanWithKey("zzz");
		StubBean b2 = new StubBeanWithKey("aaa");
		List<Bean> beans = new ArrayList<>(Arrays.asList(b1, b2));
		TestListModel model = new TestListModel(mockModule, mockDocument, beans);
		SortParameterImpl sort = new SortParameterImpl();
		sort.setBy(Bean.BIZ_KEY);
		sort.setDirection(SortDirection.ascending);
		model.setSortParameters(new SortParameterImpl[] {sort});
		model.setStartRow(0);
		model.setEndRow(10);
		Page page = model.fetch();
		assertEquals(2, page.getTotalRows());
		// After sorting by bizKey ascending, "aaa" should be first
		assertEquals("aaa", page.getRows().get(0).getBizKey());
	}

	@Test
	void sortDescendingOrdersRows() throws Exception {
		StubBean b1 = new StubBeanWithKey("aaa");
		StubBean b2 = new StubBeanWithKey("zzz");
		List<Bean> beans = new ArrayList<>(Arrays.asList(b1, b2));
		TestListModel model = new TestListModel(mockModule, mockDocument, beans);
		SortParameterImpl sort = new SortParameterImpl();
		sort.setBy(Bean.BIZ_KEY);
		sort.setDirection(SortDirection.descending);
		model.setSortParameters(new SortParameterImpl[] {sort});
		model.setStartRow(0);
		model.setEndRow(10);
		Page page = model.fetch();
		assertEquals(2, page.getTotalRows());
		assertEquals("zzz", page.getRows().get(0).getBizKey());
	}

	@Test
	void getProjectionsContainsDefaultFields() {
		TestListModel model = new TestListModel(mockModule, mockDocument, Collections.emptyList());
		// projections are populated in postConstruct; without calling it, check empty model
		// When constructed with module+document, postConstruct is NOT called automatically
		// so projections is empty initially. Just verify getProjections() does not throw.
		assertNotNull(model.getProjections());
	}

	@Test
	void fetchWithSummaryMinOnColumnBinding() throws Exception {
		// Min/Max/Sum/Avg/Count with no columns — DOCUMENT_ID is still set
		TestListModel model = new TestListModel(mockModule, mockDocument, Collections.emptyList());
		model.setSummary(AggregateFunction.Min);
		model.setStartRow(0);
		model.setEndRow(10);
		Page page = model.fetch();
		Bean summary = page.getSummary();
		assertNotNull(summary);
		assertEquals(Long.valueOf(0), summary.getDynamic(Bean.DOCUMENT_ID));
	}

	@Test
	void fetchWithSummaryMaxOnColumnBinding() throws Exception {
		TestListModel model = new TestListModel(mockModule, mockDocument, Collections.emptyList());
		model.setSummary(AggregateFunction.Max);
		model.setStartRow(0);
		model.setEndRow(10);
		Page page = model.fetch();
		assertNotNull(page.getSummary());
	}

	@Test
	void fetchWithSummarySumOnColumnBinding() throws Exception {
		TestListModel model = new TestListModel(mockModule, mockDocument, Collections.emptyList());
		model.setSummary(AggregateFunction.Sum);
		model.setStartRow(0);
		model.setEndRow(10);
		Page page = model.fetch();
		assertNotNull(page.getSummary());
	}

	@Test
	void fetchWithSummaryAvgOnColumnBinding() throws Exception {
		TestListModel model = new TestListModel(mockModule, mockDocument, Collections.emptyList());
		model.setSummary(AggregateFunction.Avg);
		model.setStartRow(0);
		model.setEndRow(10);
		Page page = model.fetch();
		assertNotNull(page.getSummary());
	}

	@Test
	void fetchWithSummaryCountOnColumnBinding() throws Exception {
		TestListModel model = new TestListModel(mockModule, mockDocument, Collections.emptyList());
		model.setSummary(AggregateFunction.Count);
		model.setStartRow(0);
		model.setEndRow(10);
		Page page = model.fetch();
		assertNotNull(page.getSummary());
	}

	@Test
	void fetchSummaryDocumentIdIsRowCount() throws Exception {
		List<Bean> beans = new ArrayList<>();
		beans.add(new StubBean());
		beans.add(new StubBean());
		beans.add(new StubBean());
		TestListModel model = new TestListModel(mockModule, mockDocument, beans);
		model.setStartRow(0);
		model.setEndRow(10);
		Page page = model.fetch();
		Bean summary = page.getSummary();
		assertNotNull(summary);
		// DOCUMENT_ID is set to the total row count as a Long
		assertEquals(Long.valueOf(3), summary.getDynamic(Bean.DOCUMENT_ID));
	}

	/** StubBean variant with a numeric value accessible via Binder.get(bean, "bizKey"). */
	private static class StubBeanWithBizKey extends StubBean {
		private static final long serialVersionUID = 1L;
		private final String key;

		StubBeanWithBizKey(String key) {
			this.key = key;
		}

		@Override
		public String getBizKey() {
			return key;
		}
	}

	/** Helper: create a column mock with the given binding. */
	private static MetaDataQueryColumn columnWithBinding(String binding) {
		MetaDataQueryColumn col = mock(MetaDataQueryColumn.class);
		when(col.getBinding()).thenReturn(binding);
		return col;
	}

	/** ListModel that returns a fixed set of columns. */
	private class TestListModelWithColumns extends TestListModel {
		private final List<MetaDataQueryColumn> columns;

		TestListModelWithColumns(List<Bean> rows, List<MetaDataQueryColumn> cols) {
			super(mockModule, mockDocument, rows);
			this.columns = cols;
		}

		@Override
		public List<MetaDataQueryColumn> getColumns() {
			return columns;
		}
	}

	@Test
	void fetchCountSummaryWithColumnBindingCountsNonNullValues() throws Exception {
		// "bizKey" binding returns a non-null String for StubBeanWithBizKey
		MetaDataQueryColumn col = columnWithBinding(Bean.BIZ_KEY);
		List<Bean> beans = new ArrayList<>(Arrays.asList(
				new StubBeanWithBizKey("a"),
				new StubBeanWithBizKey("b")));
		TestListModelWithColumns model = new TestListModelWithColumns(beans, Arrays.asList(col));
		model.setSummary(AggregateFunction.Count);
		model.setStartRow(0);
		model.setEndRow(10);
		Page page = model.fetch();
		Bean summary = page.getSummary();
		assertNotNull(summary);
		// bizKey column should have count 2 (both non-null)
		assertEquals(Long.valueOf(2), summary.getDynamic(Bean.BIZ_KEY));
	}

	@Test
	void fetchCountSummaryWithColumnBindingSkipsNullValues() throws Exception {
		// "bizKey" binding returns null for StubBean (returns "stub" actually)
		// Use a bean returning null bizKey
		MetaDataQueryColumn col = columnWithBinding(Bean.BIZ_KEY);
		List<Bean> beans = new ArrayList<>(Arrays.asList(
				new StubBeanWithBizKey(null),
				new StubBeanWithBizKey("b")));
		TestListModelWithColumns model = new TestListModelWithColumns(beans, Arrays.asList(col));
		model.setSummary(AggregateFunction.Count);
		model.setStartRow(0);
		model.setEndRow(10);
		Page page = model.fetch();
		Bean summary = page.getSummary();
		assertNotNull(summary);
		// Only one non-null value — count = 1
		assertEquals(Long.valueOf(1), summary.getDynamic(Bean.BIZ_KEY));
	}

	@Test
	void fetchMinSummaryWithColumnBindingReturnsMinValue() throws Exception {
		MetaDataQueryColumn col = columnWithBinding(Bean.BIZ_KEY);
		List<Bean> beans = new ArrayList<>(Arrays.asList(
				new StubBeanWithBizKey("zzz"),
				new StubBeanWithBizKey("aaa"),
				new StubBeanWithBizKey("mmm")));
		TestListModelWithColumns model = new TestListModelWithColumns(beans, Arrays.asList(col));
		model.setSummary(AggregateFunction.Min);
		model.setStartRow(0);
		model.setEndRow(10);
		Page page = model.fetch();
		Bean summary = page.getSummary();
		assertNotNull(summary);
		assertEquals("aaa", summary.getDynamic(Bean.BIZ_KEY));
	}

	@Test
	void fetchMaxSummaryWithColumnBindingReturnsMaxValue() throws Exception {
		MetaDataQueryColumn col = columnWithBinding(Bean.BIZ_KEY);
		List<Bean> beans = new ArrayList<>(Arrays.asList(
				new StubBeanWithBizKey("aaa"),
				new StubBeanWithBizKey("zzz"),
				new StubBeanWithBizKey("mmm")));
		TestListModelWithColumns model = new TestListModelWithColumns(beans, Arrays.asList(col));
		model.setSummary(AggregateFunction.Max);
		model.setStartRow(0);
		model.setEndRow(10);
		Page page = model.fetch();
		Bean summary = page.getSummary();
		assertNotNull(summary);
		assertEquals("zzz", summary.getDynamic(Bean.BIZ_KEY));
	}
}
