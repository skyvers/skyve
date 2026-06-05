package modules.admin.DataMaintenance.models;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.Arrays;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import java.util.TreeMap;

import org.junit.jupiter.api.Test;
import org.skyve.CORE;
import org.skyve.content.ContentIterable;
import org.skyve.content.ContentManager;
import org.skyve.content.SearchResult;
import org.skyve.domain.Bean;
import org.skyve.domain.DynamicBean;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.types.OptimisticLock;
import org.skyve.impl.content.NoOpContentManager;
import org.skyve.metadata.view.model.list.Page;

import modules.admin.domain.Content;
import util.AbstractH2Test;

class ContentModelH2Test extends AbstractH2Test {
	@Test
	@SuppressWarnings("static-method")
	void postConstructPopulatesDrivingDocumentColumnsAndProjections() {
		ContentModel model = new ContentModel();

		model.postConstruct(CORE.getCustomer(), false);

		assertThat(model.getDescription(), is("All Content"));
		assertThat(model.getDrivingDocument().getName(), is(Content.DOCUMENT_NAME));
		assertEquals(8, model.getColumns().size());
		assertThat(model.getColumns().get(0).getBinding(), is(Content.customerNamePropertyName));
		assertThat(model.getColumns().get(1).getBinding(), is(Content.moduleNamePropertyName));
		assertThat(model.getColumns().get(2).getBinding(), is(Content.documentNamePropertyName));
		assertThat(model.getColumns().get(3).getBinding(), is(Content.attributeNamePropertyName));
		assertThat(model.getColumns().get(4).getBinding(), is(Content.contentBizIdPropertyName));
		assertThat(model.getColumns().get(5).getBinding(), is(Content.contentIdPropertyName));
		assertThat(model.getColumns().get(6).getBinding(), is(Content.lastModifiedPropertyName));
		assertThat(model.getColumns().get(7).getBinding(), is(Content.contentPropertyName));
		assertTrue(model.getProjections().contains(Bean.DOCUMENT_ID));
		assertTrue(model.getProjections().contains(Content.contentPropertyName));
		assertTrue(model.getProjections().contains(Content.lastModifiedPropertyName));
		assertNull(model.getFilter());
		assertNull(model.newFilter());
		model.putParameter("ignored", "ignored");
	}

	@Test
	@SuppressWarnings("static-method")
	void unsupportedListModelOperationsThrow() {
		ContentModel model = new ContentModel();
		TreeMap<String, Object> values = new TreeMap<>();

		assertThrows(IllegalStateException.class, model::iterate);
		assertThrows(IllegalStateException.class, () -> updateModel(model, values));
		assertThrows(IllegalStateException.class, () -> removeFromModel(model));
	}

	@Test
	@SuppressWarnings("static-method")
	void fetchBuildsPageRowsFromAccessibleContentResults() throws Exception {
		SearchResult skippedByStartRow = result("content-1", "biz-1", "text", new Date(1_000L));
		SearchResult includedWithLastModified = result("content-2", "biz-2", "markup", new Date(2_000L));
		SearchResult inaccessible = result("content-3", "biz-3", "memo", new Date(3_000L));
		SearchResult includedWithoutLastModified = result(null, "biz-4", "image", null);
		TestableContentModel model = new TestableContentModel(skippedByStartRow,
				includedWithLastModified,
				inaccessible,
				includedWithoutLastModified);
		model.setStartRow(1);
		model.setEndRow(4);

		Page page = model.fetch();

		assertEquals(4L, page.getTotalRows());
		assertEquals(2, page.getRows().size());
		assertThat(page.getSummary().getBizModule(), is(Content.MODULE_NAME));
		DynamicBean first = (DynamicBean) page.getRows().get(0);
		assertThat(first.getBizModule(), is(Content.MODULE_NAME));
		assertThat(first.getBizDocument(), is(Content.DOCUMENT_NAME));
		assertThat(first.get(Bean.DOCUMENT_ID), is("content-2"));
		assertThat(first.get(Content.contentBizIdPropertyName), is("biz-2"));
		assertThat(first.get(Content.contentIdPropertyName), is("content-2"));
		assertThat(first.get(Content.customerNamePropertyName), is(CORE.getCustomer().getName()));
		assertThat(first.get(Content.moduleNamePropertyName), is("admin"));
		assertThat(first.get(Content.documentNamePropertyName), is("User"));
		assertThat(first.get(Content.attributeNamePropertyName), is("markup"));
		assertThat(first.get(Content.contentPropertyName), is("excerpt-biz-2"));
		assertThat(first.get(Content.lastModifiedPropertyName), is(new org.skyve.domain.types.Timestamp(new Date(2_000L))));
		assertThat(((OptimisticLock) first.get(PersistentBean.LOCK_NAME)).getUsername(), is(CORE.getUser().getName()));

		DynamicBean second = (DynamicBean) page.getRows().get(1);
		assertThat(second.get(Bean.DOCUMENT_ID), is("biz-4"));
		assertNull(second.get(Content.contentIdPropertyName));
		assertNull(second.get(Content.lastModifiedPropertyName));
		assertThat(((OptimisticLock) second.get(PersistentBean.LOCK_NAME)).getUsername(), is(CORE.getUser().getName()));
	}

	@Test
	@SuppressWarnings("static-method")
	void fetchStopsAddingRowsWhenEndRowIsReached() throws Exception {
		TestableContentModel model = new TestableContentModel(result("content-1", "biz-1", "text", new Date(1_000L)),
				result("content-2", "biz-2", "markup", new Date(2_000L)),
				result("content-3", "biz-3", "memo", new Date(3_000L)));
		model.setStartRow(0);
		model.setEndRow(1);

		Page page = model.fetch();

		assertEquals(2, page.getRows().size());
	}

	private static void updateModel(ContentModel model, TreeMap<String, Object> values) throws Exception {
		model.update("id", values);
	}

	private static void removeFromModel(ContentModel model) throws Exception {
		model.remove("id");
	}

	private static SearchResult result(String contentId, String bizId, String attributeName, Date lastModified) {
		SearchResult result = new SearchResult();
		result.setContentId(contentId);
		result.setCustomerName(CORE.getCustomer().getName());
		result.setModuleName("admin");
		result.setDocumentName("User");
		result.setBizDataGroupId(CORE.getUser().getDataGroupId());
		result.setBizUserId(CORE.getUser().getId());
		result.setBizId(bizId);
		result.setAttributeName(attributeName);
		result.setExcerpt("excerpt-" + bizId);
		result.setLastModified(lastModified);
		return result;
	}

	private static class TestableContentModel extends ContentModel {
		private final List<SearchResult> results;

		private TestableContentModel(SearchResult... results) {
			this.results = Arrays.asList(results);
		}

		@Override
		@SuppressWarnings({"resource", "java:S1488"})
		protected ContentManager newContentManager() {
			ContentManager result = new NoOpContentManager() {
				@Override
				public ContentIterable all() {
					return () -> new ContentIterable.ContentIterator() {
						private final Iterator<SearchResult> iterator = results.iterator();

						@Override
						public boolean hasNext() {
							return iterator.hasNext();
						}

						@Override
						public SearchResult next() {
							return iterator.next();
						}

						@Override
						public long getTotalHits() {
							return results.size();
						}
					};
				}
			};
			return result;
		}

		@Override
		protected boolean canAccessContent(String bizCustomer,
											String bizModule,
											String bizDocument,
											String bizDataGroupId,
											String bizUserId,
											String bizId,
											String attributeName) {
			return !"biz-3".equals(bizId);
		}
	}
}
