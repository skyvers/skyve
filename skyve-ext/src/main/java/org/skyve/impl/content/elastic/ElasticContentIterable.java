package org.skyve.impl.content.elastic;

import java.text.ParseException;
import java.util.Iterator;

import org.elasticsearch.action.search.SearchResponse;
import org.elasticsearch.client.Client;
import org.elasticsearch.common.unit.TimeValue;
import org.elasticsearch.index.query.QueryBuilders;
import org.elasticsearch.search.SearchHit;
import org.elasticsearch.search.SearchHits;
import org.skyve.content.ContentIterable;
import org.skyve.content.SearchResult;
import org.skyve.impl.util.TimeUtil;
import org.skyve.impl.util.UtilImpl;

class ElasticContentIterable implements ContentIterable {
	private Client client = null;

	ElasticContentIterable(Client client) {
		this.client = client;
	}
	
	class ESIterator implements ContentIterator {
		private Iterator<SearchHit> i = null;
		private long totalHits = 0;
		private String scrollId = null;
		
		private ESIterator() {
			@SuppressWarnings("synthetic-access")
			SearchResponse response = client.prepareSearch()
										.setIndices(ESClient.ATTACHMENT_INDEX_NAME, ESClient.BEAN_INDEX_NAME)
										.setTypes(ESClient.ATTACHMENT_INDEX_TYPE, ESClient.BEAN_INDEX_TYPE)
								        .setQuery(QueryBuilders.matchAllQuery())
								        .setSize(1000)
								        .setScroll(TimeValue.timeValueMinutes(2))
										.addFields(ESClient.BEAN_CUSTOMER_NAME,
													ESClient.BEAN_MODULE_KEY,
													ESClient.BEAN_DOCUMENT_KEY,
													ESClient.BEAN_DATA_GROUP_ID,
													ESClient.BEAN_USER_ID,
													ESClient.BEAN_DOCUMENT_ID,
													ESClient.BEAN_ATTRIBUTE_NAME,
													ESClient.LAST_MODIFIED,
													ESClient.FILE_LAST_MODIFIED,
													ESClient.CONTENT)
								        .execute()
								        .actionGet();
			scrollId = response.getScrollId();
			SearchHits hits = response.getHits();
			totalHits = hits.getTotalHits();
			i = hits.iterator();
		}

		@Override
		public boolean hasNext() {
			boolean hasNext = i.hasNext();

			if ((! hasNext) && (scrollId != null)) {
				@SuppressWarnings("synthetic-access")
				SearchResponse response = client.prepareSearchScroll(scrollId)
									        .setScroll(TimeValue.timeValueMinutes(2))
									        .execute()
									        .actionGet();
				SearchHits hits = response.getHits();
				i = hits.iterator();
				hasNext = i.hasNext();
				if (! hasNext) {
					@SuppressWarnings("synthetic-access")
					boolean success = client.prepareClearScroll().addScrollId(scrollId).execute().actionGet().isSucceeded();
					scrollId = null;
					if (! success) {
						throw new IllegalStateException("Could not clean up the ES scrollId");
					}
				}
			}

			return hasNext;
		}

		@Override
		public SearchResult next() {
			SearchHit searchHit = i.next();

			String bizCustomer = (String) ESClient.fieldValue(searchHit, ESClient.BEAN_CUSTOMER_NAME);
			String bizModule = (String) ESClient.fieldValue(searchHit, ESClient.BEAN_MODULE_KEY);
			String bizDocument = (String) ESClient.fieldValue(searchHit, ESClient.BEAN_DOCUMENT_KEY);
			String bizDataGroupId = (String) ESClient.fieldValue(searchHit, ESClient.BEAN_DATA_GROUP_ID);
			String bizUserId = (String) ESClient.fieldValue(searchHit, ESClient.BEAN_USER_ID);
			String bizId = (String) ESClient.fieldValue(searchHit, ESClient.BEAN_DOCUMENT_ID);

			SearchResult hit = new SearchResult();
			
			hit.setCustomerName(bizCustomer);
			hit.setModuleName(bizModule);
			hit.setDocumentName(bizDocument);
			hit.setBizDataGroupId(bizDataGroupId);
			hit.setBizUserId(bizUserId);
			hit.setBizId(bizId);

			hit.setExcerpt((String) ESClient.fieldValue(searchHit, ESClient.CONTENT));
			hit.setAttributeName((String) ESClient.fieldValue(searchHit, ESClient.BEAN_ATTRIBUTE_NAME));
			
			String lastModified = (String) ESClient.fieldValue(searchHit, ESClient.LAST_MODIFIED);
			if (lastModified != null) {
				try {
					hit.setLastModified(TimeUtil.parseISODate(lastModified));
				}
				catch (ParseException e) {
					if (UtilImpl.CONTENT_TRACE) UtilImpl.LOGGER.info("ESIterable.ESIterator.next(): Could not parse ISO last modified date of " + lastModified + " for content ID = " + searchHit.getId());
				}
			}
			String fileLastModified = (String) ESClient.fieldValue(searchHit, ESClient.FILE_LAST_MODIFIED);
			if (fileLastModified != null) {
				try {
					hit.setLastModified(TimeUtil.parseISODate(fileLastModified));
				}
				catch (ParseException e) {
					if (UtilImpl.CONTENT_TRACE) UtilImpl.LOGGER.info("ESIterable.ESIterator.next(): Could not parse ISO file last modified date of " + fileLastModified + " for content ID = " + searchHit.getId());
				}
			}
			hit.setContentId(searchHit.getId());

			return hit;
		}

		@Override
		public void remove() {
			throw new IllegalAccessError("Cannot remove from an ESIterator.");
		}
		
		@Override
		public long getTotalHits() {
			return totalHits;
		}
	}

	
	@Override
	@SuppressWarnings("synthetic-access")
	public ContentIterator iterator() {
		return new ESIterator();
	}
}
