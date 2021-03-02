package org.skyve.impl.content.elastic;

import java.util.Collections;
import java.util.Iterator;

//import org.elasticsearch.action.search.SearchResponse;
//import org.elasticsearch.client.Client;
//import org.elasticsearch.common.unit.TimeValue;
//import org.elasticsearch.index.query.QueryBuilders;
//import org.elasticsearch.search.SearchHit;
//import org.elasticsearch.search.SearchHits;
import org.skyve.content.ContentIterable;
import org.skyve.content.SearchResult;

class ElasticContentIterable implements ContentIterable {
//	private Client client = null;
/*
	ElasticContentIterable(Client client) {
		this.client = client;
	}
*/

	class ESIterator implements ContentIterator {
//		private Iterator<SearchHit> i = null;
		private long totalHits = 0;
//		private String scrollId = null;
		
		private ESIterator() {
/*
			@SuppressWarnings("synthetic-access")
			SearchResponse response = client.prepareSearch()
										.setIndices(ElasticContentManager.ATTACHMENT_INDEX_NAME, ElasticContentManager.BEAN_INDEX_NAME)
										.setTypes(ElasticContentManager.ATTACHMENT_INDEX_TYPE, ElasticContentManager.BEAN_INDEX_TYPE)
								        .setQuery(QueryBuilders.matchAllQuery())
								        .setSize(1000)
								        .setScroll(TimeValue.timeValueMinutes(2))
										.addFields(ElasticContentManager.BEAN_CUSTOMER_NAME,
													ElasticContentManager.BEAN_MODULE_KEY,
													ElasticContentManager.BEAN_DOCUMENT_KEY,
													ElasticContentManager.BEAN_DATA_GROUP_ID,
													ElasticContentManager.BEAN_USER_ID,
													ElasticContentManager.BEAN_DOCUMENT_ID,
													ElasticContentManager.BEAN_ATTRIBUTE_NAME,
													ElasticContentManager.LAST_MODIFIED,
													ElasticContentManager.FILE_LAST_MODIFIED,
													ElasticContentManager.CONTENT)
								        .execute()
								        .actionGet();
			scrollId = response.getScrollId();
			SearchHits hits = response.getHits();
			totalHits = hits.getTotalHits();
			i = hits.iterator();
*/
		}

		@Override
		public boolean hasNext() {
return false;
/*
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
*/
		}

		@Override
		public SearchResult next() {
/*
			SearchHit searchHit = i.next();

			String bizCustomer = (String) ElasticContentManager.fieldValue(searchHit, ElasticContentManager.BEAN_CUSTOMER_NAME);
			String bizModule = (String) ElasticContentManager.fieldValue(searchHit, ElasticContentManager.BEAN_MODULE_KEY);
			String bizDocument = (String) ElasticContentManager.fieldValue(searchHit, ElasticContentManager.BEAN_DOCUMENT_KEY);
			String bizDataGroupId = (String) ElasticContentManager.fieldValue(searchHit, ElasticContentManager.BEAN_DATA_GROUP_ID);
			String bizUserId = (String) ElasticContentManager.fieldValue(searchHit, ElasticContentManager.BEAN_USER_ID);
			String bizId = (String) ElasticContentManager.fieldValue(searchHit, ElasticContentManager.BEAN_DOCUMENT_ID);

			SearchResult hit = new SearchResult();
			
			hit.setCustomerName(bizCustomer);
			hit.setModuleName(bizModule);
			hit.setDocumentName(bizDocument);
			hit.setBizDataGroupId(bizDataGroupId);
			hit.setBizUserId(bizUserId);
			hit.setBizId(bizId);

			hit.setExcerpt((String) ElasticContentManager.fieldValue(searchHit, ElasticContentManager.CONTENT));
			hit.setAttributeName((String) ElasticContentManager.fieldValue(searchHit, ElasticContentManager.BEAN_ATTRIBUTE_NAME));
			
			String lastModified = (String) ElasticContentManager.fieldValue(searchHit, ElasticContentManager.LAST_MODIFIED);
			if (lastModified != null) {
				try {
					hit.setLastModified(TimeUtil.parseISODate(lastModified));
				}
				catch (@SuppressWarnings("unused") ParseException e) {
					if (UtilImpl.CONTENT_TRACE) UtilImpl.LOGGER.info("ESIterable.ESIterator.next(): Could not parse ISO last modified date of " + lastModified + " for content ID = " + searchHit.getId());
				}
			}
			String fileLastModified = (String) ElasticContentManager.fieldValue(searchHit, ElasticContentManager.FILE_LAST_MODIFIED);
			if (fileLastModified != null) {
				try {
					hit.setLastModified(TimeUtil.parseISODate(fileLastModified));
				}
				catch (@SuppressWarnings("unused") ParseException e) {
					if (UtilImpl.CONTENT_TRACE) UtilImpl.LOGGER.info("ESIterable.ESIterator.next(): Could not parse ISO file last modified date of " + fileLastModified + " for content ID = " + searchHit.getId());
				}
			}
			hit.setContentId(searchHit.getId());

			return hit;
*/
return null;
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
