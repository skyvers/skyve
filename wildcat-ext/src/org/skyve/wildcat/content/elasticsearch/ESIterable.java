package org.skyve.wildcat.content.elasticsearch;

import java.text.ParseException;
import java.util.Iterator;

import org.elasticsearch.action.search.SearchResponse;
import org.elasticsearch.client.Client;
import org.elasticsearch.common.unit.TimeValue;
import org.elasticsearch.index.query.QueryBuilders;
import org.elasticsearch.search.SearchHit;
import org.elasticsearch.search.SearchHitField;
import org.elasticsearch.search.SearchHits;
import org.skyve.wildcat.content.ContentIterable;
import org.skyve.wildcat.content.SearchResult;
import org.skyve.wildcat.util.TimeUtil;
import org.skyve.wildcat.util.UtilImpl;

class ESIterable implements ContentIterable {
	private Client client = null;

	ESIterable(Client client) {
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

			String bizCustomer = (String) searchHit.field(ESClient.BEAN_CUSTOMER_NAME).value();
			String bizModule = (String) searchHit.field(ESClient.BEAN_MODULE_KEY).value();
			String bizDocument = (String) searchHit.field(ESClient.BEAN_DOCUMENT_KEY).value();
			String bizId = (String) searchHit.field(ESClient.BEAN_DOCUMENT_ID).value();

			SearchResult hit = new SearchResult();
			
			hit.setCustomerName(bizCustomer);
			hit.setModuleName(bizModule);
			hit.setDocumentName(bizDocument);
			hit.setBizId(bizId);

			hit.setExcerpt((String) searchHit.field(ESClient.CONTENT).value()); 

			SearchHitField field = searchHit.field(ESClient.BEAN_ATTRIBUTE_NAME);
			if (field != null) {
				hit.setAttributeName((String) field.getValue());
			}
			field = searchHit.field(ESClient.LAST_MODIFIED);
			if (field != null) {
				try {
					hit.setLastModified(TimeUtil.parseISODate((String) field.getValue()));
				}
				catch (ParseException e) {
					if (UtilImpl.CONTENT_TRACE) UtilImpl.LOGGER.info("ESIterable.ESIterator.next(): Could not parse ISO last modified date of " + field.getValue() + " for content ID = " + searchHit.getId());
				}
			}
			field = searchHit.field(ESClient.FILE_LAST_MODIFIED);
			if (field != null) {
				try {
					hit.setLastModified(TimeUtil.parseISODate((String) field.getValue()));
				}
				catch (ParseException e) {
					if (UtilImpl.CONTENT_TRACE) UtilImpl.LOGGER.info("ESIterable.ESIterator.next(): Could not parse ISO last modified date of " + field.getValue() + " for content ID = " + searchHit.getId());
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
