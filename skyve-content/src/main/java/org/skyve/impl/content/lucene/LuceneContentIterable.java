package org.skyve.impl.content.lucene;

import java.io.IOException;

import org.apache.lucene.document.Document;
import org.apache.lucene.index.DirectoryReader;
import org.apache.lucene.index.IndexReader;
import org.apache.lucene.search.IndexSearcher;
import org.apache.lucene.search.MatchAllDocsQuery;
import org.apache.lucene.search.ScoreDoc;
import org.apache.lucene.store.Directory;
import org.skyve.content.ContentIterable;
import org.skyve.content.SearchResult;
import org.skyve.domain.Bean;
import org.skyve.domain.messages.DomainException;
import org.skyve.impl.content.AbstractContentManager;
import org.skyve.impl.util.TimeUtil;

public class LuceneContentIterable implements ContentIterable {
	private Directory directory = null;
	
	public LuceneContentIterable(Directory directory) {
		this.directory = directory;
	}
	
	class LuceneContentIterator implements ContentIterator {
		private final int PAGE_SIZE = 1000;
		
		private IndexReader reader = null;
		private long totalHits = 0L;
		private IndexSearcher searcher = null;
		private ScoreDoc[] scoreDocs = null;
		private int index = 0;
		
		private LuceneContentIterator() {
			try {
				reader = DirectoryReader.open(directory);
			}
			catch (IOException e) {
				throw new DomainException("Cannot open the content index for read to get all documents", e);
			}
			searcher = new IndexSearcher(reader);
			totalHits = reader.numDocs();
		}
		
		@Override
		public SearchResult next() {
			if ((scoreDocs != null) && (scoreDocs.length > index)) {
				int doc = scoreDocs[index++].doc;
				try {
					Document document = reader.document(doc);
					SearchResult result = new SearchResult();
					result.setAttributeName(document.get(AbstractContentManager.ATTRIBUTE_NAME));
					result.setBizDataGroupId(document.get(Bean.DATA_GROUP_ID));
					result.setBizId(document.get(Bean.DOCUMENT_ID));
					result.setBizUserId(document.get(Bean.USER_ID));
					result.setContentId(document.get(AbstractContentManager.CONTENT_ID));
					result.setCustomerName(document.get(Bean.CUSTOMER_NAME));
					result.setDocumentName(document.get(Bean.DOCUMENT_KEY));
					result.setExcerpt(document.get(AbstractContentManager.CONTENT));
					result.setLastModified(TimeUtil.parseISODate(document.get(AbstractContentManager.LAST_MODIFIED)));
					result.setModuleName(document.get(Bean.MODULE_KEY));
					return result;
				}
				catch (Exception e) {
					throw new DomainException("Cannot get the document " + doc + " from the content index", e);
				}
			}
			return null;
		}
		
		@Override
		public boolean hasNext() {
			if (scoreDocs == null) { // never issued a query
				try {
					scoreDocs = searcher.search(new MatchAllDocsQuery(), PAGE_SIZE).scoreDocs;
				}
				catch (IOException e) {
					throw new DomainException("Cannot get the first page of documents from the content index", e);
				}
				index = 0;
			}
			else if (scoreDocs.length == index) { // exhausted this page
				final ScoreDoc after = scoreDocs[index - 1]; // get the last one searched in the last page
				try {
					scoreDocs = searcher.searchAfter(after, new MatchAllDocsQuery(), PAGE_SIZE).scoreDocs;
				}
				catch (IOException e) {
					throw new DomainException("Cannot get a subsequent page of documents from the content index", e);
				}
				index = 0;
			}

			boolean more = scoreDocs.length > index;
			if ((! more) && (reader != null)) {
				try {
					reader.close();
				}
				catch (IOException e) {
					throw new DomainException("Cannot close the reader from the content index", e);
				}
			}
			return more;
		}
		
		@Override
		public long getTotalHits() {
			return totalHits;
		}
	}
	
	@Override
	public ContentIterator iterator() {
		return new LuceneContentIterator();
	}
}
