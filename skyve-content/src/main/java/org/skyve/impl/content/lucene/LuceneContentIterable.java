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

/**
 * Iterates over all records in the Lucene content index.
 *
 * <p>Iteration streams results in pages to avoid loading the full index result set into
 * memory at once.
 */
public class LuceneContentIterable implements ContentIterable {
	private Directory directory = null;
	
	/**
	 * Creates an iterable for the supplied Lucene directory.
	 *
	 * @param directory the Lucene directory containing content records
	 */
	public LuceneContentIterable(Directory directory) {
		this.directory = directory;
	}
	
	/**
	 * Provides paged iteration over Lucene documents.
	 */
	class LuceneContentIterator implements ContentIterator {
		private final int PAGE_SIZE = 1000;
		
		private IndexReader reader = null;
		private long totalHits = 0L;
		private IndexSearcher searcher = null;
		private ScoreDoc[] scoreDocs = null;
		private int index = 0;
		
		/**
		 * Opens an index reader and prepares search state.
		 */
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
		
		/**
		 * Returns the next indexed content result from the current page.
		 *
		 * @return the next result, or {@code null} when no more results remain
		 */
		@Override
		public SearchResult next() {
			if ((scoreDocs != null) && (scoreDocs.length > index)) {
				int doc = scoreDocs[index++].doc;
				try {
					Document document = reader.storedFields().document(doc);
					SearchResult result = new SearchResult();
					result.setAttributeName(document.get(AbstractContentManager.ATTRIBUTE_NAME));
					result.setBizDataGroupId(document.get(Bean.DATA_GROUP_ID));
					result.setContentId(document.get(AbstractContentManager.CONTENT_ID));
					String bizId = document.get(Bean.DOCUMENT_ID);
					if (result.isAttachment()) { // attachment content
						result.setBizId(bizId);
					}
					else { // bean content
						result.setBizId(bizId.substring(0, bizId.length() - 1)); // remove the '~'
					}
					result.setModuleName(document.get(Bean.MODULE_KEY));
					result.setDocumentName(document.get(Bean.DOCUMENT_KEY));
					result.setCustomerName(document.get(Bean.CUSTOMER_NAME));
					result.setBizUserId(document.get(Bean.USER_ID));
					result.setExcerpt(document.get(AbstractContentManager.CONTENT));
					String lastModified = document.get(AbstractContentManager.LAST_MODIFIED);
					if (lastModified != null) {
						result.setLastModified(TimeUtil.parseISODate(lastModified));
					}
					return result;
				}
				catch (Exception e) {
					throw new DomainException("Cannot get the document " + doc + " from the content index", e);
				}
			}
			return null;
		}
		
		/**
		 * Loads the first page lazily and then subsequent pages as needed.
		 *
		 * @return {@code true} when more results are available
		 */
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
		
		/**
		 * Returns the total number of indexed documents at iterator creation time.
		 *
		 * @return total indexed document count
		 */
		@Override
		public long getTotalHits() {
			return totalHits;
		}
	}
	
	/**
	 * Returns an iterator over indexed content.
	 *
	 * @return a new Lucene content iterator
	 */
	@Override
	public ContentIterator iterator() {
		return new LuceneContentIterator();
	}
}
