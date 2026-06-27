package org.apache.lucene.index;

import java.io.IOException;

public class ThrowingIndexReader extends IndexReader {
	@Override
	public Fields getTermVectors(int docID) throws IOException {
		return null;
	}

	@Override
	public TermVectors termVectors() throws IOException {
		return null;
	}

	@Override
	public int numDocs() {
		return 0;
	}

	@Override
	public int maxDoc() {
		return 0;
	}

	@Override
	public void document(int docID, StoredFieldVisitor visitor) throws IOException {
		// no-op
	}

	@Override
	public StoredFields storedFields() throws IOException {
		return null;
	}

	@Override
	protected void doClose() throws IOException {
		throw new IOException("boom");
	}

	@Override
	public IndexReaderContext getContext() {
		return null;
	}

	@Override
	public CacheHelper getReaderCacheHelper() {
		return null;
	}

	@Override
	public int docFreq(Term term) throws IOException {
		return 0;
	}

	@Override
	public long totalTermFreq(Term term) throws IOException {
		return 0L;
	}

	@Override
	public long getSumDocFreq(String field) throws IOException {
		return 0L;
	}

	@Override
	public int getDocCount(String field) throws IOException {
		return 0;
	}

	@Override
	public long getSumTotalTermFreq(String field) throws IOException {
		return 0L;
	}
}
