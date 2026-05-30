package org.skyve.impl.content.lucene;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import org.apache.lucene.analysis.standard.StandardAnalyzer;
import org.apache.lucene.document.Document;
import org.apache.lucene.document.Field;
import org.apache.lucene.document.StoredField;
import org.apache.lucene.document.StringField;
import org.apache.lucene.index.IndexWriter;
import org.apache.lucene.index.IndexWriterConfig;
import org.apache.lucene.search.IndexSearcher;
import org.apache.lucene.search.ScoreDoc;
import org.apache.lucene.store.ByteBuffersDirectory;
import org.apache.lucene.store.Directory;
import org.junit.jupiter.api.Test;
import org.skyve.content.ContentIterable.ContentIterator;
import org.skyve.content.SearchResult;
import org.skyve.domain.Bean;
import org.skyve.domain.messages.DomainException;
import org.skyve.impl.content.AbstractContentManager;
import org.skyve.impl.util.TimeUtil;

@SuppressWarnings("static-method")
class LuceneContentIterableTest {
	@Test
	void testIteratorReturnsAttachmentAndBeanContentShapes() throws Exception {
		try (Directory directory = new ByteBuffersDirectory()) {
			try (StandardAnalyzer analyzer = new StandardAnalyzer();
					IndexWriter writer = new IndexWriter(directory, new IndexWriterConfig(analyzer))) {
				writer.addDocument(createAttachmentDocument());
				writer.addDocument(createBeanDocument());
				writer.commit();
			}

			ContentIterator iterator = new LuceneContentIterable(directory).iterator();
			assertEquals(2L, iterator.getTotalHits());

			List<SearchResult> results = new ArrayList<>();
			while (iterator.hasNext()) {
				SearchResult result = iterator.next();
				assertNotNull(result);
				results.add(result);
			}

			assertEquals(2, results.size());
			assertNull(iterator.next());

			SearchResult attachmentResult = results.stream().filter(SearchResult::isAttachment).findFirst().orElse(null);
			assertNotNull(attachmentResult);
			assertEquals("ATT-1", attachmentResult.getBizId());
			assertEquals("file", attachmentResult.getAttributeName());
			assertNotNull(attachmentResult.getLastModified());

			SearchResult beanResult = results.stream().filter(r -> !r.isAttachment()).findFirst().orElse(null);
			assertNotNull(beanResult);
			assertEquals("BEAN-1", beanResult.getBizId());
			assertNull(beanResult.getAttributeName());
		}
	}

	@Test
	void testIteratorOnEmptyIndex() throws Exception {
		try (Directory directory = new ByteBuffersDirectory()) {
			try (StandardAnalyzer analyzer = new StandardAnalyzer();
					IndexWriter writer = new IndexWriter(directory, new IndexWriterConfig(analyzer))) {
				writer.commit();
			}

			ContentIterator iterator = new LuceneContentIterable(directory).iterator();

			assertEquals(0L, iterator.getTotalHits());
			assertFalse(iterator.hasNext());
			assertNull(iterator.next());
		}
	}

	@Test
	void testIteratorLoadsSubsequentPages() throws Exception {
		try (Directory directory = new ByteBuffersDirectory()) {
			try (StandardAnalyzer analyzer = new StandardAnalyzer();
					IndexWriter writer = new IndexWriter(directory, new IndexWriterConfig(analyzer))) {
				for (int i = 0; i < 1001; i++) {
					writer.addDocument(createPagedBeanDocument(i));
				}
				writer.commit();
			}

			ContentIterator iterator = new LuceneContentIterable(directory).iterator();
			assertEquals(1001L, iterator.getTotalHits());

			int count = 0;
			while (iterator.hasNext()) {
				assertNotNull(iterator.next());
				count++;
			}

			assertEquals(1001, count);
			assertNull(iterator.next());
		}
	}

	@Test
	void testIteratorWrapsMalformedBeanDocumentFailure() throws Exception {
		try (Directory directory = new ByteBuffersDirectory()) {
			try (StandardAnalyzer analyzer = new StandardAnalyzer();
					IndexWriter writer = new IndexWriter(directory, new IndexWriterConfig(analyzer))) {
				writer.addDocument(createMalformedBeanDocument());
				writer.commit();
			}

			ContentIterator iterator = new LuceneContentIterable(directory).iterator();
			assertTrue(iterator.hasNext());
			DomainException exception = assertThrows(DomainException.class, iterator::next);
			assertTrue(exception.getMessage().contains("Cannot get the document"));
		}
	}

	@Test
	void testIteratorConstructorWrapsReaderOpenFailure() throws Exception {
		try (Directory directory = new ByteBuffersDirectory()) {
			DomainException exception = assertThrows(DomainException.class, () -> new LuceneContentIterable(directory).iterator());
			assertTrue(exception.getMessage().contains("Cannot open the content index for read"));
		}
	}

	@Test
	void testIteratorNextReturnsNullBeforeHasNextIsCalled() throws Exception {
		try (Directory directory = new ByteBuffersDirectory()) {
			try (StandardAnalyzer analyzer = new StandardAnalyzer();
					IndexWriter writer = new IndexWriter(directory, new IndexWriterConfig(analyzer))) {
				writer.addDocument(createAttachmentDocument());
				writer.commit();
			}

			ContentIterator iterator = new LuceneContentIterable(directory).iterator();
			assertNull(iterator.next());
		}
	}

	@Test
	void testIteratorWrapsFirstPageSearchFailure() throws Exception {
		try (Directory directory = new ByteBuffersDirectory()) {
			try (StandardAnalyzer analyzer = new StandardAnalyzer();
					IndexWriter writer = new IndexWriter(directory, new IndexWriterConfig(analyzer))) {
				writer.addDocument(createAttachmentDocument());
				writer.commit();
			}

			ContentIterator iterator = new LuceneContentIterable(directory).iterator();
			IndexSearcher searcher = mock(IndexSearcher.class);
			when(searcher.search(any(), anyInt())).thenThrow(new IOException("boom"));
			setField(iterator, "searcher", searcher);

			DomainException exception = assertThrows(DomainException.class, iterator::hasNext);
			assertTrue(exception.getMessage().contains("Cannot get the first page"));
		}
	}

	@Test
	void testIteratorWrapsSubsequentPageSearchFailure() throws Exception {
		try (Directory directory = new ByteBuffersDirectory()) {
			try (StandardAnalyzer analyzer = new StandardAnalyzer();
					IndexWriter writer = new IndexWriter(directory, new IndexWriterConfig(analyzer))) {
				writer.addDocument(createAttachmentDocument());
				writer.commit();
			}

			ContentIterator iterator = new LuceneContentIterable(directory).iterator();
			IndexSearcher searcher = mock(IndexSearcher.class);
			setField(iterator, "scoreDocs", new ScoreDoc[] {new ScoreDoc(1, 1.0f)});
			setField(iterator, "index", Integer.valueOf(1));
			when(searcher.searchAfter(any(), any(), anyInt())).thenThrow(new IOException("boom"));
			setField(iterator, "searcher", searcher);

			DomainException exception = assertThrows(DomainException.class, iterator::hasNext);
			assertTrue(exception.getMessage().contains("Cannot get a subsequent page"));
		}
	}

	private static void setField(Object target, String fieldName, Object value) throws Exception {
		java.lang.reflect.Field field = target.getClass().getDeclaredField(fieldName);
		field.setAccessible(true);
		field.set(target, value);
	}

	private static Document createAttachmentDocument() {
		Document document = new Document();
		document.add(new StringField(AbstractContentManager.ATTRIBUTE_NAME, "file", Field.Store.YES));
		document.add(new StringField(Bean.DATA_GROUP_ID, "group", Field.Store.YES));
		document.add(new StringField(AbstractContentManager.CONTENT_ID, "content-1", Field.Store.YES));
		document.add(new StringField(Bean.DOCUMENT_ID, "ATT-1", Field.Store.YES));
		document.add(new StringField(Bean.MODULE_KEY, "admin", Field.Store.YES));
		document.add(new StringField(Bean.DOCUMENT_KEY, "Contact", Field.Store.YES));
		document.add(new StringField(Bean.CUSTOMER_NAME, "demo", Field.Store.YES));
		document.add(new StringField(Bean.USER_ID, "admin", Field.Store.YES));
		document.add(new StoredField(AbstractContentManager.CONTENT, "attachment excerpt"));
		document.add(new StringField(AbstractContentManager.LAST_MODIFIED,
				TimeUtil.formatISODate(new Date(0L), true),
				Field.Store.YES));
		return document;
	}

	private static Document createBeanDocument() {
		Document document = new Document();
		document.add(new StringField(Bean.DATA_GROUP_ID, "group", Field.Store.YES));
		document.add(new StringField(AbstractContentManager.CONTENT_ID, "content-2", Field.Store.YES));
		document.add(new StringField(Bean.DOCUMENT_ID, "BEAN-1~", Field.Store.YES));
		document.add(new StringField(Bean.MODULE_KEY, "admin", Field.Store.YES));
		document.add(new StringField(Bean.DOCUMENT_KEY, "Contact", Field.Store.YES));
		document.add(new StringField(Bean.CUSTOMER_NAME, "demo", Field.Store.YES));
		document.add(new StringField(Bean.USER_ID, "admin", Field.Store.YES));
		document.add(new StoredField(AbstractContentManager.CONTENT, "bean excerpt"));
		return document;
	}

	private static Document createPagedBeanDocument(int index) {
		Document document = new Document();
		document.add(new StringField(Bean.DOCUMENT_ID, "BEAN-" + index + "~", Field.Store.YES));
		document.add(new StringField(Bean.MODULE_KEY, "admin", Field.Store.YES));
		document.add(new StringField(Bean.DOCUMENT_KEY, "Contact", Field.Store.YES));
		document.add(new StringField(Bean.CUSTOMER_NAME, "demo", Field.Store.YES));
		document.add(new StoredField(AbstractContentManager.CONTENT, "bean excerpt " + index));
		return document;
	}

	private static Document createMalformedBeanDocument() {
		Document document = new Document();
		document.add(new StringField(Bean.MODULE_KEY, "admin", Field.Store.YES));
		document.add(new StringField(Bean.DOCUMENT_KEY, "Contact", Field.Store.YES));
		document.add(new StringField(Bean.CUSTOMER_NAME, "demo", Field.Store.YES));
		document.add(new StoredField(AbstractContentManager.CONTENT, "broken bean excerpt"));
		return document;
	}
}