package org.skyve.impl.content.lucene;

import java.io.IOException;
import java.nio.file.Paths;
import java.util.Date;
import java.util.List;
import java.util.Map;

import org.apache.lucene.analysis.Analyzer;
import org.apache.lucene.analysis.TokenStream;
import org.apache.lucene.analysis.en.EnglishAnalyzer;
import org.apache.lucene.document.Document;
import org.apache.lucene.document.Field.Store;
import org.apache.lucene.document.StoredField;
import org.apache.lucene.document.StringField;
import org.apache.lucene.document.TextField;
import org.apache.lucene.index.DirectoryReader;
import org.apache.lucene.index.Fields;
import org.apache.lucene.index.IndexReader;
import org.apache.lucene.index.IndexWriter;
import org.apache.lucene.index.IndexWriterConfig;
import org.apache.lucene.index.IndexWriterConfig.OpenMode;
import org.apache.lucene.index.Term;
import org.apache.lucene.queryparser.classic.QueryParser;
import org.apache.lucene.search.BooleanClause.Occur;
import org.apache.lucene.search.BooleanQuery;
import org.apache.lucene.search.DocValuesFieldExistsQuery;
import org.apache.lucene.search.IndexSearcher;
import org.apache.lucene.search.Query;
import org.apache.lucene.search.ScoreDoc;
import org.apache.lucene.search.TermQuery;
import org.apache.lucene.search.TopDocs;
import org.apache.lucene.search.highlight.Formatter;
import org.apache.lucene.search.highlight.Fragmenter;
import org.apache.lucene.search.highlight.Highlighter;
import org.apache.lucene.search.highlight.QueryScorer;
import org.apache.lucene.search.highlight.SimpleHTMLFormatter;
import org.apache.lucene.search.highlight.SimpleSpanFragmenter;
import org.apache.lucene.search.highlight.TokenSources;
import org.apache.lucene.store.Directory;
import org.apache.lucene.store.FSDirectory;
import org.apache.lucene.util.BytesRef;
import org.skyve.content.AttachmentContent;
import org.skyve.content.BeanContent;
import org.skyve.content.ContentIterable;
import org.skyve.content.MimeType;
import org.skyve.content.SearchResult;
import org.skyve.content.SearchResults;
import org.skyve.content.TextExtractor;
import org.skyve.domain.Bean;
import org.skyve.domain.messages.ManyResultsException;
import org.skyve.domain.messages.NoResultsException;
import org.skyve.impl.content.AbstractContentManager;
import org.skyve.impl.content.FileSystemContentManager;
import org.skyve.impl.content.TikaTextExtractor;
import org.skyve.impl.util.TimeUtil;
import org.skyve.impl.util.UtilImpl;

// If we want to use multiple indices for beans and attachments like elastic we will need 
// 2 dirs underneath CONTENT/SKYVE_CONTENT/ and 2 writer instances
// Use a MultiReader instance to read from 2 indexes at once
// At the moment the content ID is appended with '~' if its a bean content. Nothing is appended for attachments.
// This makes document updating work when there is both attachments and bean content in the same Skyve bean.
public class LuceneContentManager extends FileSystemContentManager {
	static final char BEAN_CONTENT_SUFFIX = '~';
	
	private static Directory directory;
	private static Analyzer analyzer;
	private static IndexWriter writer;
	
	@Override
	public void startup() {
		try {
			directory = FSDirectory.open(Paths.get(UtilImpl.CONTENT_DIRECTORY, CLUSTER_NAME));
			analyzer = new EnglishAnalyzer();
			IndexWriterConfig iwc = new IndexWriterConfig(analyzer);
			iwc.setOpenMode(OpenMode.CREATE_OR_APPEND);
			iwc.setRAMBufferSizeMB(128.0);
			writer = new IndexWriter(directory, iwc);
		}
		catch (IOException e) {
			throw new IllegalStateException("Could not startup Lucene content manager", e);
		}
	}

	@Override
	public void shutdown() {
		try {
			try {
				if (writer != null) {
					if (writer.isOpen()) {
						writer.close();
					}
					writer = null;
				}
			}
			finally {
				try {
					if (analyzer != null) {
						analyzer.close();
						analyzer = null;
					}
				}
				finally {
					if (directory != null) {
						directory.close();
						directory = null;
					}
				}
			}
		}
		catch (IOException e) {
			throw new IllegalStateException("Could not shutdown Lucene content manager", e);
		}
	}
	
	@Override
	public void close() throws Exception {
		writer.flush();
		writer.commit();
	}
	
	@Override
	public void put(BeanContent content) throws Exception {
		String bizContentId = content.getBizId() + BEAN_CONTENT_SUFFIX;
		
		StringBuilder text = new StringBuilder(256);
		Map<String, String> properties = content.getProperties();
		for (String name : properties.keySet()) {
			String value = properties.get(name);
			if (value != null) {
				if (text.length() > 0) {
					text.append(' ');
				}
				text.append(value);
			}
		}

		// Add text to index
		Document document = new Document();
		document.add(new TextField(CONTENT, text.toString(), Store.YES)); // needs to be stored for excerpt generation
		
		// Add meta-data
		String bizCustomer = content.getBizCustomer();
		if (bizCustomer != null) {
			document.add(new StringField(Bean.CUSTOMER_NAME, bizCustomer, Store.YES));
		}
		String bizDataGroupId = content.getBizDataGroupId();
		if (bizDataGroupId != null) {
			document.add(new StringField(Bean.DATA_GROUP_ID, bizDataGroupId, Store.YES));
		}
		String bizUserId = content.getBizUserId();
		if (bizUserId != null) {
			document.add(new StringField(Bean.USER_ID, bizUserId, Store.YES));
		}
		String bizModule = content.getBizModule();
		if (bizModule != null) {
			document.add(new StringField(Bean.MODULE_KEY, bizModule, Store.YES));
		}
		String bizDocument = content.getBizDocument();
		if (bizDocument != null) {
			document.add(new StringField(Bean.DOCUMENT_KEY, bizDocument, Store.YES));
		}
		document.add(new StringField(Bean.DOCUMENT_ID, bizContentId, Store.YES));

		// Last modified
		document.add(new StoredField(LAST_MODIFIED, TimeUtil.formatISODate(new Date(), true)));
			
		if (UtilImpl.CONTENT_TRACE) UtilImpl.LOGGER.info("LuceneContentManager.put(): " + bizContentId);
		writer.updateDocument(new Term(Bean.DOCUMENT_ID, bizContentId), document);
	}
	
	@Override
	public void put(AttachmentContent attachment, boolean index) throws Exception {
		// NB Call super first coz this sets the content ID and last modified date.
		super.put(attachment, index);
		putIndex(attachment, index);
	}
	
	@Override
	public void update(AttachmentContent attachment) throws Exception {
		// NB Call super first coz this sets the last modified date.
		super.update(attachment);

		try (IndexReader attachmentReader = DirectoryReader.open(directory)) {
			IndexSearcher searcher = new IndexSearcher(attachmentReader);
			ScoreDoc[] results = searcher.search(new TermQuery(new Term(CONTENT_ID, attachment.getContentId())), 2).scoreDocs;
			if (results.length == 0) {
				throw new NoResultsException();
			}
			if (results.length > 1) {
				throw new ManyResultsException();
			}
			Document document = attachmentReader.document(results[0].doc);
			if (document == null) {
				throw new NoResultsException();
			}

			// remove document fields about to be updated when putIndex() is called.
			document.removeField(Bean.CUSTOMER_NAME);
			document.removeField(Bean.DATA_GROUP_ID);
			document.removeField(Bean.USER_ID);
			document.removeField(Bean.MODULE_KEY);
			document.removeField(Bean.DOCUMENT_KEY);
			document.removeField(Bean.DOCUMENT_ID);
			document.removeField(ATTRIBUTE_NAME);
			document.removeField(FILENAME);
			document.removeField(LAST_MODIFIED);
			document.removeField(CONTENT_ID);
			
			putIndex(document, attachment);
		}
	}
	
	private static void putIndex(AttachmentContent attachment, boolean index) throws Exception {
		Document document = new Document();
		if (index) {
			TextExtractor extractor = new TikaTextExtractor();
			String text = extractor.extractTextFromContent(attachment);
			if (text != null) {
				document.add(new TextField(CONTENT, text, Store.YES)); // needs to be stored for excerpt generation
			}
		}

		// Doc as binary attachment, inlined
		if (! UtilImpl.CONTENT_FILE_STORAGE) {
			byte[] bytes = attachment.getContentBytes();
			if (bytes != null) {
				document.add(new StoredField(ATTACHMENT, bytes));
			}
		}
		
		putIndex(document, attachment);
	}
	
	private static void putIndex(Document document, AttachmentContent attachment) throws Exception {
		String bizId = attachment.getBizId();

		// Add meta-data
		String bizCustomer = attachment.getBizCustomer();
		if (bizCustomer != null) {
			document.add(new StringField(Bean.CUSTOMER_NAME, bizCustomer, Store.YES));
		}
		String bizDataGroupId = attachment.getBizDataGroupId();
		if (bizDataGroupId != null) {
			document.add(new StringField(Bean.DATA_GROUP_ID, bizDataGroupId, Store.YES));
		}
		String bizUserId = attachment.getBizUserId();
		if (bizUserId != null) {
			document.add(new StringField(Bean.USER_ID, bizUserId, Store.YES));
		}
		String bizModule = attachment.getBizModule();
		if (bizModule != null) {
			document.add(new StringField(Bean.MODULE_KEY, bizModule, Store.YES));
		}
		String bizDocument = attachment.getBizDocument();
		if (bizDocument != null) {
			document.add(new StringField(Bean.DOCUMENT_KEY, bizDocument, Store.YES));
		}
		document.add(new StringField(Bean.DOCUMENT_ID, bizId, Store.YES));
		String attributeName = attachment.getAttributeName();
		if (attributeName != null) {
			document.add(new StoredField(ATTRIBUTE_NAME, attributeName));
		}

		// Add file attributes
		String fileName = attachment.getFileName();
		if (fileName != null) {
			document.add(new StringField(FILENAME, fileName, Store.YES));
		}
		document.add(new StoredField(LAST_MODIFIED, TimeUtil.formatISODate(attachment.getLastModified(), true)));

		if (UtilImpl.CONTENT_TRACE) UtilImpl.LOGGER.info("LuceneContentManager.put(): " + bizId);
		String contentId = attachment.getContentId();
		// Even if existing, add the content ID to the document as it could be a re-index
		document.add(new StringField(CONTENT_ID, contentId, Store.YES));
		// delete if exists and re-add
		writer.updateDocument(new Term(CONTENT_ID, contentId), document);
	}
	
	@Override
	public AttachmentContent getAttachment(String contentId) throws Exception {
		if (UtilImpl.CONTENT_FILE_STORAGE) {
			return super.getAttachment(contentId);
		}

		try (IndexReader attachmentReader = DirectoryReader.open(directory)) {
			IndexSearcher searcher = new IndexSearcher(attachmentReader);
			ScoreDoc[] results = searcher.search(new TermQuery(new Term(CONTENT_ID, contentId)), 2).scoreDocs;
			if (results.length > 1) {
				throw new ManyResultsException();
			}
			if (results.length == 0) {
				return null;
			}
			Document document = attachmentReader.document(results[0].doc);
			if (document == null) {
				return null;
			}
		
			MimeType mimeType = null;
			String contentType = document.get(CONTENT_TYPE);
			if (contentType != null) {
				mimeType = MimeType.fromContentType(contentType);
			}
			String fileName = document.get(FILENAME);
			Date lastModified = TimeUtil.parseISODate(document.get(LAST_MODIFIED));
			String bizCustomer = document.get(Bean.CUSTOMER_NAME);
			String bizModule = document.get(Bean.MODULE_KEY);
			String bizDocument = document.get(Bean.DOCUMENT_KEY);
			String bizDataGroupId = document.get(Bean.DATA_GROUP_ID);
			String bizUserId = document.get(Bean.USER_ID);
			String bizId = document.get(Bean.DOCUMENT_ID);
			String binding = document.get(ATTRIBUTE_NAME);
			BytesRef bytesRef = document.getBinaryValue(ATTACHMENT);
			if (bytesRef == null) {
				return null;
			}
			byte[] bytes = bytesRef.bytes;
			if (bytes == null) {
				return null;
			}
			
			AttachmentContent result = new AttachmentContent(bizCustomer,
																bizModule,
																bizDocument,
																bizDataGroupId,
																bizUserId,
																bizId,
																binding,
																fileName,
																mimeType,
																bytes);
			result.setLastModified(lastModified);
			result.setContentType(contentType);
			result.setContentId(contentId);
			if (UtilImpl.CONTENT_TRACE) UtilImpl.LOGGER.info("LuceneContentManager.get(" + contentId + "): exists");
			return result;
		}
	}
	
	@Override
	public void removeBean(String bizId) throws Exception {
		writer.deleteDocuments(new Term(Bean.DOCUMENT_ID, bizId + BEAN_CONTENT_SUFFIX));
	}
	
	@Override
	public void removeAttachment(String contentId) throws Exception {
		writer.deleteDocuments(new Term(CONTENT_ID, contentId));
		super.removeAttachment(contentId);
	}
	
	@Override
	public ContentIterable all() throws Exception {
		return new LuceneContentIterable(directory);
	}
	
	@Override
	public SearchResults google(String search, int maxResults) throws Exception {
		SearchResults results = new SearchResults();
		
		String term = UtilImpl.processStringValue(search);
		if ((term != null) && (maxResults > 0)) {
			long millis = System.currentTimeMillis();

			try (IndexReader reader = DirectoryReader.open(directory)) {
				IndexSearcher searcher = new IndexSearcher(reader);
				QueryParser parser = new QueryParser(CONTENT, analyzer);
				Query query = parser.parse(term);
				// Search for double the amount of docs (filtered by security later) up to a max of 10000
				TopDocs hits = searcher.search(query, Math.min(maxResults * 2, 10000));

				results.setSearchTimeInSecs(Long.toString(((System.currentTimeMillis() - millis) / 1000L)));

				Formatter formatter = new SimpleHTMLFormatter("<span class=\"highlight\">", "</span>");
		        
				// It scores text fragments by the number of unique query terms found
				// Basically the matching score in layman terms
				QueryScorer scorer = new QueryScorer(query);
		         
				// used to markup highlighted terms found in the best sections of a text
				Highlighter highlighter = new Highlighter(formatter, scorer);
		         
				// It breaks text up into same-size texts but does not split up spans
				Fragmenter fragmenter = new SimpleSpanFragmenter(scorer, 10);
		         
				highlighter.setTextFragmenter(fragmenter);
		         
				// TODO results.setSuggestion(search);
	
				List<SearchResult> resultList = results.getResults();
				StringBuilder excerpt = new StringBuilder(256);
				// Iterate over found results
				for (int i = 0, l = hits.scoreDocs.length; i < l; i++) {
					int docid = hits.scoreDocs[i].doc;
					Document document = searcher.doc(docid);

					String bizCustomer = document.get(Bean.CUSTOMER_NAME);
					String bizModule = document.get(Bean.MODULE_KEY);
					String bizDocument = document.get(Bean.DOCUMENT_KEY);
					String bizDataGroupId = document.get(Bean.DATA_GROUP_ID);
					String bizUserId = document.get(Bean.USER_ID);
					String bizId = document.get(Bean.DOCUMENT_ID);
					String attributeName = document.get(ATTRIBUTE_NAME);
					if (canAccessContent(bizCustomer,
											bizModule,
											bizDocument,
											bizDataGroupId,
											bizUserId,
											bizId,
											attributeName)) {
						// Get stored text from found document
						String text = document.get(CONTENT);
						 
						// Create token stream
					    Fields vectors = reader.getTermVectors(docid);
						try (TokenStream stream = TokenSources.getTokenStream(CONTENT, vectors, text, analyzer, -1)) {
							// Get highlighted text fragments
							String[] fragments = highlighter.getBestFragments(stream, text, 10);
							excerpt.setLength(0);
							for (String fragment : fragments) {
								excerpt.append(' ').append(fragment);
							}
						}
					
						SearchResult result = new SearchResult();
						result.setAttributeName(attributeName);
						result.setBizDataGroupId(bizDataGroupId);
						String contentId = document.get(AbstractContentManager.CONTENT_ID);
						result.setContentId(contentId);
						if (contentId == null) { // bean content
							result.setBizId(bizId.substring(0, bizId.length() - 1));
						}
						else { // attachment content
							result.setBizId(bizId);
						}
						result.setBizUserId(bizUserId);
						result.setCustomerName(bizCustomer);
						result.setDocumentName(bizDocument);
						result.setExcerpt(excerpt.toString());
						result.setLastModified(TimeUtil.parseISODate(document.get(LAST_MODIFIED)));
						result.setModuleName(bizModule);
						result.setScore((int) (hits.scoreDocs[i].score * 100.0));
						resultList.add(result);

						if (resultList.size() >= maxResults) {
							break;
						}
					}
				}
			}
		}
		
		return results;
	}
	
	@Override
	public void truncate(String customerName) throws Exception {
		writer.deleteDocuments(new Term(Bean.CUSTOMER_NAME, customerName));
	}
	
	@Override
	public void truncateAttachments(String customerName) throws Exception {
		writer.deleteDocuments(new BooleanQuery.Builder()
										.add(new TermQuery(new Term(Bean.CUSTOMER_NAME, customerName)), Occur.MUST)
										.add(new DocValuesFieldExistsQuery(CONTENT_ID), Occur.MUST)
										.build());
	}
	
	@Override
	public void truncateBeans(String customerName) throws Exception {
		writer.deleteDocuments(new BooleanQuery.Builder()
										.add(new TermQuery(new Term(Bean.CUSTOMER_NAME, customerName)), Occur.MUST)
										.add(new TermQuery(new Term(Bean.DOCUMENT_ID, "~")), Occur.MUST)
										.build());
	}
	
	@Override
	public void reindex(AttachmentContent attachment, boolean index) throws Exception {
		putIndex(attachment, index);
	}
}
