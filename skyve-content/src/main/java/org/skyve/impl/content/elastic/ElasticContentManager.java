package org.skyve.impl.content.elastic;

//import java.io.File;
//import java.io.FileOutputStream;
//import java.io.FileWriter;
import java.io.IOException;
//import java.nio.file.Files;
//import java.nio.file.StandardCopyOption;
//import java.util.ArrayList;
//import java.util.Date;
//import java.util.List;
//import java.util.Map;
//import java.util.TreeMap;
//import java.util.UUID;
//import java.util.logging.Level;

//import org.apache.commons.codec.binary.Base64;
//import org.apache.tika.Tika;
//import org.apache.tika.exception.TikaException;
//import org.apache.tika.metadata.HttpHeaders;
//import org.apache.tika.metadata.MSOffice;
//import org.apache.tika.metadata.Metadata;
//import org.apache.tika.metadata.TikaCoreProperties;
//import org.elasticsearch.action.admin.indices.flush.FlushResponse;
//import org.elasticsearch.action.get.GetRequestBuilder;
//import org.elasticsearch.action.get.GetResponse;
//import org.elasticsearch.action.index.IndexResponse;
//import org.elasticsearch.action.search.SearchResponse;
//import org.elasticsearch.action.search.SearchType;
//import org.elasticsearch.client.Client;
//import org.elasticsearch.common.Strings;
//import org.elasticsearch.common.io.stream.BytesStreamInput;
//import org.elasticsearch.common.text.Text;
//import org.elasticsearch.common.xcontent.XContentBuilder;
//import org.elasticsearch.common.xcontent.XContentFactory;
//import org.elasticsearch.index.get.GetField;
//import org.elasticsearch.index.query.FilterBuilder;
//import org.elasticsearch.index.query.FilterBuilders;
//import org.elasticsearch.index.query.QueryBuilder;
//import org.elasticsearch.index.query.QueryBuilders;
//import org.elasticsearch.node.Node;
//import org.elasticsearch.search.SearchHit;
//import org.elasticsearch.search.SearchHitField;
//import org.elasticsearch.search.facet.FacetBuilders;
//import org.elasticsearch.search.facet.terms.TermsFacet;
//import org.elasticsearch.search.facet.terms.TermsFacet.Entry;
//import org.elasticsearch.search.highlight.HighlightField;
import org.skyve.content.AttachmentContent;
import org.skyve.content.BeanContent;
import org.skyve.content.ContentIterable;
//import org.skyve.content.MimeType;
//import org.skyve.content.SearchResult;
import org.skyve.content.SearchResults;
import org.skyve.domain.Bean;
//import org.skyve.domain.messages.DomainException;
import org.skyve.impl.content.AbstractContentManager;
//import org.skyve.impl.util.TimeUtil;
import org.skyve.impl.util.UtilImpl;
//import org.skyve.util.FileUtil;
//import org.skyve.util.JSON;

public class ElasticContentManager extends AbstractContentManager {
	static final String ATTACHMENT_INDEX_NAME = "attachments";
	static final String ATTACHMENT_INDEX_TYPE = "attachment";
	static final String BEAN_INDEX_NAME = "beans";
	static final String BEAN_INDEX_TYPE = "bean";

//    private static final String ATTACHMENT = "attachment";
//
//	private static final String FILE = "file";
//    public static final String FILE_CONTENT_TYPE = "file.content_type";
//    private static final String FILE_FILENAME = "file.filename";
//    public static final String FILE_LAST_MODIFIED = "file.last_modified";
//    private static final String FILESIZE = "filesize";
//
//    private static final String META = "meta";
//    private static final String META_TITLE = "meta.title";
//    private static final String META_AUTHOR = "meta.author";
//    private static final String META_KEYWORDS = "meta.keywords";
//    private static final String AUTHOR = "author";
//    private static final String TITLE = "title";
//    private static final String DATE = "date";
//    private static final String KEYWORDS = "keywords";
//
//    private static final String BEAN = "bean";
    static final String BEAN_CUSTOMER_NAME = "bean." + Bean.CUSTOMER_NAME;
    static final String BEAN_MODULE_KEY = "bean." + Bean.MODULE_KEY;
    static final String BEAN_DOCUMENT_KEY = "bean." + Bean.DOCUMENT_KEY;
    static final String BEAN_DATA_GROUP_ID = "bean." + Bean.DATA_GROUP_ID;
    static final String BEAN_USER_ID = "bean." + Bean.USER_ID;
    static final String BEAN_DOCUMENT_ID = "bean." + Bean.DOCUMENT_ID;
    static final String BEAN_ATTRIBUTE_NAME = "bean.attribute";
	
//	private static Node node = ElasticUtil.localNode();
//	private static final Tika TIKA = new Tika();

//	private Client client = null;
	
	public ElasticContentManager() {
//		client = ElasticUtil.localClient(node);
	}
	
	@Override
	public void startup() {
//		ElasticUtil.prepareIndex(client, ElasticContentManager.ATTACHMENT_INDEX_NAME, ElasticContentManager.ATTACHMENT_INDEX_TYPE);
//		ElasticUtil.prepareIndex(client, ElasticContentManager.BEAN_INDEX_NAME, ElasticContentManager.BEAN_INDEX_TYPE);
	}

	@Override
	public void shutdown() {
//		ElasticUtil.close(node);
	}

	@Override
	public void close() {
/*
		FlushResponse flushResponse = client.admin().indices().prepareFlush(ATTACHMENT_INDEX_NAME).setWaitIfOngoing(true).execute().actionGet();
		if (flushResponse.getFailedShards() > 0) {
			throw new DomainException("Could not flush the Elastic attachments index to disk");
		}

		client.close();
*/
	}
	
	@Override
	public void put(BeanContent content)
	throws Exception {
/*
		try (XContentBuilder source = XContentFactory.jsonBuilder().startObject()) {
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
			source.field(CONTENT, text);
			
			// Bean
			source.startObject(BEAN)
					.field(Bean.CUSTOMER_NAME, content.getBizCustomer())
					.field(Bean.DATA_GROUP_ID, content.getBizDataGroupId())
					.field(Bean.USER_ID, content.getBizUserId())
					.field(Bean.MODULE_KEY, content.getBizModule())
					.field(Bean.DOCUMENT_KEY, content.getBizDocument())
					.field(Bean.DOCUMENT_ID, content.getBizId())
					.endObject(); // Bean

			// Last modified
			source.field(LAST_MODIFIED, new Date());
			
			if (UtilImpl.CONTENT_TRACE) UtilImpl.LOGGER.info("ElasticContentManager.put(): " + source.string());
			client.prepareIndex(BEAN_INDEX_NAME, 
									BEAN_INDEX_TYPE,
									content.getBizId()).setSource(source).execute().actionGet();
		}
*/
	}
	
	@Override
	public void put(AttachmentContent attachment, boolean index)
	throws Exception {
		put(attachment, index, true);
	}
	
	@Override
	public void update(AttachmentContent content) throws Exception {
		// needs implementation
	}
	
	@Override
	public void reindex(AttachmentContent attachment, boolean index) 
	throws Exception {
		put(attachment, index, false);
	}

	@SuppressWarnings("unused")
	private void put(AttachmentContent attachment, boolean index, boolean store)
	throws Exception {
/*
		try (XContentBuilder source = XContentFactory.jsonBuilder().startObject()) {
		
			byte[] content = attachment.getContentBytes();

			// Sniff content type if necessary
			String contentType = attachment.getContentType();
			if (contentType == null) {
				String fileName = attachment.getFileName();
				if (fileName == null) {
					contentType = TIKA.detect(content);
				}
				else {
					contentType = TIKA.detect(content, fileName);
				}
				attachment.setContentType(contentType);
			}
			
			if (index) {
				try (BytesStreamInput contentStream = new BytesStreamInput(content, false)) {
					Metadata metadata = new Metadata();
					String parsedContent = "";
					try {
						// Set the maximum length of strings returned by the parseToString method, -1 sets no limit
						parsedContent = TIKA.parseToString(contentStream, metadata, 100000);
					}
					catch (TikaException e) {
						UtilImpl.LOGGER.log(Level.SEVERE, 
												"ElasticContentManager.put(): Attachment could not be parsed by TIKA and so has not been textually indexed",
												e);
					}
					
					// File
					source.startObject(FILE)
							.field(FILENAME, attachment.getFileName())
							.field(LAST_MODIFIED, new Date())
							.field(CONTENT_TYPE,
									(contentType != null) ? 
										contentType : 
										metadata.get(HttpHeaders.CONTENT_TYPE));
					if (metadata.get(HttpHeaders.CONTENT_LENGTH) != null) {
						// We try to get CONTENT_LENGTH from Tika first
						source.field(FILESIZE, metadata.get(HttpHeaders.CONTENT_LENGTH));
					}
					else {
						// Otherwise, we use our byte[] length
						source.field(FILESIZE, content.length);
					}
					source.endObject(); // File
	
					// Meta
					String title = metadata.get(TikaCoreProperties.TITLE);
					source.startObject(META)
							.field(AUTHOR, metadata.get(MSOffice.AUTHOR))
							.field(TITLE,
									(title != null) ? title : attachment.getFileName())
							.field(DATE, metadata.get(TikaCoreProperties.CREATED))
							.array(KEYWORDS,
									Strings.commaDelimitedListToStringArray(metadata.get(TikaCoreProperties.KEYWORDS)))
							.endObject(); // Meta
			
					// Bean
					source.startObject(BEAN)
							.field(Bean.CUSTOMER_NAME, attachment.getBizCustomer())
							.field(Bean.DATA_GROUP_ID, attachment.getBizDataGroupId())
							.field(Bean.USER_ID, attachment.getBizUserId())
							.field(Bean.MODULE_KEY, attachment.getBizModule())
							.field(Bean.DOCUMENT_KEY, attachment.getBizDocument())
							.field(Bean.DOCUMENT_ID, attachment.getBizId())
							.field(ATTRIBUTE_NAME, attachment.getAttributeName())
							.endObject(); // Bean
		
					// Doc content
					source.field(CONTENT, parsedContent);
			
					// Doc as binary attachment, inlined
					if (store && (! UtilImpl.CONTENT_FILE_STORAGE)) {
						source.field(ATTACHMENT, new String(new Base64().encode(content)));
					}
					
					// End of our document
					source.endObject();
				}
			}
			else {
				// File
				source.startObject(FILE)
						.field(FILENAME, attachment.getFileName())
						.field(LAST_MODIFIED, new Date())
						.field(CONTENT_TYPE, contentType);
				// No indexing, so we use our byte[] length
				source.field(FILESIZE, content.length);
				source.endObject(); // File

				// Meta
				source.startObject(META)
						.field(AUTHOR, (String) null)
						.field(TITLE, attachment.getFileName())
						.field(DATE, (String) null)
						.array(KEYWORDS, new String[0])
						.endObject(); // Meta
		
				// Bean
				source.startObject(BEAN)
						.field(Bean.CUSTOMER_NAME, attachment.getBizCustomer())
						.field(Bean.DATA_GROUP_ID, attachment.getBizDataGroupId())
						.field(Bean.USER_ID, attachment.getBizUserId())
						.field(Bean.MODULE_KEY, attachment.getBizModule())
						.field(Bean.DOCUMENT_KEY, attachment.getBizDocument())
						.field(Bean.DOCUMENT_ID, attachment.getBizId())
						.field(ATTRIBUTE_NAME, attachment.getAttributeName())
						.endObject(); // Bean
	
				// Doc as binary attachment, inlined
				if (store && (! UtilImpl.CONTENT_FILE_STORAGE)) {
					source.field(ATTACHMENT, new String(new Base64().encode(content)));
				}
				
				// End of our document
				source.endObject();
			}
			if (UtilImpl.CONTENT_TRACE) UtilImpl.LOGGER.info("ElasticContentManager.put(): " + source.string());
			String contentId = attachment.getContentId();
			IndexResponse indexResponse = client.prepareIndex(ATTACHMENT_INDEX_NAME, 
																ATTACHMENT_INDEX_TYPE,
																(contentId == null) ? UUID.randomUUID().toString() : contentId)
													.setSource(source).execute().actionGet();
			if (indexResponse.isCreated()) {
				attachment.setContentId(indexResponse.getId());
			}

			if (store && UtilImpl.CONTENT_FILE_STORAGE) {
				StringBuilder absoluteContentStoreFolderPath = new StringBuilder(128);
				absoluteContentStoreFolderPath.append(UtilImpl.CONTENT_DIRECTORY).append(FILE_STORE_NAME).append('/');

				writeContentFiles(absoluteContentStoreFolderPath, attachment, content);
			}
		}
*/
	}

	@Override
	public AttachmentContent getAttachment(String contentId) throws Exception {
		if (UtilImpl.CONTENT_FILE_STORAGE) {
			StringBuilder absoluteContentStoreFolderPath = new StringBuilder(128);
			absoluteContentStoreFolderPath.append(UtilImpl.CONTENT_DIRECTORY).append(FILE_STORE_NAME).append('/');
			return getFromFileSystem(absoluteContentStoreFolderPath, contentId);
		}
		return getFromElastic(contentId);
	}

	@SuppressWarnings("static-method")
	private AttachmentContent getFromElastic(@SuppressWarnings("unused") String contentId) throws Exception {
/*
		GetRequestBuilder builder = client.prepareGet(ATTACHMENT_INDEX_NAME, ATTACHMENT_INDEX_TYPE, contentId);
		builder.setFields(ATTACHMENT, 
							FILE_FILENAME,
							FILE_CONTENT_TYPE, 
							FILE_LAST_MODIFIED,
							BEAN_CUSTOMER_NAME,
							BEAN_MODULE_KEY,
							BEAN_DOCUMENT_KEY,
							BEAN_DATA_GROUP_ID,
							BEAN_USER_ID,
							BEAN_DOCUMENT_ID,
							BEAN_ATTRIBUTE_NAME);
		GetResponse response = builder.get();
		if (! response.isExists()) {
			if (UtilImpl.CONTENT_TRACE) UtilImpl.LOGGER.info("ElasticContentManager.get(" + contentId + "): DNE");
			return null;
		}

		GetField field = response.getField(FILE_CONTENT_TYPE);
		MimeType mimeType = null;
		String contentType = null;
		if (field != null) {
			contentType = (String) field.getValue();
			mimeType = MimeType.fromContentType(contentType);
		}

		// content is a base64 encoded stream straight out of the index.
		field = response.getField(ATTACHMENT);
		// NB This can occur when a content repository is changed from file storage to index
		// stored and is not properly cleaned up with backup/restore.
		if (field == null) {
			if (UtilImpl.CONTENT_TRACE) UtilImpl.LOGGER.info("ElasticContentManager.get(" + contentId + ") - Attachment: DNE");
			return null;
		}
		String content = (String) field.getValue();
		
		String fileName = null;
		field = response.getField(FILE_FILENAME);
		if (field != null) {
			fileName = (String) field.getValue();
		}
		Date lastModified = TimeUtil.parseISODate((String) response.getField(FILE_LAST_MODIFIED).getValue());

		String bizCustomer = (String) response.getField(BEAN_CUSTOMER_NAME).getValue();
		String bizModule = (String) response.getField(BEAN_MODULE_KEY).getValue();
		String bizDocument = (String) response.getField(BEAN_DOCUMENT_KEY).getValue();
		String bizDataGroupId = null;
		field = response.getField(BEAN_DATA_GROUP_ID);
		if (field != null) {
			bizDataGroupId = (String) field.getValue();
		}
		String bizUserId = (String) response.getField(BEAN_USER_ID).getValue();
		String bizId = (String) response.getField(BEAN_DOCUMENT_ID).getValue();
		String binding = (String) response.getField(BEAN_ATTRIBUTE_NAME).getValue();

		AttachmentContent result = new AttachmentContent(bizCustomer,
															bizModule,
															bizDocument,
															bizDataGroupId,
															bizUserId,
															bizId,
															binding,
															fileName,
															mimeType,
															new Base64().decode(content));
		result.setLastModified(lastModified);
		result.setContentType(contentType);
		result.setContentId(response.getId());
		if (UtilImpl.CONTENT_TRACE) UtilImpl.LOGGER.info("ElasticContentManager.get(" + contentId + "): exists");

		return result;
*/
return null;
	}

	@Override
	public void removeBean(String bizId) {
/*
		if (UtilImpl.CONTENT_TRACE) UtilImpl.LOGGER.info("ElasticContentManager.remove(" + content.getBizId() + ")");
		client.prepareDelete(BEAN_INDEX_NAME,
								BEAN_INDEX_TYPE,
								content.getBizId()).execute().actionGet();
*/
	}

	@Override
	public void removeAttachment(String contentId) throws IOException {
/*
		if (UtilImpl.CONTENT_TRACE) UtilImpl.LOGGER.info("ElasticContentManager.remove(" + contentId + ")");
		client.prepareDelete(ATTACHMENT_INDEX_NAME,
								ATTACHMENT_INDEX_TYPE,
								contentId).execute().actionGet();

		if (UtilImpl.CONTENT_FILE_STORAGE) {
			StringBuilder path = new StringBuilder(128);
			path.append(UtilImpl.CONTENT_DIRECTORY).append(FILE_STORE_NAME).append('/');
			AbstractContentManager.appendBalancedFolderPathFromContentId(contentId, path, false);
			File dir = new File(path.toString());
			File thirdDir = null;
			if (dir.exists()) {
				thirdDir = dir.getParentFile();
				FileUtil.delete(dir);
			}
			
			// Delete the folder structure housing the content file, if empty.
			if ((thirdDir != null) && thirdDir.exists() && thirdDir.isDirectory()) {
				File secondDir = thirdDir.getParentFile();
				if (thirdDir.delete()) {
					if ((secondDir != null) && secondDir.exists() && secondDir.isDirectory()) {
						File firstDir = secondDir.getParentFile();
						if (secondDir.delete()) {
							if ((firstDir != null) && firstDir.exists() && firstDir.isDirectory()) {
								firstDir.delete();
							}
						}
					}
				}
			}
		}
*/
	}

	@Override
	public void dropIndexing() throws Exception {
		// TODO
	}
	
	@Override
	public void truncateIndexing(String customerName) throws Exception {
/*
		if (UtilImpl.CONTENT_TRACE) UtilImpl.LOGGER.info("ElasticContentManager.truncate(" + customerName + ")");
		client.prepareDeleteByQuery()
			.setIndices(ATTACHMENT_INDEX_NAME, BEAN_INDEX_NAME)
			.setTypes(ATTACHMENT_INDEX_TYPE, BEAN_INDEX_TYPE)
			.setQuery(QueryBuilders.termQuery(BEAN_CUSTOMER_NAME, customerName)).execute().actionGet();
		
		FlushResponse flushResponse = client.admin().indices().prepareFlush(ATTACHMENT_INDEX_NAME, BEAN_INDEX_NAME).setWaitIfOngoing(true).execute().actionGet();
		if (flushResponse.getFailedShards() > 0) {
			throw new DomainException("Could not flush the Elastic beans and attachments index to disk");
		}
*/
	}

	@Override
	public void truncateAttachmentIndexing(String customerName) throws Exception {
/*
		if (UtilImpl.CONTENT_TRACE) UtilImpl.LOGGER.info("ElasticContentManager.truncateAttachments(" + customerName + ")");
		client.prepareDeleteByQuery()
			.setIndices(ATTACHMENT_INDEX_NAME)
			.setTypes(ATTACHMENT_INDEX_TYPE)
			.setQuery(QueryBuilders.termQuery(BEAN_CUSTOMER_NAME, customerName)).execute().actionGet();

		FlushResponse flushResponse = client.admin().indices().prepareFlush(ATTACHMENT_INDEX_NAME).setWaitIfOngoing(true).execute().actionGet();
		if (flushResponse.getFailedShards() > 0) {
			throw new DomainException("Could not flush the Elastic attachments index to disk");
		}
*/
	}
	
	@Override
	public void truncateBeanIndexing(String customerName) throws Exception {
/*
		if (UtilImpl.CONTENT_TRACE) UtilImpl.LOGGER.info("ElasticContentManager.truncateBeans(" + customerName + ")");
		client.prepareDeleteByQuery()
			.setIndices(BEAN_INDEX_NAME)
			.setTypes(BEAN_INDEX_TYPE)
			.setQuery(QueryBuilders.termQuery(BEAN_CUSTOMER_NAME, customerName)).execute().actionGet();

		FlushResponse flushResponse = client.admin().indices().prepareFlush(BEAN_INDEX_NAME).setWaitIfOngoing(true).execute().actionGet();
		if (flushResponse.getFailedShards() > 0) {
			throw new DomainException("Could not flush the Elastic beans index to disk");
		}
*/
	}

	@Override
	public SearchResults google(String search, int maxResults)
	throws Exception {
/*
		QueryBuilder qb;
		if ((search == null) || search.trim().isEmpty()) {
			qb = QueryBuilders.matchAllQuery();
		}
		else {
			qb = QueryBuilders.queryString(search)
					.field(CONTENT)
					.field(FILE_FILENAME)
					.field(META_TITLE)
					.field(META_KEYWORDS)
					.field(META_AUTHOR);
		}

		SearchResponse searchResponse = client.prepareSearch()
											.setIndices(ATTACHMENT_INDEX_NAME, BEAN_INDEX_NAME)
											.setTypes(ATTACHMENT_INDEX_TYPE, BEAN_INDEX_TYPE)
											.setSearchType(SearchType.DFS_QUERY_THEN_FETCH).setQuery(qb)
											.setFrom(0).setSize(10000)
											.addHighlightedField(CONTENT)
											.addHighlightedField(FILE_FILENAME)
											.addHighlightedField(META_TITLE)
											.addHighlightedField(META_KEYWORDS)
											.addHighlightedField(META_AUTHOR)
											.setHighlighterPreTags("<span class=\"highlight\">")
											.setHighlighterPostTags("</span>")
											.addFields(BEAN_CUSTOMER_NAME,
														BEAN_MODULE_KEY,
														BEAN_DOCUMENT_KEY,
														BEAN_DATA_GROUP_ID,
														BEAN_USER_ID,
														BEAN_DOCUMENT_ID,
														BEAN_ATTRIBUTE_NAME,
														LAST_MODIFIED,
														FILE_LAST_MODIFIED)
											.execute().actionGet();

		SearchResults results = new SearchResults();
		results.setSearchTimeInSecs(Integer.toString((int) (searchResponse.getTookInMillis() / 1000)));

		List<SearchResult> hits = results.getResults();
		for (SearchHit searchHit : searchResponse.getHits()) {
			String bizCustomer = (String) fieldValue(searchHit, BEAN_CUSTOMER_NAME);
			String bizModule = (String) fieldValue(searchHit, BEAN_MODULE_KEY);
			String bizDocument = (String) fieldValue(searchHit, BEAN_DOCUMENT_KEY);
			String bizDataGroupId = (String) fieldValue(searchHit, BEAN_DATA_GROUP_ID);
			String bizUserId = (String) fieldValue(searchHit, BEAN_USER_ID);
			String bizId = (String) fieldValue(searchHit, BEAN_DOCUMENT_ID);
			if (canReadContent(bizCustomer,
								bizModule,
								bizDocument,
								bizDataGroupId,
								bizUserId,
								bizId)) {
				SearchResult hit = new SearchResult();
	
				hit.setCustomerName(bizCustomer);
				hit.setModuleName(bizModule);
				hit.setDocumentName(bizDocument);
				hit.setBizDataGroupId(bizDataGroupId);
				hit.setBizUserId(bizUserId);
				hit.setBizId(bizId);
				hit.setScore((int) (searchHit.score() * 100.0));
	
				hit.setAttributeName((String) fieldValue(searchHit, BEAN_ATTRIBUTE_NAME));
				String isoDate = (String) fieldValue(searchHit, LAST_MODIFIED);
				if (isoDate != null) {
					hit.setLastModified(TimeUtil.parseISODate(isoDate));
				}
				isoDate = (String) fieldValue(searchHit, FILE_LAST_MODIFIED);
				if (isoDate != null) {
					hit.setLastModified(TimeUtil.parseISODate(isoDate));
				}
				hit.setContentId(searchHit.getId());

//				hit.setSource(searchHit.getSourceAsString());
//	
//				if (searchHit.getFields() != null) {
//					if (searchHit.getFields().get(FILE_CONTENT_TYPE) != null) {
//						hit.setContentType((String) searchHit.getFields().get(FILE_CONTENT_TYPE).getValue());
//					}
//				}
//	
//				if (searchHit.getSource() != null) {
//					hit.setTitle(ESUtil.getSingleStringValue(META_TITLE, searchHit.getSource()));
//				}

				if (searchHit.getHighlightFields() != null) {
					for (HighlightField highlightField : searchHit.getHighlightFields().values()) {
						Text[] fragmentsBuilder = highlightField.getFragments();
						StringBuilder excerpt = new StringBuilder(256);
						for (Text fragment : fragmentsBuilder) {
							excerpt.append(fragment.string()).append(' ');
						}
						hit.setExcerpt(excerpt.toString().trim());
					}
				}
	
				hits.add(hit);
				
				if (hits.size() >= maxResults) {
					break;
				}
			}
		}

		return results;
*/
return new SearchResults();
	}
/*
	static Object fieldValue(SearchHit hit, String fieldName) {
		SearchHitField field = hit.field(fieldName);
		if (field != null) {
			return field.value();
		}
		return null;
	}
*/	
	@Override
	public ContentIterable all() throws Exception {
//		return new ElasticContentIterable(client);
return new ElasticContentIterable();
	}
/*
	static List<String> complete(Client client, String query) {
		List<String> results = new ArrayList<>();

//		QueryBuilder qb = QueryBuilders.matchAllQuery();
		FilterBuilder fb = FilterBuilders.prefixFilter("file", query);

		SearchResponse searchHits = client.prepareSearch()
										.setIndices(ATTACHMENT_INDEX_NAME, BEAN_INDEX_NAME)
										.setTypes(ATTACHMENT_INDEX_TYPE, BEAN_INDEX_TYPE)
										.addFacet(FacetBuilders.termsFacet("autocomplete").field(FILE).facetFilter(fb))
										.execute().actionGet();

		TermsFacet terms = searchHits.getFacets().facet("autocomplete");
		for (Entry entry : terms.getEntries()) {
			results.add(entry.getTerm().string());
		}

		return results;
	}
*/
}
