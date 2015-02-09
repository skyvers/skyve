package org.skyve.wildcat.content.elasticsearch;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;

import org.apache.commons.codec.binary.Base64;
import org.apache.tika.Tika;
import org.apache.tika.metadata.HttpHeaders;
import org.apache.tika.metadata.MSOffice;
import org.apache.tika.metadata.Metadata;
import org.apache.tika.metadata.TikaCoreProperties;
import org.elasticsearch.action.get.GetResponse;
import org.elasticsearch.action.index.IndexResponse;
import org.elasticsearch.action.search.SearchResponse;
import org.elasticsearch.action.search.SearchType;
import org.elasticsearch.client.Client;
import org.elasticsearch.common.Strings;
import org.elasticsearch.common.io.stream.BytesStreamInput;
import org.elasticsearch.common.text.Text;
import org.elasticsearch.common.xcontent.XContentBuilder;
import org.elasticsearch.common.xcontent.XContentFactory;
import org.elasticsearch.index.get.GetField;
import org.elasticsearch.index.query.FilterBuilder;
import org.elasticsearch.index.query.FilterBuilders;
import org.elasticsearch.index.query.QueryBuilder;
import org.elasticsearch.index.query.QueryBuilders;
import org.elasticsearch.node.Node;
import org.elasticsearch.search.SearchHit;
import org.elasticsearch.search.SearchHitField;
import org.elasticsearch.search.facet.FacetBuilders;
import org.elasticsearch.search.facet.terms.TermsFacet;
import org.elasticsearch.search.facet.terms.TermsFacet.Entry;
import org.elasticsearch.search.highlight.HighlightField;
import org.skyve.content.MimeType;
import org.skyve.domain.Bean;
import org.skyve.domain.messages.DomainException;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.user.User;
import org.skyve.wildcat.content.AbstractContentManager;
import org.skyve.wildcat.content.AttachmentContent;
import org.skyve.wildcat.content.BeanContent;
import org.skyve.wildcat.content.SearchResult;
import org.skyve.wildcat.content.SearchResults;
import org.skyve.wildcat.metadata.user.SuperUser;
import org.skyve.wildcat.persistence.AbstractPersistence;
import org.skyve.wildcat.util.TimeUtil;
import org.skyve.wildcat.util.UtilImpl;

public class ESClient extends AbstractContentManager {
	static final String ATTACHMENT_INDEX_NAME = "attachments";
	static final String ATTACHMENT_INDEX_TYPE = "attachment";
	static final String BEAN_INDEX_NAME = "beans";
	static final String BEAN_INDEX_TYPE = "bean";
    private static final String CONTENT = "content";
    private static final String ATTACHMENT = "attachment";

	private static final String FILE = "file";
    public static final String FILE_CONTENT_TYPE = "file.content_type";
    private static final String FILENAME = "filename";
    private static final String FILE_FILENAME = "file.filename";
    public static final String FILE_LAST_MODIFIED = "file.last_modified";
    public static final String LAST_MODIFIED = "last_modified";
    private static final String CONTENT_TYPE = "content_type";
    private static final String FILESIZE = "filesize";

    private static final String META = "meta";
    private static final String META_TITLE = "meta.title";
    private static final String AUTHOR = "author";
    private static final String TITLE = "title";
    private static final String DATE = "date";
    private static final String KEYWORDS = "keywords";

    private static final String BEAN = "bean";
    private static final String ATTRIBUTE_NAME = "attribute";
    static final String BEAN_CUSTOMER_NAME = "bean." + Bean.CUSTOMER_NAME;
    static final String BEAN_MODULE_KEY = "bean." + Bean.MODULE_KEY;
    static final String BEAN_DOCUMENT_KEY = "bean." + Bean.DOCUMENT_KEY;
    static final String BEAN_DATA_GROUP_ID = "bean." + Bean.DATA_GROUP_ID;
    static final String BEAN_USER_ID = "bean." + Bean.USER_ID;
    static final String BEAN_DOCUMENT_ID = "bean." + Bean.DOCUMENT_ID;
    static final String BEAN_ATTRIBUTE_NAME = "bean.attribute";
	
	private static Node node = ESUtil.localNode();
	private static final Tika TIKA = new Tika();

	private Client client = null;
	
	public ESClient() {
		client = ESUtil.localClient(node);
	}
	
	@Override
	public void init() throws Exception {
		ESUtil.prepareIndex(client, ESClient.ATTACHMENT_INDEX_NAME, ESClient.ATTACHMENT_INDEX_TYPE);
		ESUtil.prepareIndex(client, ESClient.BEAN_INDEX_NAME, ESClient.BEAN_INDEX_TYPE);
	}

	@Override
	public void dispose() throws Exception {
		ESUtil.close(node);
	}

	@Override
	public void close() {
		client.close();
	}
	
	@Override
	public void put(BeanContent content)
	throws Exception {
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
			
			if (UtilImpl.CONTENT_TRACE) UtilImpl.LOGGER.info("ESClient.put(): " + source.string());
			client.prepareIndex(BEAN_INDEX_NAME, 
									BEAN_INDEX_TYPE,
									content.getBizId()).setSource(source).execute().actionGet();
		}
	}
	
	@Override
	public void put(AttachmentContent attachment)
	throws Exception {
		try (XContentBuilder source = XContentFactory.jsonBuilder().startObject()) {
		
			// Set the maximum length of strings returned by the parseToString method, -1 sets no limit
			byte[] content = attachment.getContentBytes();
			try (BytesStreamInput contentStream = new BytesStreamInput(content, false)) {
				Metadata metadata = new Metadata();
				String parsedContent = TIKA.parseToString(contentStream, metadata, 100000);
				
				// File
				MimeType contentType = attachment.getMimeType();
				source.startObject(FILE)
						.field(FILENAME, attachment.getFileName())
						.field(LAST_MODIFIED, new Date())
						.field(CONTENT_TYPE,
								(contentType != null) ? 
									contentType.toString() : 
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
		
				// Doc as binary attachment
				source.field(ATTACHMENT, new String(new Base64().encode(content)));
				// End of our document
				source.endObject();
			}
			
			if (UtilImpl.CONTENT_TRACE) UtilImpl.LOGGER.info("ESClient.put(): " + source.string());
			IndexResponse response = client.prepareIndex(ATTACHMENT_INDEX_NAME, 
															ATTACHMENT_INDEX_TYPE,
															attachment.getContentId())
										.setSource(source).execute().actionGet();
			if (response.isCreated()) {
				attachment.setContentId(response.getId());
			}
		}
	}

	@Override
	public AttachmentContent get(String contentId) throws Exception {
		GetResponse response = client.prepareGet(ATTACHMENT_INDEX_NAME, ATTACHMENT_INDEX_TYPE, contentId)
									.setFields(ATTACHMENT, 
												FILE_FILENAME,
												FILE_CONTENT_TYPE, 
												FILE_LAST_MODIFIED,
												BEAN_CUSTOMER_NAME,
												BEAN_MODULE_KEY,
												BEAN_DOCUMENT_KEY,
												BEAN_DATA_GROUP_ID,
												BEAN_USER_ID,
												BEAN_DOCUMENT_ID,
												BEAN_ATTRIBUTE_NAME).get();
		if (! response.isExists()) {
			if (UtilImpl.CONTENT_TRACE) UtilImpl.LOGGER.info("ESClient.get(" + contentId + "): DNE");
			return null;
		}

		GetField field = response.getField(FILE_CONTENT_TYPE);
		MimeType mimeType = MimeType.plain;
		if (field != null) {
			String content_type = (String) field.getValue();
			mimeType = MimeType.fromMimeType(content_type);
		}
		String content = (String) response.getField(ATTACHMENT).getValue();
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
		result.setContentId(response.getId());
		if (UtilImpl.CONTENT_TRACE) UtilImpl.LOGGER.info("ESClient.get(" + contentId + "): exists");

		return result;
	}
	
	@Override
	public void remove(BeanContent content) {
		if (UtilImpl.CONTENT_TRACE) UtilImpl.LOGGER.info("ESClient.remove(" + content.getBizId() + ")");
		client.prepareDelete(BEAN_INDEX_NAME,
								BEAN_INDEX_TYPE,
								content.getBizId()).execute().actionGet();
	}

	@Override
	public void remove(String contentId) {
		if (UtilImpl.CONTENT_TRACE) UtilImpl.LOGGER.info("ESClient.remove(" + contentId + ")");
		client.prepareDelete(ATTACHMENT_INDEX_NAME,
								ATTACHMENT_INDEX_TYPE,
								contentId).execute().actionGet();
	}

	@Override
	public void truncate(String customerName) throws Exception {
		if (UtilImpl.CONTENT_TRACE) UtilImpl.LOGGER.info("ESClient.truncate(" + customerName + ")");
		client.prepareDeleteByQuery()
			.setIndices(ATTACHMENT_INDEX_NAME, BEAN_INDEX_NAME)
			.setTypes(ATTACHMENT_INDEX_TYPE, BEAN_INDEX_TYPE)
			.setQuery(QueryBuilders.termQuery(BEAN_CUSTOMER_NAME, customerName)).execute().actionGet();
	}

	@Override
	public SearchResults google(String search, int maxResults)
	throws Exception {
		QueryBuilder qb;
		if ((search == null) || search.trim().isEmpty()) {
			qb = QueryBuilders.matchAllQuery();
		}
		else {
			qb = QueryBuilders.queryString(search).defaultField("_all");
		}

		SearchResponse searchResponse = client.prepareSearch()
											.setIndices(ATTACHMENT_INDEX_NAME, BEAN_INDEX_NAME)
											.setTypes(ATTACHMENT_INDEX_TYPE, BEAN_INDEX_TYPE)
											.setSearchType(SearchType.DFS_QUERY_THEN_FETCH).setQuery(qb)
											.setFrom(0).setSize(10000)
											.addHighlightedField(FILE_FILENAME)
											.addHighlightedField(CONTENT)
											.addHighlightedField(META_TITLE)
											.setHighlighterPreTags("<strong>")
											.setHighlighterPostTags("</strong>")
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
			String bizCustomer = (String) searchHit.field(BEAN_CUSTOMER_NAME).value();
			String bizModule = (String) searchHit.field(BEAN_MODULE_KEY).value();
			String bizDocument = (String) searchHit.field(BEAN_DOCUMENT_KEY).value();
			String bizDataGroupId = null;
			SearchHitField field = searchHit.field(BEAN_DATA_GROUP_ID);
			if (field != null) {
				bizDataGroupId = (String) field.value();
			}
			String bizUserId = (String) searchHit.field(BEAN_USER_ID).value();
			String bizId = (String) searchHit.field(BEAN_DOCUMENT_ID).value();
			if (allow(bizCustomer,
						bizModule,
						bizDocument,
						bizDataGroupId,
						bizUserId,
						bizId)) {
				SearchResult hit = new SearchResult();
	
				hit.setCustomerName(bizCustomer);
				hit.setModuleName(bizModule);
				hit.setDocumentName(bizDocument);
				hit.setBizId(bizId);
				hit.setScore((int) (searchHit.score() * 100.0));
	
				field = searchHit.field(BEAN_ATTRIBUTE_NAME);
				if (field != null) {
					hit.setAttributeName((String) field.getValue());
				}
				field = searchHit.field(LAST_MODIFIED);
				if (field != null) {
					hit.setLastModified(TimeUtil.parseISODate((String) field.getValue()));
				}
				field = searchHit.field(FILE_LAST_MODIFIED);
				if (field != null) {
					hit.setLastModified(TimeUtil.parseISODate((String) field.getValue()));
				}
				hit.setContentId(searchHit.getId());
/*			
				hit.setSource(searchHit.getSourceAsString());
	
				if (searchHit.getFields() != null) {
					if (searchHit.getFields().get(FILE_CONTENT_TYPE) != null) {
						hit.setContentType((String) searchHit.getFields().get(FILE_CONTENT_TYPE).getValue());
					}
				}
	
				if (searchHit.getSource() != null) {
					hit.setTitle(ESUtil.getSingleStringValue(META_TITLE, searchHit.getSource()));
				}
*/
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
	}

	private static boolean allow(String bizCustomer,
									String bizModule,
									String bizDocument,
									String bizDataGroupId,
									String bizUserId,
									String bizId) {
		AbstractPersistence persistence = AbstractPersistence.get();
		User user = persistence.getUser();
		if (user instanceof SuperUser) {
			return true;
		}

		try {
			if (! user.canReadBean(bizId, bizModule, bizDocument, bizCustomer, bizDataGroupId, bizUserId)) {
				return false;
			}
		}
		catch (MetaDataException e) {
			// This can happen when a document was indexed but then the customer access was taken away
			if (UtilImpl.SECURITY_TRACE) System.err.println("Could not get the document for " + bizModule + '.' + bizDocument);
			return false;
		}
		catch (DomainException e) {
			// This happens when the data was deleted but the CMS was not kept in sync
			if (UtilImpl.SECURITY_TRACE) System.err.println("Could not retrieve bean " + bizModule + '.' + bizDocument + " with ID " + bizId);
			return false;
		}

		return true;
	}
	
	@Override
	public Iterable<SearchResult> all() throws Exception {
		return new ESIterable(client);
	}

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
}
