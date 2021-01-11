package org.skyve.impl.content.elastic;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.List;
import java.util.Map;

//import org.elasticsearch.action.admin.indices.analyze.AnalyzeRequest;
//import org.elasticsearch.action.admin.indices.analyze.AnalyzeResponse;
//import org.elasticsearch.action.admin.indices.create.CreateIndexResponse;
//import org.elasticsearch.action.admin.indices.delete.DeleteIndexResponse;
//import org.elasticsearch.action.admin.indices.mapping.get.GetMappingsResponse;
//import org.elasticsearch.action.admin.indices.mapping.put.PutMappingRequestBuilder;
//import org.elasticsearch.action.admin.indices.mapping.put.PutMappingResponse;
//import org.elasticsearch.client.Client;
//import org.elasticsearch.client.transport.TransportClient;
//import org.elasticsearch.common.settings.ImmutableSettings;
//import org.elasticsearch.common.settings.Settings;
//import org.elasticsearch.common.transport.InetSocketTransportAddress;
//import org.elasticsearch.common.xcontent.XContentBuilder;
//import org.elasticsearch.common.xcontent.XContentFactory;
//import org.elasticsearch.common.xcontent.support.XContentMapValues;
//import org.elasticsearch.indices.IndexAlreadyExistsException;
//import org.elasticsearch.node.Node;
//import org.elasticsearch.node.NodeBuilder;
import org.skyve.impl.util.UtilImpl;
import org.skyve.util.Util;

public class ElasticUtil {
	private static final String CLUSTER_NAME = "SKYVE_CONTENT";

	private ElasticUtil() {
		// disallow instantiation
	}
/*	
	static Node localNode() {
		Settings settings = ImmutableSettings.settingsBuilder().
								put("http.enabled", false).
								put("path.data", UtilImpl.CONTENT_DIRECTORY).build();
		return NodeBuilder.nodeBuilder().local(true).data(true).clusterName(CLUSTER_NAME).settings(settings).node();
	}
	
	static Client localClient(Node node) {
		return node.client();
	}
	
	static void close(Node node) {
		if ((node != null) && (! node.isClosed())) {
			node.close();
		}
	}
	
	static Node remoteNode() {
		return NodeBuilder.nodeBuilder().local(false).data(false).clusterName(CLUSTER_NAME).node();
	}

	static Client remoteClient() {
		Settings settings = ImmutableSettings.settingsBuilder().
								put("client.transport.sniff", true).
								put("cluster.name", CLUSTER_NAME).build();
		TransportClient client = new TransportClient(settings);
		client.addTransportAddress(new InetSocketTransportAddress("localhost", 9300));
//		client.addTransportAddress(new InetSocketTransportAddress("host2", 9300));
		
		return client;
	}
	
	static void prepareIndex(Client client, String index, String type, XContentBuilder xcontent)
	throws Exception {
		if (! doesIndexExist(client, index)) {
			try {
				createIndex(client, index);
			}
			// doesIndexExist doesn't seem to work in the current version of elastic search
			catch (@SuppressWarnings("unused") IndexAlreadyExistsException e) {
				// be silent
			}
		}
		if (! doesTypeExist(client, index, type)) {
			putMapping(client, index, type, xcontent);
		}
	}

	static void prepareIndex(Client client, String index, String type)
	throws Exception {
		prepareIndex(client, index, type, null);
	}
	
	private static boolean doesIndexExist(Client client, String index) {
		return client.admin().indices().prepareExists(index).execute().actionGet().isExists();
	}
	
	private static boolean doesTypeExist(Client client, String index, String type) {
		return client.admin().indices().prepareExists(index, type).execute().actionGet().isExists();
	}
	
	private static boolean doesMappingExist(Client client, String index, String type) {
		GetMappingsResponse mappingsResponse = client.admin().indices().prepareGetMappings(index).setTypes(type).get();
        if (mappingsResponse.getMappings().get(index) == null) {
            return false;
        }
		return mappingsResponse.getMappings().get(index).containsKey(type);
	}

	private static void putMapping(Client client, String index, String type, XContentBuilder xcontent)
	throws Exception {
		// If type does not exist, we create it
		boolean mappingExist = doesMappingExist(client, index, type);
		if (! mappingExist) {
			String source = null;
			// Read the mapping json file if exists and use it
			if (xcontent == null) source = readJsonDefinition(type);
			
			if (source != null || xcontent != null) {
				PutMappingRequestBuilder pmrb = client.admin().indices().preparePutMapping(index).setType(type);
				if (source != null) {
					pmrb.setSource(source);
				}
				
				if (xcontent != null) {
					pmrb.setSource(xcontent);
				}
				
				// Create type and mapping
				PutMappingResponse response = pmrb.execute().actionGet();			
				if (! response.isAcknowledged()) {
					throw new Exception("Could not define mapping for type ["+index+"]/["+type+"].");
				}
			}
			else {
				Util.LOGGER.info("No mapping definition for ["+index+"]/["+type+"]. Ignoring.");
			}
		}
	}

	private static String readJsonDefinition(String type) throws Exception {
		return readFileInClasspath("/org/skyve/util/content/elasticsearch/" + type + ".json");
	}	

	private static String readFileInClasspath(String url) throws Exception {
		StringBuilder result = new StringBuilder(1024);
		
		try {
			try (InputStream is = ElasticUtil.class.getResourceAsStream(url)) {
				try (InputStreamReader isr = new InputStreamReader(is)) {
					try (BufferedReader br = new BufferedReader(isr)) {
						String line;
						while ((line = br.readLine()) != null) {
							result.append(line);
						}
					}
				}
			}
		}
		catch (@SuppressWarnings("unused") Exception e){
			return null;
		}

		return result.toString();
	}	

	static void createIndex(Client client, String index)
	throws Exception {
		String settings = "{\"analysis\": {\"analyzer\": {\"default\": {\"type\": \"english\"}}}}";
		CreateIndexResponse dir = client.admin().indices().prepareCreate(index).setSettings(settings).execute().actionGet();
		if (! dir.isAcknowledged()) {
			throw new Exception("ES did not acknowledge index creation...");
		}
	}

	static void deleteIndex(Client client, String index)
	throws Exception {
		DeleteIndexResponse dir = client.admin().indices().prepareDelete(index).execute().actionGet();
		if (! dir.isAcknowledged()) {
			throw new Exception("ES did not acknowledge index removal...");
		}
	}
	
	static String analyze(Client client) throws Exception {
		try (XContentBuilder xcb = XContentFactory.jsonBuilder()) {
			AnalyzeRequest req = new AnalyzeRequest(ElasticContentManager.BEAN_INDEX_NAME, "this is a tests");//.analyzer("english");
			AnalyzeResponse analyzeResponse = client.admin().indices().analyze(req).actionGet();
			return analyzeResponse.toXContent(xcb, null).string();
		}
	}
	
	static String hash(String toSign) throws NoSuchAlgorithmException {

		MessageDigest md = MessageDigest.getInstance("MD5");
		md.update(toSign.getBytes());

		String key = "";
		byte b[] = md.digest();
		for (int i = 0; i < b.length; i++) {
			long t = b[i] < 0 ? 256 + b[i] : b[i];
			key += Long.toHexString(t);
		}

		return key;
	}


	static String getSingleStringValue(String path, Map<String, Object> content) {
		List<Object> obj = XContentMapValues.extractRawValues(path, content);
		if (obj.isEmpty()) {
			return null;
		}
		return ((String) obj.get(0));
	}

	public static void main(String[] args) 
	throws Exception {
		UtilImpl.CONTENT_DIRECTORY = "/C:/_/skyve/skyve-ee/content/";
		try (Node n = localNode()) {
			try (Client c = localClient(n)) {
				ElasticUtil.prepareIndex(c, ElasticContentManager.ATTACHMENT_INDEX_NAME, ElasticContentManager.ATTACHMENT_INDEX_TYPE);
				ElasticUtil.prepareIndex(c, ElasticContentManager.BEAN_INDEX_NAME, ElasticContentManager.BEAN_INDEX_TYPE);
				Thread.sleep(10000);
				System.out.println(analyze(c));
			}
		}
	}
*/
}
