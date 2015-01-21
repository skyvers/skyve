package org.skyve.wildcat.content.elasticsearch;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.List;
import java.util.Map;

import org.elasticsearch.action.admin.indices.create.CreateIndexResponse;
import org.elasticsearch.action.admin.indices.delete.DeleteIndexResponse;
import org.elasticsearch.action.admin.indices.mapping.get.GetMappingsResponse;
import org.elasticsearch.action.admin.indices.mapping.put.PutMappingRequestBuilder;
import org.elasticsearch.action.admin.indices.mapping.put.PutMappingResponse;
import org.elasticsearch.client.Client;
import org.elasticsearch.client.transport.TransportClient;
import org.elasticsearch.common.settings.ImmutableSettings;
import org.elasticsearch.common.settings.Settings;
import org.elasticsearch.common.transport.InetSocketTransportAddress;
import org.elasticsearch.common.xcontent.XContentBuilder;
import org.elasticsearch.common.xcontent.support.XContentMapValues;
import org.elasticsearch.indices.IndexAlreadyExistsException;
import org.elasticsearch.node.Node;
import org.elasticsearch.node.NodeBuilder;
import org.skyve.util.Util;
import org.skyve.wildcat.util.UtilImpl;

public class ESUtil {
	private static final String CLUSTER_NAME = "WILDCAT_CONTENT";

	private ESUtil() {
		// disallow instantiation
	}
	
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
			catch (IndexAlreadyExistsException e) {
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
		return readFileInClasspath("/estemplate/" + type + ".json");
	}	

	private static String readFileInClasspath(String url) throws Exception {
		StringBuilder result = new StringBuilder(1024);
		
		try {
			try (InputStream is= ESUtil.class.getResourceAsStream(url)) {
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
		catch (Exception e){
			return null;
		}

		return result.toString();
	}	

	static void createIndex(Client client, String index)
	throws Exception {
		CreateIndexResponse dir = client.admin().indices().prepareCreate(index).execute().actionGet();
		if (! dir.isAcknowledged()) {
			throw new Exception("ES did not acknowledge index removal...");
		}
	}

	static void deleteIndex(Client client, String index)
	throws Exception {
		DeleteIndexResponse dir = client.admin().indices().prepareDelete(index).execute().actionGet();
		if (! dir.isAcknowledged()) {
			throw new Exception("ES did not acknowledge index removal...");
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
/*
	public static void main(String[] args) 
	throws Exception {
//		Node n = localNode();
//		Client c = localClient(node);

		Node n = remoteNode();
		Client c = remoteClient();
		
		String content;
		try (BufferedInputStream bis = new BufferedInputStream(new FileInputStream("/Users/mike/Bill_everyone_2.csv"))) {
			try (ByteArrayOutputStream baos = new ByteArrayOutputStream()) {
				byte[] bytes = new byte[1024]; // 1K
				int bytesRead = 0;
				while ((bytesRead = bis.read(bytes)) > 0) {
					baos.write(bytes, 0, bytesRead);
				}
				content = new String(new Base64().encode(baos.toByteArray()));
			}
		}

		ESAttachmentContent d = new ESAttachmentContent("poo", content);
		putDocument(c, d);
		System.out.println("ID = " + d.getId());
		System.out.println("TYPE = " + d.getType());
		System.out.println("CONTENT = " + d.getContent());
		System.out.println("CONTENT TYPE = " + d.getContentType());
		d = getDocument(c, null, null, d.getId());
		System.out.println("ID = " + d.getId());
		System.out.println("TYPE = " + d.getType());
		System.out.println("CONTENT = " + d.getContent());
		System.out.println("CONTENT TYPE = " + d.getContentType());

		SearchResults r = google(c, "ina", 0, 10);
		System.out.println(r.getTotalHits());
		for (Hit h : r.getHits()) {
			System.out.println('X');
			for (String hl : h.getHighlights()) {
				System.out.println(hl);
			}
		}
		close(n);
		
//		c = transportClient();
	}
*/
}
