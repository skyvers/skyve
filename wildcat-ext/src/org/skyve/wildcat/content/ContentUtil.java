package org.skyve.wildcat.content;

import java.io.File;
import java.io.IOException;
import java.text.NumberFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.Map;
import java.util.Stack;
import java.util.StringTokenizer;

import javax.jcr.Item;
import javax.jcr.NoSuchWorkspaceException;
import javax.jcr.Node;
import javax.jcr.PathNotFoundException;
import javax.jcr.Repository;
import javax.jcr.RepositoryException;
import javax.jcr.Session;
import javax.jcr.SimpleCredentials;
import javax.jcr.Value;
import javax.jcr.query.Query;
import javax.jcr.query.Row;
import javax.jcr.query.RowIterator;
import javax.jcr.version.Version;
import javax.jcr.version.VersionHistory;
import javax.jcr.version.VersionIterator;

import org.apache.jackrabbit.api.JackrabbitWorkspace;
import org.apache.jackrabbit.core.NodeImpl;
import org.apache.jackrabbit.core.TransientRepository;
import org.apache.jackrabbit.spi.commons.name.NameConstants;
import org.skyve.content.MimeType;
import org.skyve.domain.Bean;
import org.skyve.wildcat.util.UtilImpl;

public class ContentUtil {
	static final String SEARCH_USER = "search";
	static final String FULL_USER = "full";

	private static final Repository REPOSITORY;
	static {
		try {
			REPOSITORY = new TransientRepository(UtilImpl.CONTENT_DIRECTORY + File.separator + "repository.xml", 
													UtilImpl.CONTENT_DIRECTORY);
		}
		catch (IOException e) {
			throw new IllegalStateException("Cannot initialise Content repository " + e);
		}
	}

	public static Session getSearchSession(String customerName) 
	throws RepositoryException, IOException {
		Session result = null;
		try {
			result = REPOSITORY.login(new SimpleCredentials(SEARCH_USER, "password".toCharArray()), 
										customerName);
		}
		catch (NoSuchWorkspaceException e) {
			createWorkspace(customerName);
			result = REPOSITORY.login(new SimpleCredentials(SEARCH_USER, "password".toCharArray()), 
										customerName);
		}

		return result;
	}

	public static Session getFullSession(String customerName) 
	throws RepositoryException, IOException {
		Session result = null;
		try {
			result = REPOSITORY.login(new SimpleCredentials(FULL_USER, "password".toCharArray()), 
										customerName);
		}
		catch (NoSuchWorkspaceException e) {
			createWorkspace(customerName);
			result = REPOSITORY.login(new SimpleCredentials(FULL_USER, "password".toCharArray()), 
										customerName);
		}

		return result;
	}

	public static Session getDefaultSession()
	throws RepositoryException, IOException {
		return getFullSession("default");
	}
	
	private static void createWorkspace(String customerName) 
	throws RepositoryException, IOException {
		Session defaultSession = getDefaultSession();
		try {
			JackrabbitWorkspace ws = (JackrabbitWorkspace) defaultSession.getWorkspace();
			ws.createWorkspace(customerName);
		}
		finally {
			defaultSession.logout();
		}
	}

	private static class NodeAndBinding {
		private Node parent;
		private String binding;
	}

	@SuppressWarnings("synthetic-access")
	private static NodeAndBinding makePath(Session session, String path) 
	throws RepositoryException {
		Node rootNode = session.getRootNode();
		String customerName = session.getWorkspace().getName();
		Node customerNode = null;
		if (rootNode.hasNode(customerName)) {
			customerNode = rootNode.getNode(customerName);
		}
		else {
			customerNode = rootNode.addNode(customerName, "nt:unstructured");
		}
		String binding = null;

		// Save document path
		StringTokenizer s = new StringTokenizer(path, "/");
		Node parent = customerNode;
		while (s.hasMoreTokens()) {
			String token = s.nextToken();
			if (s.hasMoreTokens()) {
				Node child = null;
				if (parent.hasNode(token)) {
					child = parent.getNode(token);
				}
				else {
					child = parent.addNode(token, "nt:unstructured");
				}
				parent = child;
			}
			else {
				binding = token;
			}
		}

		NodeAndBinding result = new NodeAndBinding();
		result.parent = parent;
		result.binding = binding;

		return result;
	}

	@SuppressWarnings("synthetic-access")
	public static void put(Session session, BeanContent content) 
	throws RepositoryException {
		Node beanNode = makePath(session, content.getPath() + "/extra").parent;

		Map<String, String> properties = content.getProperties();
		for (String propertyName : properties.keySet()) {
			beanNode.setProperty(propertyName, properties.get(propertyName));
		}
	}

	public static void move(Session session, BeanContent content, String fromBizDataGroupId, String fromBizUserId)
	throws RepositoryException {
		String newPath = content.getPath();
		String oldPath = Content.createPathString(content.getBizModule(), 
													content.getBizDocument(), 
													fromBizDataGroupId,
													fromBizUserId, 
													content.getBizId(), 
													null);
		if (UtilImpl.CONTENT_TRACE) UtilImpl.LOGGER.info("Move node from " + oldPath + " to " + newPath);
		session.move(oldPath, newPath);
	}

	@SuppressWarnings("synthetic-access")
	public static StreamContent put(Session session, StreamContent content, boolean forRestore) 
	throws RepositoryException, IOException {
		Node fileNode = null; // represents the file
		Node resourceNode = null; // file resource (child with input stream in it)
		if ((! forRestore) && (content.getUuid() != null)) { // update
			fileNode = session.getNodeByUUID(content.getUuid());
			if (content.isVersionable()) {
				fileNode.checkout();
			}
			resourceNode = fileNode.getNode("jcr:content");
		}
		else { // insert
			NodeAndBinding nodeAndBinding = makePath(session, content.getPath());
			// add or get file node
			if (nodeAndBinding.parent.hasNode(nodeAndBinding.binding)) {
				fileNode = nodeAndBinding.parent.getNode(nodeAndBinding.binding);
			}
			else {
				if (forRestore) {
					fileNode = ((NodeImpl) nodeAndBinding.parent).addNodeWithUuid(nodeAndBinding.binding, 
																					"nt:file", 
																					content.getUuid());
				}
				else {
					fileNode = nodeAndBinding.parent.addNode(nodeAndBinding.binding, "nt:file");
				}
			}
			if (content.isVersionable()) {
				if (fileNode.canAddMixin("mix:versionable")) {
					fileNode.addMixin("mix:versionable");
				}
			}
			else {
				if (fileNode.canAddMixin("mix:referenceable")) {
					fileNode.addMixin("mix:referenceable");
				}
			}
			if (fileNode.hasNode("jcr:content")) {
				resourceNode = fileNode.getNode("jcr:content");
			}
			else {
				resourceNode = fileNode.addNode("jcr:content", "nt:resource");
			}
		}

		try {
			resourceNode.setProperty("jcr:data", content.getStream());
		}
		finally {
			content.getStream().close();
		}

		resourceNode.setProperty("jcr:mimeType", content.getMimeType().toString());
		Calendar lastModified = Calendar.getInstance();
		resourceNode.setProperty("jcr:lastModified", lastModified);

		session.save();
		if (content.isVersionable()) {
			fileNode.checkin();

			content.setVersions(new Stack<Date>());
			VersionHistory history = fileNode.getVersionHistory();
			for (VersionIterator it = history.getAllVersions(); it.hasNext();) {
				Version version = (Version) it.next();
				content.getVersions().add(version.getCreated().getTime());
			}
		}
		content.setLastModified(lastModified.getTime());
		content.setUuid(fileNode.getProperty("jcr:uuid").getString());
		return content;
	}

	public static StreamContent get(Session session, String uuid) 
	throws RepositoryException {
		Node fileNode = session.getNodeByUUID(uuid);
		Node resourceNode = fileNode.getNode("jcr:content");

		StreamContent result = new StreamContent(session.getWorkspace().getName(), fileNode.getPath());
		result.setUuid(uuid);
		result.setMimeType(MimeType.fromMimeType(resourceNode.getProperty("jcr:mimeType").getString()));
		result.setLastModified(resourceNode.getProperty("jcr:lastModified").getDate().getTime());
		result.setStream(resourceNode.getProperty("jcr:data").getStream());

		if (fileNode.isNodeType(NameConstants.MIX_VERSIONABLE.getLocalName())) {
			VersionHistory history = fileNode.getVersionHistory();
			if (history != null) {
				for (VersionIterator it = history.getAllVersions(); it.hasNext();) {
					Version version = (Version) it.next();
					result.getVersions().add(version.getCreated().getTime());
				}
			}
		}

		return result;
	}

	/**
	 * Remove the node with the given path. 
	 * NB Do not remove parents with no children as they could be a bean leaf node with indexed attributes. 
	 * This means that we potentially leave orphaned nodes that are no longer used - oh well.
	 * 
	 * @param session
	 * @param path
	 * @throws RepositoryException
	 */
	private static void remove(Session session, String path) 
	throws RepositoryException {
		if (UtilImpl.CONTENT_TRACE) UtilImpl.LOGGER.info("Remove node " + path);
		String customerName = session.getWorkspace().getName();

		try {
			Node node = session.getRootNode().getNode(customerName + path);
			Node parent = node.getParent();
			while ((parent.getDepth() > 0) && (parent.getNodes().getSize() <= 1L)) {
				node = parent;
				parent = parent.getParent();
			}
			node.remove();
		}
		catch (PathNotFoundException e) {
			// no node exists so nothing to do...
			if (UtilImpl.CONTENT_TRACE) UtilImpl.LOGGER.info("Cannot remove node (not found) at " + path);
		}
	}

	public static void remove(Session session, Bean bean, String attributeName) 
	throws RepositoryException {
		remove(session, 
				Content.createPathString(bean.getBizModule(), 
											bean.getBizDocument(), 
											bean.getBizDataGroupId(), 
											bean.getBizUserId(), 
											bean.getBizId(), 
											attributeName));
	}

	public static void remove(Session session, Bean bean) throws RepositoryException {
		remove(session, 
				Content.createPathString(bean.getBizModule(), 
											bean.getBizDocument(), 
											bean.getBizDataGroupId(), 
											bean.getBizUserId(), 
											bean.getBizId(), 
											null));
	}

	public static SearchResults search(Session session, String keywords, Long firstResult, Integer maxResults)
	throws RepositoryException {
		return search(session, "//*", keywords, firstResult, maxResults);
	}
	
	public static SearchResults search(Session session,
										String moduleName,
										String documentName,
										String keywords,
										Long firstResult,
										Integer maxResults)
	throws RepositoryException {
		StringBuilder path = new StringBuilder(128);
		path.append("//").append(session.getWorkspace().getName());
		path.append('/').append(moduleName);
		path.append('/').append(documentName).append('*');
		return search(session, path.toString(), keywords, firstResult, maxResults);
	}

	public static SearchResults search(Session session, String queryPath, String keywords, Long firstResult, Integer maxResults)
	throws RepositoryException {
		SearchResults results = new SearchResults();

		String stmt = queryPath + "[jcr:contains(., '" + 
						keywords.replaceAll("'", "''") + 
						"')]/rep:excerpt(.) order by jcr:score() descending";
		Query query = session.getWorkspace().getQueryManager().createQuery(stmt, Query.XPATH);

		long time = System.currentTimeMillis();
		RowIterator rows = query.execute().getRows();
		time = System.currentTimeMillis() - time;
		NumberFormat nf = NumberFormat.getNumberInstance();
		nf.setMaximumFractionDigits(2);
		nf.setMinimumFractionDigits(2);
		results.setSearchTimeInSecs(nf.format(time / 1000d));
		results.setTotalResults(rows.getSize());

		long from = 0L;
		long to = Long.MAX_VALUE;
		if (firstResult != null) {
			from = firstResult.longValue();
			rows.skip(from);
		}
		if (maxResults != null) {
			to = from + maxResults.intValue();
		}

		for (long l = from; (l < to) && rows.hasNext(); l++) {
			Row row = rows.nextRow();
			SearchResult result = new SearchResult();
			result.setExcerpt(row.getValue("rep:excerpt(.)").getString());
			try {
			    result.setScore(Integer.parseInt(row.getValue("jcr:score").getString()) / 10);
			}
			catch (NumberFormatException e) {
			    result.setScore(0);
			}
			if (UtilImpl.CONTENT_TRACE) UtilImpl.LOGGER.info(row.getValue("jcr:path").getString());
			Item item = session.getItem(row.getValue("jcr:path").getString());
			Node node = item.getParent();
			if (node.hasProperty("jcr:uuid")) { // stream node
				result.setContentId(node.getProperty("jcr:uuid").getString());
				result.setBinding(node.getName());
				node = node.getParent();
				result.setBizId(node.getName());
				node = node.getParent();
			}
			else { // bean content
				result.setBizId(item.getName());
			}
			node = node.getParent().getParent(); // remove user and data group from path

			result.setDocumentName(node.getName());
			node = node.getParent();
			result.setModuleName(node.getName());

			results.getResults().add(result);
		}

		if (rows.getSize() < 5) {
			stmt = "/jcr:root[rep:spellcheck('" + keywords + "')]/rep:spellcheck()";
			RowIterator suggestionRow = session.getWorkspace().getQueryManager().createQuery(stmt, Query.XPATH).execute().getRows();
			if (suggestionRow.hasNext()) {
				Value v = suggestionRow.nextRow().getValue("rep:spellcheck()");
				if (v != null) {
					results.setSuggestion(v.getString());
				}
			}
		}

		return results;
	}
}
