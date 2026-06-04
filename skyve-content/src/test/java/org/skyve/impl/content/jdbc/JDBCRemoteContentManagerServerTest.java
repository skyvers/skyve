package org.skyve.impl.content.jdbc;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Proxy;
import java.sql.CallableStatement;
import java.sql.Connection;
import java.sql.DriverManager;
import java.util.Map;
import java.util.TreeMap;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.content.AttachmentContent;
import org.skyve.content.BeanContent;
import org.skyve.content.ContentIterable;
import org.skyve.content.SearchResult;
import org.skyve.content.SearchResults;
import org.skyve.domain.PersistentBean;
import org.skyve.impl.cache.StateUtil;
import org.skyve.impl.content.AbstractContentManager;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.UUIDv7;
import org.skyve.persistence.DataStore;

@SuppressWarnings("static-method")
class JDBCRemoteContentManagerServerTest {
	private final String originalServerArgs = UtilImpl.CONTENT_JDBC_SERVER_ARGS;
	private final Class<? extends AbstractContentManager> originalImplementationClass = AbstractContentManager.IMPLEMENTATION_CLASS;
	private final Map<String, DataStore> originalDataStores = UtilImpl.DATA_STORES;

	@BeforeEach
	void configureFakeContentManager() {
		AbstractContentManager.IMPLEMENTATION_CLASS = FakeContentManager.class;
		FakeContentManager.reset();
	}

	@AfterEach
	void restoreServerArgs() {
		UtilImpl.CONTENT_JDBC_SERVER_ARGS = originalServerArgs;
		UtilImpl.DATA_STORES = originalDataStores;
		AbstractContentManager.IMPLEMENTATION_CLASS = originalImplementationClass;
		FakeContentManager.reset();
		JDBCRemoteContentManagerServer.shutdown();
	}

	@Test
	void testStartupRegistersAliasesAndFunctionsCanBeCalledViaJdbc() throws Exception {
		String dbUrl = "jdbc:h2:mem:content_server_startup;DB_CLOSE_DELAY=-1";
		Map<String, DataStore> dataStores = new TreeMap<>();
		dataStores.put("CONTENT", new DataStore("org.h2.Driver", dbUrl, "org.skyve.impl.persistence.hibernate.dialect.H2SpatialDialect"));
		UtilImpl.DATA_STORES = dataStores;
		int port = 19000 + (int) (System.nanoTime() % 1000L);
		UtilImpl.CONTENT_JDBC_SERVER_ARGS = "-tcpPort " + port + " -tcpDaemon";

		JDBCRemoteContentManagerServer.startup();

		BeanContent beanContent = new BeanContent(samplePersistentBean("biz-startup-1"));
		String encodedBean = StateUtil.encode64(beanContent);

		try (Connection c = DriverManager.getConnection(dbUrl)) {
			try (CallableStatement putBean = c.prepareCall("CALL PUT_BEAN(?)")) {
				putBean.setString(1, encodedBean);
				putBean.execute();
			}

			try (CallableStatement putAttachment = c.prepareCall("? = CALL PUT_ATTACHMENT(?,?)")) {
				putAttachment.registerOutParameter(1, java.sql.Types.VARCHAR);
				putAttachment.setString(2, StateUtil.encode64(sampleAttachment()));
				putAttachment.setBoolean(3, true);
				putAttachment.execute();
				assertNotNull(putAttachment.getString(1));
			}
		}

		assertNotNull(FakeContentManager.lastPutBean);
		assertNotNull(FakeContentManager.lastPutAttachment);
	}

	@Test
	void testStartupThrowsWhenServerArgsMissing() {
		UtilImpl.CONTENT_JDBC_SERVER_ARGS = null;

		IllegalStateException ex = assertThrows(IllegalStateException.class, JDBCRemoteContentManagerServer::startup);
		assertTrue(ex.getMessage().contains("there are no server arguments defined"));
	}

	@Test
	void testStartupWrapsSqlExceptions() {
		UtilImpl.CONTENT_JDBC_SERVER_ARGS = "-tcpPort not-a-number";

		IllegalStateException ex = assertThrows(IllegalStateException.class, JDBCRemoteContentManagerServer::startup);
		assertTrue(ex.getMessage().contains("Could not startup JDBCRemoteContentManagerServer"));
	}

	@Test
	void testShutdownDoesNothingWhenServerNotStarted() {
		assertDoesNotThrow(JDBCRemoteContentManagerServer::shutdown);
	}

	@Test
	void testPutBeanFunctionDelegatesToContentManager() throws Exception {
		BeanContent beanContent = new BeanContent(samplePersistentBean("biz-123"));

		JDBCRemoteContentManagerServer.putBeanFunction(StateUtil.encode64(beanContent));

		assertNotNull(FakeContentManager.lastPutBean);
		assertEquals("biz-123", FakeContentManager.lastPutBean.getBizId());
	}

	@Test
	void testPutAttachmentFunctionReturnsContentId() throws Exception {
		AttachmentContent attachment = sampleAttachment();
		String payload = StateUtil.encode64(attachment);

		String contentId = JDBCRemoteContentManagerServer.putAttachmentFunction(payload, true);

		assertNotNull(contentId);
		assertEquals(contentId, FakeContentManager.lastPutAttachment.getContentId());
		assertTrue(FakeContentManager.lastPutAttachmentIndex);
	}

	@Test
	void testUpdateAttachmentFunctionDelegatesToContentManager() throws Exception {
		AttachmentContent attachment = sampleAttachment();
		attachment.setContentId("existing-content-id");

		JDBCRemoteContentManagerServer.updateAttachmentFunction(StateUtil.encode64(attachment));

		assertNotNull(FakeContentManager.lastUpdatedAttachment);
		assertEquals("existing-content-id", FakeContentManager.lastUpdatedAttachment.getContentId());
	}

	@Test
	void testGetAttachmentFunctionReturnsEncodedPayload() throws Exception {
		AttachmentContent expected = sampleAttachment();
		expected.setContentId("cid-1");
		FakeContentManager.attachmentToReturn = expected;

		String payload = JDBCRemoteContentManagerServer.getAttachmentFunction("cid-1");

		assertNotNull(payload);
		AttachmentContent decoded = StateUtil.decode64(payload);
		assertEquals("cid-1", decoded.getContentId());
		assertEquals("cid-1", FakeContentManager.lastGetAttachmentId);
	}

	@Test
	void testGetAttachmentFunctionReturnsNullWhenAbsent() throws Exception {
		FakeContentManager.attachmentToReturn = null;

		String payload = JDBCRemoteContentManagerServer.getAttachmentFunction("missing");

		assertNull(payload);
		assertEquals("missing", FakeContentManager.lastGetAttachmentId);
	}

	@Test
	void testRemoveFunctionsDelegate() throws Exception {
		JDBCRemoteContentManagerServer.removeBeanFunction("biz-123");
		JDBCRemoteContentManagerServer.removeAttachmentFunction("cid-123");

		assertEquals("biz-123", FakeContentManager.lastRemovedBeanId);
		assertEquals("cid-123", FakeContentManager.lastRemovedAttachmentId);
	}

	@Test
	void testGoogleSearchFunctionReturnsEncodedResults() throws Exception {
		SearchResults expected = new SearchResults();
		SearchResult row = new SearchResult();
		row.setBizId("biz-1");
		expected.getResults().add(row);
		FakeContentManager.searchResultsToReturn = expected;

		String payload = JDBCRemoteContentManagerServer.googleSearchFunction("term", 7);

		assertNotNull(payload);
		SearchResults decoded = StateUtil.decode64(payload);
		assertEquals(1, decoded.getResults().size());
		assertEquals("biz-1", decoded.getResults().get(0).getBizId());
		assertEquals("term", FakeContentManager.lastSearchTerm);
		assertEquals(7, FakeContentManager.lastSearchMaxResults);
	}

	private static AttachmentContent sampleAttachment() {
		return new AttachmentContent("demo",
									"admin",
									"Contact",
									null,
									"",
									UUIDv7.create().toString(),
									"image").attachment("sample.txt", "text/plain", new byte[] {1, 2, 3});
	}

	private static PersistentBean samplePersistentBean(String bizId) {
		InvocationHandler handler = (proxy, method, args) -> {
			String name = method.getName();
			return switch (name) {
				case "getBizCustomer" -> "demo";
				case "getBizModule" -> "admin";
				case "getBizDocument" -> "Contact";
				case "getBizDataGroupId" -> null;
				case "getBizUserId" -> "";
				case "getBizId" -> bizId;
				case "isPersisted" -> Boolean.TRUE;
				case "equals" -> Boolean.valueOf(proxy == args[0]);
				case "hashCode" -> Integer.valueOf(System.identityHashCode(proxy));
				case "toString" -> "PersistentBeanProxy(" + bizId + ")";
				default -> null;
			};
		};

		return (PersistentBean) Proxy.newProxyInstance(PersistentBean.class.getClassLoader(), new Class<?>[] {PersistentBean.class}, handler);
	}

	public static class FakeContentManager extends AbstractContentManager {
		private static BeanContent lastPutBean;
		private static AttachmentContent lastPutAttachment;
		private static boolean lastPutAttachmentIndex;
		private static AttachmentContent lastUpdatedAttachment;
		private static String lastGetAttachmentId;
		private static String lastRemovedBeanId;
		private static String lastRemovedAttachmentId;
		private static String lastSearchTerm;
		private static int lastSearchMaxResults;

		private static AttachmentContent attachmentToReturn;
		private static SearchResults searchResultsToReturn;

		private static void reset() {
			lastPutBean = null;
			lastPutAttachment = null;
			lastPutAttachmentIndex = false;
			lastUpdatedAttachment = null;
			lastGetAttachmentId = null;
			lastRemovedBeanId = null;
			lastRemovedAttachmentId = null;
			lastSearchTerm = null;
			lastSearchMaxResults = 0;
			attachmentToReturn = null;
			searchResultsToReturn = new SearchResults();
		}

		@Override
		public void put(BeanContent content) {
			lastPutBean = content;
		}

		@Override
		public void put(AttachmentContent content, boolean index) {
			lastPutAttachment = content;
			lastPutAttachmentIndex = index;
			if (content.getContentId() == null) {
				content.setContentId("generated-content-id");
			}
		}

		@Override
		public void update(AttachmentContent content) {
			lastUpdatedAttachment = content;
		}

		@Override
		public AttachmentContent getAttachment(String contentId) {
			lastGetAttachmentId = contentId;
			return attachmentToReturn;
		}

		@Override
		public void removeBean(String bizId) {
			lastRemovedBeanId = bizId;
		}

		@Override
		public void removeAttachment(String contentId) {
			lastRemovedAttachmentId = contentId;
		}

		@Override
		public SearchResults google(String search, int maxResults) {
			lastSearchTerm = search;
			lastSearchMaxResults = maxResults;
			return searchResultsToReturn;
		}

		@Override
		public void dropIndexing() {
			// not required in these tests
		}

		@Override
		public void truncateIndexing(String customerName) {
			// not required in these tests
		}

		@Override
		public void truncateAttachmentIndexing(String customerName) {
			// not required in these tests
		}

		@Override
		public void truncateBeanIndexing(String customerName) {
			// not required in these tests
		}

		@Override
		public ContentIterable all() {
			return () -> new ContentIterable.ContentIterator() {
				@Override
				public boolean hasNext() {
					return false;
				}

				@Override
				public SearchResult next() {
					return null;
				}

				@Override
				public long getTotalHits() {
					return 0;
				}
			};
		}

		@Override
		public void startup() {
			// no-op
		}

		@Override
		public void shutdown() {
			// no-op
		}

		@Override
		public void close() {
			// no-op
		}

		@Override
		public void reindex(AttachmentContent attachment, boolean index) {
			// not required in these tests
		}
	}
}
