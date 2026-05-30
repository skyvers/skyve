package org.skyve.impl.content.jdbc;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Proxy;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.Statement;
import java.util.Map;
import java.util.TreeMap;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.content.AttachmentContent;
import org.skyve.content.BeanContent;
import org.skyve.content.SearchResults;
import org.skyve.domain.PersistentBean;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.UUIDv7;
import org.skyve.persistence.DataStore;

@SuppressWarnings("static-method")
class JDBCRemoteContentManagerClientTest {
	private static final String TEST_DB_URL = "jdbc:h2:mem:content_client_test;DB_CLOSE_DELAY=-1";

	private Map<String, DataStore> originalDataStores;

	@BeforeEach
	void configureDataStoreAndAliases() throws Exception {
		originalDataStores = UtilImpl.DATA_STORES;
		UtilImpl.DATA_STORES = new TreeMap<>();
		UtilImpl.DATA_STORES.put(JDBCRemoteContentManagerServer.CONTENT_DATA_STORE_NAME,
				new DataStore("org.h2.Driver",
							TEST_DB_URL,
							"org.skyve.impl.persistence.hibernate.dialect.H2SpatialDialect"));

		JDBCRemoteContentManagerClientAliasHarness.reset();
		try (Connection c = DriverManager.getConnection(TEST_DB_URL);
				Statement s = c.createStatement()) {
			s.execute("DROP ALIAS IF EXISTS " + JDBCRemoteContentManagerServer.PUT_BEAN_FUNCTION_NAME);
			s.execute("DROP ALIAS IF EXISTS " + JDBCRemoteContentManagerServer.PUT_ATTACHMENT_FUNCTION_NAME);
			s.execute("DROP ALIAS IF EXISTS " + JDBCRemoteContentManagerServer.UPDATE_ATTACHMENT_FUNCTION_NAME);
			s.execute("DROP ALIAS IF EXISTS " + JDBCRemoteContentManagerServer.GET_ATTACHMENT_FUNCTION_NAME);
			s.execute("DROP ALIAS IF EXISTS " + JDBCRemoteContentManagerServer.REMOVE_BEAN_FUNCTION_NAME);
			s.execute("DROP ALIAS IF EXISTS " + JDBCRemoteContentManagerServer.REMOVE_ATTACHMENT_FUNCTION_NAME);
			s.execute("DROP ALIAS IF EXISTS " + JDBCRemoteContentManagerServer.GOOGLE_SEARCH_FUNCTION_NAME);

			s.execute("CREATE ALIAS " + JDBCRemoteContentManagerServer.PUT_BEAN_FUNCTION_NAME
					+ " FOR \"org.skyve.impl.content.jdbc.JDBCRemoteContentManagerClientAliasHarness.putBeanAlias\"");
			s.execute("CREATE ALIAS " + JDBCRemoteContentManagerServer.PUT_ATTACHMENT_FUNCTION_NAME
					+ " FOR \"org.skyve.impl.content.jdbc.JDBCRemoteContentManagerClientAliasHarness.putAttachmentAlias\"");
			s.execute("CREATE ALIAS " + JDBCRemoteContentManagerServer.UPDATE_ATTACHMENT_FUNCTION_NAME
					+ " FOR \"org.skyve.impl.content.jdbc.JDBCRemoteContentManagerClientAliasHarness.updateAttachmentAlias\"");
			s.execute("CREATE ALIAS " + JDBCRemoteContentManagerServer.GET_ATTACHMENT_FUNCTION_NAME
					+ " FOR \"org.skyve.impl.content.jdbc.JDBCRemoteContentManagerClientAliasHarness.getAttachmentAlias\"");
			s.execute("CREATE ALIAS " + JDBCRemoteContentManagerServer.REMOVE_BEAN_FUNCTION_NAME
					+ " FOR \"org.skyve.impl.content.jdbc.JDBCRemoteContentManagerClientAliasHarness.removeBeanAlias\"");
			s.execute("CREATE ALIAS " + JDBCRemoteContentManagerServer.REMOVE_ATTACHMENT_FUNCTION_NAME
					+ " FOR \"org.skyve.impl.content.jdbc.JDBCRemoteContentManagerClientAliasHarness.removeAttachmentAlias\"");
			s.execute("CREATE ALIAS " + JDBCRemoteContentManagerServer.GOOGLE_SEARCH_FUNCTION_NAME
					+ " FOR \"org.skyve.impl.content.jdbc.JDBCRemoteContentManagerClientAliasHarness.googleSearchAlias\"");
		}
	}

	@AfterEach
	void restoreDataStores() {
		UtilImpl.DATA_STORES = originalDataStores;
		JDBCRemoteContentManagerClientAliasHarness.reset();
	}

	@Test
	void testLifecycleMethodsAreNoOp() throws Exception {
		try (JDBCRemoteContentManagerClient client = new JDBCRemoteContentManagerClient()) {
			client.startup();
			client.shutdown();
		}
	}

	@Test
	void testPutBeanDelegatesToAlias() throws Exception {
		BeanContent bean = new BeanContent(samplePersistentBean("biz-101"));

		try (JDBCRemoteContentManagerClient client = new JDBCRemoteContentManagerClient()) {
			client.put(bean);
		}

		assertNotNull(JDBCRemoteContentManagerClientAliasHarness.lastPutBean);
		assertEquals("biz-101", JDBCRemoteContentManagerClientAliasHarness.lastPutBean.getBizId());
	}

	@Test
	void testPutAttachmentSetsReturnedContentId() throws Exception {
		AttachmentContent attachment = sampleAttachment();

		try (JDBCRemoteContentManagerClient client = new JDBCRemoteContentManagerClient()) {
			client.put(attachment, true);
		}

		assertEquals(JDBCRemoteContentManagerClientAliasHarness.TEST_CONTENT_ID, attachment.getContentId());
		assertNotNull(JDBCRemoteContentManagerClientAliasHarness.lastPutAttachment);
		assertTrue(JDBCRemoteContentManagerClientAliasHarness.lastPutAttachmentIndex);
	}

	@Test
	void testUpdateSendsRemoteClonePayload() throws Exception {
		AttachmentContent attachment = sampleAttachment();
		attachment.setContentId("existing-1");

		try (JDBCRemoteContentManagerClient client = new JDBCRemoteContentManagerClient()) {
			client.update(attachment);
		}

		assertNotNull(JDBCRemoteContentManagerClientAliasHarness.lastUpdatedAttachment);
		assertEquals("existing-1", JDBCRemoteContentManagerClientAliasHarness.lastUpdatedAttachment.getContentId());
		assertEquals(0, JDBCRemoteContentManagerClientAliasHarness.lastUpdatedAttachment.getContentBytes().length);
	}

	@Test
	void testGetAttachmentDecodesAliasPayload() throws Exception {
		AttachmentContent expected = sampleAttachment();
		expected.setContentId("cid-xyz");
		JDBCRemoteContentManagerClientAliasHarness.lastPutAttachment = expected;

		try (JDBCRemoteContentManagerClient client = new JDBCRemoteContentManagerClient()) {
			AttachmentContent result = client.getAttachment("cid-xyz");
			assertNotNull(result);
			assertEquals("cid-xyz", result.getContentId());
		}

		assertEquals("cid-xyz", JDBCRemoteContentManagerClientAliasHarness.lastRequestedAttachmentId);
	}

	@Test
	void testRemoveAndGoogleDelegateToAliases() throws Exception {
		try (JDBCRemoteContentManagerClient client = new JDBCRemoteContentManagerClient()) {
			client.removeBean("biz-remove");
			client.removeAttachment("cid-remove");
			SearchResults results = client.google("alpha", 5);
			assertNotNull(results);
			assertEquals(0, results.getResults().size());
		}

		assertEquals("biz-remove", JDBCRemoteContentManagerClientAliasHarness.lastRemovedBeanId);
		assertEquals("cid-remove", JDBCRemoteContentManagerClientAliasHarness.lastRemovedAttachmentId);
		assertEquals("alpha", JDBCRemoteContentManagerClientAliasHarness.lastSearch);
		assertEquals(5, JDBCRemoteContentManagerClientAliasHarness.lastMaxResults);
	}

	@Test
	void testUnsupportedOperationsThrowHelpfulMessages() throws Exception {
		try (JDBCRemoteContentManagerClient client = new JDBCRemoteContentManagerClient()) {
			AttachmentContent attachment = sampleAttachment();

			UnsupportedOperationException reindex = assertThrows(UnsupportedOperationException.class,
					() -> client.reindex(attachment, true));
			UnsupportedOperationException drop = assertThrows(UnsupportedOperationException.class, client::dropIndexing);
			UnsupportedOperationException truncate = assertThrows(UnsupportedOperationException.class,
					() -> client.truncateIndexing("cust"));
			UnsupportedOperationException truncateAttachment = assertThrows(UnsupportedOperationException.class,
					() -> client.truncateAttachmentIndexing("cust"));
			UnsupportedOperationException truncateBean = assertThrows(UnsupportedOperationException.class,
					() -> client.truncateBeanIndexing("cust"));
			UnsupportedOperationException all = assertThrows(UnsupportedOperationException.class, client::all);

			assertTrue(reindex.getMessage().contains("not supported"));
			assertTrue(drop.getMessage().contains("not supported"));
			assertTrue(truncate.getMessage().contains("not supported"));
			assertTrue(truncateAttachment.getMessage().contains("not supported"));
			assertTrue(truncateBean.getMessage().contains("not supported"));
			assertTrue(all.getMessage().contains("not supported"));
		}
	}

	private static AttachmentContent sampleAttachment() {
		return new AttachmentContent("demo",
									"admin",
									"Contact",
									null,
									"",
									UUIDv7.create().toString(),
									"image").attachment("file.txt", new byte[] {1});
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

}