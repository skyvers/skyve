package org.skyve.impl.content.ejb;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.junit.jupiter.api.Test;
import org.skyve.content.AttachmentContent;
import org.skyve.content.BeanContent;
import org.skyve.content.MimeType;
import org.skyve.content.SearchResults;
import org.skyve.domain.PersistentBean;

@SuppressWarnings("static-method")
class AbstractEJBRemoteContentManagerClientTest {
	@Test
	void lifecycleMethodsAreNoOps() throws Exception {
		EJBRemoteContentManagerServer server = mock(EJBRemoteContentManagerServer.class);
		try (TestClient client = new TestClient(server)) {
			client.startup();
			client.close();
			client.shutdown();
		}
		verifyNoInteractions(server);
	}

	@Test
	void methodsDelegateToObtainedServer() throws Exception {
		EJBRemoteContentManagerServer server = mock(EJBRemoteContentManagerServer.class);
		try (TestClient client = new TestClient(server)) {
			BeanContent beanContent = new BeanContent(bean("B1"));
			AttachmentContent attachment = attachment("B1");
			AttachmentContent returned = attachment("B2");
			SearchResults results = new SearchResults();
			when(server.put(attachment, true)).thenReturn("content-1");
			when(server.getAttachment("content-1")).thenReturn(returned);
			when(server.google("query", 10)).thenReturn(results);

			client.put(beanContent);
			client.put(attachment, true);
			client.update(attachment);
			AttachmentContent actualAttachment = client.getAttachment("content-1");
			client.removeBean("B1");
			client.removeAttachment("content-1");
			SearchResults actualResults = client.google("query", 10);

			verify(server).put(beanContent);
			verify(server).put(attachment, true);
			verify(server).update(any(AttachmentContent.class));
			verify(server).removeBean("B1");
			verify(server).removeAttachment("content-1");
			assertEquals("content-1", attachment.getContentId());
			assertSame(returned, actualAttachment);
			assertSame(results, actualResults);
		}
	}

	@Test
	void unsupportedRepositoryOperationsThrow() throws Exception {
		try (TestClient client = new TestClient(mock(EJBRemoteContentManagerServer.class))) {
			assertThrows(UnsupportedOperationException.class, client::dropIndexing);
			assertThrows(UnsupportedOperationException.class, () -> client.truncateIndexing("customer"));
			assertThrows(UnsupportedOperationException.class, () -> client.truncateAttachmentIndexing("customer"));
			assertThrows(UnsupportedOperationException.class, () -> client.truncateBeanIndexing("customer"));
			assertThrows(UnsupportedOperationException.class, client::all);
			assertThrows(UnsupportedOperationException.class, () -> client.reindex(null, true));
		}
	}

	private static PersistentBean bean(String bizId) {
		PersistentBean bean = mock(PersistentBean.class);
		when(bean.getBizCustomer()).thenReturn("demo");
		when(bean.getBizModule()).thenReturn("admin");
		when(bean.getBizDocument()).thenReturn("User");
		when(bean.getBizDataGroupId()).thenReturn("group1");
		when(bean.getBizUserId()).thenReturn("user1");
		when(bean.getBizId()).thenReturn(bizId);
		return bean;
	}

	private static AttachmentContent attachment(String bizId) {
		return new AttachmentContent("demo", "admin", "User", "group1", "user1", bizId, "avatar")
				.attachment("avatar.txt", MimeType.plain, "hello".getBytes());
	}

	private static final class TestClient extends AbstractEJBRemoteContentManagerClient {
		private final EJBRemoteContentManagerServer server;

		private TestClient(EJBRemoteContentManagerServer server) {
			this.server = server;
		}

		@Override
		public EJBRemoteContentManagerServer obtainServer() {
			return server;
		}
	}
}
