package org.skyve.impl.content.ejb;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.content.AttachmentContent;
import org.skyve.content.BeanContent;
import org.skyve.content.MimeType;
import org.skyve.content.SearchResults;
import org.skyve.domain.PersistentBean;
import org.skyve.impl.content.AbstractContentManager;
import org.skyve.impl.content.NoOpContentManager;

@SuppressWarnings("static-method")
class AbstractEJBRemoteContentManagerServerBeanTest {
	private Class<? extends AbstractContentManager> originalContentManagerClass;

	@BeforeEach
	void setUp() {
		originalContentManagerClass = AbstractContentManager.IMPLEMENTATION_CLASS;
		AbstractContentManager.IMPLEMENTATION_CLASS = CapturingContentManager.class;
		CapturingContentManager.reset();
	}

	@AfterEach
	void tearDown() {
		AbstractContentManager.IMPLEMENTATION_CLASS = originalContentManagerClass;
		CapturingContentManager.reset();
	}

	@Test
	void delegatesContentOperationsToConfiguredContentManager() throws Exception {
		TestServerBean server = new TestServerBean();
		BeanContent beanContent = beanContent();
		AttachmentContent attachmentContent = attachmentContent();
		SearchResults searchResults = new SearchResults();
		CapturingContentManager.attachmentToReturn = attachmentContent;
		CapturingContentManager.searchResultsToReturn = searchResults;

		server.put(beanContent);
		String contentId = server.put(attachmentContent, true);
		server.update(attachmentContent);
		AttachmentContent found = server.getAttachment("content-1");
		server.removeBean("bean-1");
		server.removeAttachment("content-1");
		SearchResults results = server.google("needle", 7);

		assertSame(beanContent, CapturingContentManager.beanPut);
		assertSame(attachmentContent, CapturingContentManager.attachmentPut);
		assertTrue(CapturingContentManager.attachmentPutIndex);
		assertEquals("content-1", contentId);
		assertSame(attachmentContent, CapturingContentManager.attachmentUpdated);
		assertEquals("content-1", CapturingContentManager.contentIdRequested);
		assertSame(attachmentContent, found);
		assertEquals("bean-1", CapturingContentManager.beanRemoved);
		assertEquals("content-1", CapturingContentManager.attachmentRemoved);
		assertEquals("needle", CapturingContentManager.searchText);
		assertEquals(7, CapturingContentManager.maxResults);
		assertSame(searchResults, results);
		assertEquals(7, CapturingContentManager.closeCount);
	}

	@Test
	void propagatesContentManagerExceptions() {
		CapturingContentManager.failure = new IllegalStateException("content failed");
		TestServerBean server = new TestServerBean();

		IllegalStateException thrown = assertThrows(IllegalStateException.class,
				() -> server.removeAttachment("content-1"));

		assertSame(CapturingContentManager.failure, thrown);
		assertEquals(1, CapturingContentManager.closeCount);
	}

	private static BeanContent beanContent() {
		PersistentBean bean = mock(PersistentBean.class);
		when(bean.getBizCustomer()).thenReturn("customer");
		when(bean.getBizModule()).thenReturn("admin");
		when(bean.getBizDocument()).thenReturn("User");
		when(bean.getBizDataGroupId()).thenReturn("group");
		when(bean.getBizUserId()).thenReturn("user");
		when(bean.getBizId()).thenReturn("bean-1");
		return new BeanContent(bean);
	}

	private static AttachmentContent attachmentContent() {
		AttachmentContent content = new AttachmentContent("customer", "admin", "User", "group", "user", "bean-1", "photo");
		content.setContentId("content-1");
		content.attachment("photo.txt", MimeType.plain, "hello".getBytes(java.nio.charset.StandardCharsets.UTF_8));
		return content;
	}

	private static final class TestServerBean extends AbstractEJBRemoteContentManagerServerBean {
		// Concrete type for testing the abstract delegation implementation.
	}

	public static class CapturingContentManager extends NoOpContentManager {
		private static BeanContent beanPut;
		private static AttachmentContent attachmentPut;
		private static boolean attachmentPutIndex;
		private static AttachmentContent attachmentUpdated;
		private static String contentIdRequested;
		private static AttachmentContent attachmentToReturn;
		private static String beanRemoved;
		private static String attachmentRemoved;
		private static String searchText;
		private static int maxResults;
		private static SearchResults searchResultsToReturn;
		private static RuntimeException failure;
		private static int closeCount;

		static void reset() {
			beanPut = null;
			attachmentPut = null;
			attachmentPutIndex = false;
			attachmentUpdated = null;
			contentIdRequested = null;
			attachmentToReturn = null;
			beanRemoved = null;
			attachmentRemoved = null;
			searchText = null;
			maxResults = 0;
			searchResultsToReturn = null;
			failure = null;
			closeCount = 0;
		}

		@Override
		public void put(BeanContent content) {
			throwIfConfigured();
			beanPut = content;
		}

		@Override
		public void put(AttachmentContent content, boolean index) {
			throwIfConfigured();
			attachmentPut = content;
			attachmentPutIndex = index;
		}

		@Override
		public void update(AttachmentContent content) {
			throwIfConfigured();
			attachmentUpdated = content;
		}

		@Override
		public AttachmentContent getAttachment(String contentId) {
			throwIfConfigured();
			contentIdRequested = contentId;
			return attachmentToReturn;
		}

		@Override
		public void removeBean(String bizId) {
			throwIfConfigured();
			beanRemoved = bizId;
		}

		@Override
		public void removeAttachment(String contentId) {
			throwIfConfigured();
			attachmentRemoved = contentId;
		}

		@Override
		public SearchResults google(String search, int numResults) {
			throwIfConfigured();
			CapturingContentManager.searchText = search;
			CapturingContentManager.maxResults = numResults;
			return searchResultsToReturn;
		}

		@Override
		public void close() {
			closeCount++;
		}

		private static void throwIfConfigured() {
			if (failure != null) {
				throw failure;
			}
		}
	}
}
