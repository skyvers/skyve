package org.skyve.impl.content;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.lang.reflect.Field;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.skyve.content.AttachmentContent;
import org.skyve.content.BeanContent;
import org.skyve.content.ContentIterable;
import org.skyve.content.SearchResult;
import org.skyve.content.SearchResults;
import org.skyve.impl.content.lucene.LuceneContentManager;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.UUIDv7;

@SuppressWarnings("static-method")
class DelegatingContentManagerTest {
	private final Class<? extends AbstractContentManager> originalImplementationClass = AbstractContentManager.IMPLEMENTATION_CLASS;
	private final String originalManagerClass = UtilImpl.SKYVE_CONTENT_MANAGER_CLASS;

	@AfterEach
	void restoreStaticState() {
		AbstractContentManager.IMPLEMENTATION_CLASS = originalImplementationClass;
		UtilImpl.SKYVE_CONTENT_MANAGER_CLASS = originalManagerClass;
	}

	@Test
	void testForwardingToInjectedDelegate() throws Exception {
		try (DelegatingContentManager manager = new DelegatingContentManager();
				TestContentManagerDelegate delegate = new TestContentManagerDelegate()) {
			AttachmentContent attachment = new AttachmentContent("demo",
												"admin",
												"Contact",
												null,
												"",
												UUIDv7.create().toString(),
												"image").attachment("file.txt", new byte[] {1, 2, 3});
			SearchResults expectedResults = new SearchResults();
			ContentIterable expectedIterable = () -> new ContentIterable.ContentIterator() {
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

			delegate.attachmentToReturn = attachment;
			delegate.resultsToReturn = expectedResults;
			delegate.iterableToReturn = expectedIterable;
			setDelegate(manager, delegate);

			manager.put((BeanContent) null);
			manager.put(attachment, true);
			manager.update(attachment);
			assertSame(attachment, manager.getAttachment("contentId"));
			manager.removeBean("bizId");
			manager.removeAttachment("contentId");
			assertSame(expectedResults, manager.google("search", 42));
			manager.dropIndexing();
			manager.truncateIndexing("cust");
			manager.truncateAttachmentIndexing("cust");
			manager.truncateBeanIndexing("cust");
			assertSame(expectedIterable, manager.all());
			manager.reindex(attachment, false);
			manager.startup();
			manager.shutdown();

			assertTrue(delegate.putBeanCalled);
			assertTrue(delegate.putAttachmentCalled);
			assertTrue(delegate.updateCalled);
			assertEquals("contentId", delegate.getAttachmentArg);
			assertEquals("bizId", delegate.removeBeanArg);
			assertEquals("contentId", delegate.removeAttachmentArg);
			assertEquals("search", delegate.searchArg);
			assertEquals(42, delegate.maxResultsArg);
			assertTrue(delegate.dropIndexingCalled);
			assertEquals("cust", delegate.truncateIndexingArg);
			assertEquals("cust", delegate.truncateAttachmentIndexingArg);
			assertEquals("cust", delegate.truncateBeanIndexingArg);
			assertSame(attachment, delegate.reindexAttachmentArg);
			assertEquals(Boolean.FALSE, delegate.reindexIndexArg);
			assertTrue(delegate.startupCalled);
			assertTrue(delegate.shutdownCalled);
		}
	}

	@Test
	void testDelegateDefaultsToLuceneWhenImplementationClassNotConfigured() throws Exception {
		AbstractContentManager.IMPLEMENTATION_CLASS = null;
		UtilImpl.SKYVE_CONTENT_MANAGER_CLASS = null;

		try (DelegatingContentManager manager = new DelegatingContentManager()) {
			manager.shutdown();

			assertEquals(LuceneContentManager.class, AbstractContentManager.IMPLEMENTATION_CLASS);
		}
	}

	@Test
	@SuppressWarnings("resource")
	void testStartupThrowsWhenConfiguredImplementationIsMissing() {
		AbstractContentManager.IMPLEMENTATION_CLASS = null;
		UtilImpl.SKYVE_CONTENT_MANAGER_CLASS = "org.skyve.impl.content.DoesNotExist";

		DelegatingContentManager manager = new DelegatingContentManager();
		IllegalStateException ex = assertThrows(IllegalStateException.class, manager::startup);

		assertTrue(ex.getMessage().contains("Could not find factories.contentManagerClass"));
	}

	@Test
	void testStartupLoadsConfiguredImplementationClass() throws Exception {
		AbstractContentManager.IMPLEMENTATION_CLASS = null;
		UtilImpl.SKYVE_CONTENT_MANAGER_CLASS = ConfiguredContentManager.class.getName();

		try (DelegatingContentManager manager = new DelegatingContentManager()) {
			ConfiguredContentManager.startupCount = 0;
			manager.startup();
			assertEquals(ConfiguredContentManager.class, AbstractContentManager.IMPLEMENTATION_CLASS);
			assertEquals(1, ConfiguredContentManager.startupCount);
		}
	}

	private static void setDelegate(DelegatingContentManager manager, AbstractContentManager delegate) throws Exception {
		Field delegateField = DelegatingContentManager.class.getDeclaredField("delegate");
		delegateField.setAccessible(true);
		delegateField.set(manager, delegate);
	}

	private static class TestContentManagerDelegate extends AbstractContentManager {
		private boolean putBeanCalled;
		private boolean putAttachmentCalled;
		private boolean updateCalled;
		private String getAttachmentArg;
		private String removeBeanArg;
		private String removeAttachmentArg;
		private String searchArg;
		private int maxResultsArg;
		private boolean dropIndexingCalled;
		private String truncateIndexingArg;
		private String truncateAttachmentIndexingArg;
		private String truncateBeanIndexingArg;
		private AttachmentContent reindexAttachmentArg;
		private Boolean reindexIndexArg;
		private boolean startupCalled;
		private boolean shutdownCalled;

		private AttachmentContent attachmentToReturn;
		private SearchResults resultsToReturn;
		private ContentIterable iterableToReturn;

		@Override
		public void startup() {
			startupCalled = true;
		}

		@Override
		public void shutdown() {
			shutdownCalled = true;
		}

		@Override
		public void close() {
			// no-op
		}

		@Override
		public void put(BeanContent content) {
			putBeanCalled = true;
		}

		@Override
		public void put(AttachmentContent content, boolean index) {
			putAttachmentCalled = true;
		}

		@Override
		public void update(AttachmentContent content) {
			updateCalled = true;
		}

		@Override
		public AttachmentContent getAttachment(String contentId) {
			getAttachmentArg = contentId;
			return attachmentToReturn;
		}

		@Override
		public void removeBean(String bizId) {
			removeBeanArg = bizId;
		}

		@Override
		public void removeAttachment(String contentId) {
			removeAttachmentArg = contentId;
		}

		@Override
		public SearchResults google(String search, int maxResults) {
			searchArg = search;
			maxResultsArg = maxResults;
			return resultsToReturn;
		}

		@Override
		public void dropIndexing() {
			dropIndexingCalled = true;
		}

		@Override
		public void truncateIndexing(String customerName) {
			truncateIndexingArg = customerName;
		}

		@Override
		public void truncateAttachmentIndexing(String customerName) {
			truncateAttachmentIndexingArg = customerName;
		}

		@Override
		public void truncateBeanIndexing(String customerName) {
			truncateBeanIndexingArg = customerName;
		}

		@Override
		public ContentIterable all() {
			return iterableToReturn;
		}

		@Override
		public void reindex(AttachmentContent attachment, boolean index) {
			reindexAttachmentArg = attachment;
			reindexIndexArg = Boolean.valueOf(index);
		}
	}

	public static class ConfiguredContentManager extends AbstractContentManager {
		private static int startupCount;

		@Override
		public void startup() {
			startupCount++;
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
		public void put(BeanContent content) {
			// no-op
		}

		@Override
		public void put(AttachmentContent content, boolean index) {
			// no-op
		}

		@Override
		public void update(AttachmentContent content) {
			// no-op
		}

		@Override
		public AttachmentContent getAttachment(String contentId) {
			return null;
		}

		@Override
		public void removeBean(String bizId) {
			// no-op
		}

		@Override
		public void removeAttachment(String contentId) {
			// no-op
		}

		@Override
		public SearchResults google(String search, int maxResults) {
			return new SearchResults();
		}

		@Override
		public void dropIndexing() {
			// no-op
		}

		@Override
		public void truncateIndexing(String customerName) {
			// no-op
		}

		@Override
		public void truncateAttachmentIndexing(String customerName) {
			// no-op
		}

		@Override
		public void truncateBeanIndexing(String customerName) {
			// no-op
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
		public void reindex(AttachmentContent attachment, boolean index) {
			// no-op
		}
	}
}