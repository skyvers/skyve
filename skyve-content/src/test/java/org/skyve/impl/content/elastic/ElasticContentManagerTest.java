package org.skyve.impl.content.elastic;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Proxy;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Comparator;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.skyve.content.AttachmentContent;
import org.skyve.content.BeanContent;
import org.skyve.content.ContentIterable.ContentIterator;
import org.skyve.domain.PersistentBean;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.UUIDv7;

@SuppressWarnings("static-method")
class ElasticContentManagerTest {
	private final String originalContentDirectory = UtilImpl.CONTENT_DIRECTORY;
	private final boolean originalContentFileStorage = UtilImpl.CONTENT_FILE_STORAGE;
	private Path tempContentDirectory;

	@AfterEach
	void restoreContentSettings() throws Exception {
		UtilImpl.CONTENT_DIRECTORY = originalContentDirectory;
		UtilImpl.CONTENT_FILE_STORAGE = originalContentFileStorage;
		if (tempContentDirectory != null) {
			try (var stream = Files.walk(tempContentDirectory)) {
				stream.sorted(Comparator.reverseOrder()).forEach(path -> {
					try {
						Files.deleteIfExists(path);
					}
					catch (Exception e) {
						throw new IllegalStateException(e);
					}
				});
			}
		}
	}

	@Test
	void testStubLifecycleAndOperations() throws Exception {
		try (ElasticContentManager manager = new ElasticContentManager()) {
			manager.startup();
			manager.put(new BeanContent(samplePersistentBean("bean-1")));
			AttachmentContent attachment = sampleAttachment();
			manager.put(attachment, true);
			manager.update(attachment);
			manager.reindex(attachment, false);
			manager.removeBean("bean-1");
			manager.removeAttachment("cid-1");
			manager.truncateIndexing("demo");
			manager.truncateAttachmentIndexing("demo");
			manager.truncateBeanIndexing("demo");
			manager.dropIndexing();
			assertNotNull(manager.google("term", 10));
			assertEquals(0, manager.google("term", 10).getResults().size());
			manager.close();
			manager.shutdown();
		}
	}

	@Test
	void testGetAttachmentWithFileStorageAndIterator() throws Exception {
		tempContentDirectory = Files.createTempDirectory("skyve-content-elastic-");
		UtilImpl.CONTENT_DIRECTORY = tempContentDirectory.toString();
		UtilImpl.CONTENT_FILE_STORAGE = true;

		try (ElasticContentManager manager = new ElasticContentManager()) {
			assertNull(manager.getAttachment(UUIDv7.create().toString()));
			ContentIterator iterator = manager.all().iterator();
			assertFalse(iterator.hasNext());
			assertNull(iterator.next());
			assertEquals(0L, iterator.getTotalHits());
			assertThrows(IllegalAccessError.class, iterator::remove);
		}
	}

	@Test
	void testGetAttachmentWithoutFileStorageReturnsNull() throws Exception {
		UtilImpl.CONTENT_FILE_STORAGE = false;
		try (ElasticContentManager manager = new ElasticContentManager()) {
			assertNull(manager.getAttachment(UUIDv7.create().toString()));
		}
	}

	private static AttachmentContent sampleAttachment() {
		return new AttachmentContent("demo",
									"admin",
									"Contact",
									null,
									"",
									UUIDv7.create().toString(),
									"image").attachment("sample.txt", "text/plain", new byte[] {1, 2});
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
