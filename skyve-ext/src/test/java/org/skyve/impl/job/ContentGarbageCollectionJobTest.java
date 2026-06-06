package org.skyve.impl.job;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

import java.lang.reflect.Field;
import java.util.Collections;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.content.ContentIterable;
import org.skyve.content.SearchResult;
import org.skyve.impl.content.AbstractContentManager;
import org.skyve.impl.content.NoOpContentManager;
import org.skyve.impl.persistence.AbstractPersistence;

class ContentGarbageCollectionJobTest {
	private Class<? extends AbstractContentManager> originalContentManagerClass;
	private AbstractPersistence persistence;

	@BeforeEach
	void setup() throws Exception {
		originalContentManagerClass = AbstractContentManager.IMPLEMENTATION_CLASS;
		AbstractContentManager.IMPLEMENTATION_CLASS = EmptyContentManager.class;
		persistence = mock(AbstractPersistence.class);
		bindPersistenceToThread(persistence);
	}

	@AfterEach
	void teardown() throws Exception {
		AbstractContentManager.IMPLEMENTATION_CLASS = originalContentManagerClass;
		unbindPersistenceFromThread();
	}

	@Test
	void executeCommitsWhenThereIsNoContentToCollect() {
		ContentGarbageCollectionJob job = new ContentGarbageCollectionJob();

		assertDoesNotThrow(() -> job.execute(null));
		verify(persistence).commit(true);
	}

	public static class EmptyContentManager extends NoOpContentManager {
		@Override
		public ContentIterable all() {
			return () -> new ContentIterable.ContentIterator() {
				private final java.util.Iterator<SearchResult> delegate = Collections.emptyIterator();

				@Override
				public long getTotalHits() {
					return 0;
				}

				@Override
				public boolean hasNext() {
					return delegate.hasNext();
				}

				@Override
				public SearchResult next() {
					return delegate.next();
				}
			};
		}
	}

	private static void bindPersistenceToThread(AbstractPersistence persistence) throws Exception {
		Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
		field.setAccessible(true);
		@SuppressWarnings("unchecked")
		ThreadLocal<AbstractPersistence> threadLocal = (ThreadLocal<AbstractPersistence>) field.get(null);
		threadLocal.set(persistence);
	}

	private static void unbindPersistenceFromThread() throws Exception {
		Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
		field.setAccessible(true);
		@SuppressWarnings("unchecked")
		ThreadLocal<AbstractPersistence> threadLocal = (ThreadLocal<AbstractPersistence>) field.get(null);
		threadLocal.remove();
	}
}
