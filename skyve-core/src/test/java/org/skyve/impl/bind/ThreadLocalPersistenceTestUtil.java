package org.skyve.impl.bind;

import java.lang.reflect.Field;

import org.skyve.impl.persistence.AbstractPersistence;

final class ThreadLocalPersistenceTestUtil {
	private static final Field THREAD_LOCAL_PERSISTENCE_FIELD;
	static {
		try {
			THREAD_LOCAL_PERSISTENCE_FIELD = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
			THREAD_LOCAL_PERSISTENCE_FIELD.setAccessible(true);
		}
		catch (Exception e) {
			throw new ExceptionInInitializerError(e);
		}
	}

	private ThreadLocalPersistenceTestUtil() {
		// utility
	}

	/**
	 * Sets the current thread's Skyve persistence instance for tests that rely on {@code CORE.*}.
	 * <p>
	 * Call {@link #clearThreadLocalPersistence()} in a {@code finally} block after use.
	 * </p>
	 *
	 * @param persistence The non-null persistence instance to bind to the current thread.
	 * @throws Exception If reflection access to Skyve's thread-local field fails.
	 */
	@SuppressWarnings("unchecked")
	static void setThreadLocalPersistence(AbstractPersistence persistence) throws Exception {
		ThreadLocal<AbstractPersistence> threadLocal = (ThreadLocal<AbstractPersistence>) threadLocal();
		threadLocal.set(persistence);
	}

	/**
	 * Clears the current thread's bound Skyve persistence instance.
	 *
	 * @throws Exception If reflection access to Skyve's thread-local field fails.
	 */
	@SuppressWarnings("unchecked")
	static void clearThreadLocalPersistence() throws Exception {
		ThreadLocal<AbstractPersistence> threadLocal = (ThreadLocal<AbstractPersistence>) threadLocal();
		threadLocal.remove();
	}

	private static Object threadLocal() throws Exception {
		return THREAD_LOCAL_PERSISTENCE_FIELD.get(null);
	}
}
