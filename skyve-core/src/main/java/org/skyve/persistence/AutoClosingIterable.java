package org.skyve.persistence;

/**
 * An {@link Iterable} that also implements {@link AutoCloseable}, allowing database
 * cursors to be iterated lazily and closed deterministically via try-with-resources.
 *
 * <p>Use this for large result sets where loading all rows into a list at once would
 * require excessive heap. The underlying cursor is held open for the duration of
 * iteration and closed when {@link #close()} is called.
 *
 * <pre>{@code
 * try (AutoClosingIterable<MyBean> beans = query.beanIterable()) {
 *     for (MyBean bean : beans) {
 *         process(bean);
 *     }
 * }
 * }</pre>
 *
 * <p>Implementations are not thread-safe. The iterable must be consumed and closed on
 * the same thread that created it, within the scope of an active
 * {@link Persistence} session.
 *
 * @param <T> the element type returned by the iterator
 */
public interface AutoClosingIterable<T> extends Iterable<T>, AutoCloseable {
	// nothing to see here
}
