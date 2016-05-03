package org.skyve.persistence;

/**
 * A mixin for iterating and closing result sets produced from Persistence
 * @author sandsm01
 *
 * Use like this
 * <code><pre>
 * try (AutoClosingIterable&lt;MyBean&gt; beans = persistence.iterate(beansQuery)) {
 *     for (MyBean bean : beans) {
 *     }
 * }
 * </pre></code>
 * 
 * @param <T>
 */
public interface AutoClosingIterable<T> extends Iterable<T>, AutoCloseable {
	// nothing to see here
}
