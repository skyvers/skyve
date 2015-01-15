package org.skyve.persistence;

import org.skyve.domain.Bean;

/**
 * A mixin for iterating and closing result sets produced from Persistence
 * @author sandsm01
 *
 * Use like this
 * <code><pre>
 * try (AutoClosingBeanIterable&lt;MyBean&gt; beans = persistence.iterate(beansQuery)) {
 *     for (MyBean bean : beans) {
 *     }
 * }
 * </pre></code>
 * 
 * @param <T>
 */
public interface AutoClosingBeanIterable<T extends Bean> extends Iterable<T>, AutoCloseable {
	// nothing to see here
}
