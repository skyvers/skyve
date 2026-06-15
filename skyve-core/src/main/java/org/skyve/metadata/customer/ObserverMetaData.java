package org.skyve.metadata.customer;

import org.skyve.metadata.SerializableMetaData;
import org.skyve.metadata.controller.Observer;

/**
 * Metadata descriptor for a registered {@link Observer} implementation.
 *
 * <p>Declared in the customer XML under {@code <observers>}, each entry
 * names an {@link Observer} implementation. The runtime instantiates the class
 * on first access and caches the singleton via {@link #getObserver()}.
 *
 * @see Customer#getObservers()
 * @see Observer
 */
public interface ObserverMetaData extends SerializableMetaData {
	/**
	 * Returns the fully-qualified class name of the {@link Observer} implementation.
	 *
	 * @return the class name; never {@code null}
	 */
	public String getClassName();

	/**
	 * Returns the singleton {@link Observer} instance, instantiating it if necessary.
	 *
	 * @return the observer instance; never {@code null}
	 */
	public Observer getObserver();
}
