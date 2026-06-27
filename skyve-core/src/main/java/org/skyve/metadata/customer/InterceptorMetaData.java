package org.skyve.metadata.customer;

import org.skyve.metadata.SerializableMetaData;
import org.skyve.metadata.controller.Interceptor;

/**
 * Metadata descriptor for a registered {@link Interceptor} implementation.
 *
 * <p>Declared in the customer XML under {@code <interceptors>}, each entry
 * names an {@link Interceptor} subclass. The runtime instantiates the class
 * on first access and caches the singleton via {@link #getInterceptor()}.
 *
 * @see Customer#getInterceptors()
 * @see Interceptor
 */
public interface InterceptorMetaData extends SerializableMetaData {
	/**
	 * Returns the fully-qualified class name of the {@link Interceptor} subclass.
	 *
	 * @return the class name; never {@code null}
	 */
	public String getClassName();

	/**
	 * Returns the singleton {@link Interceptor} instance, instantiating it if necessary.
	 *
	 * @return the interceptor instance; never {@code null}
	 */
	public Interceptor getInterceptor();
}
