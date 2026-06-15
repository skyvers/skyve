package org.skyve.impl.metadata.repository.customer;

import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.controller.Interceptor;
import org.skyve.metadata.customer.InterceptorMetaData;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlType;

/**
 * JAXB-annotated descriptor for an {@code <interceptor>} element in a
 * {@code customer.xml} file.
 *
 * <p>Declares a fully-qualified interceptor class name that Skyve will instantiate
 * and register for the customer's persistence and action pipeline.
 *
 * <p>Threading: not thread-safe.  Read-only after JAXB unmarshalling.
 *
 * @see org.skyve.metadata.customer.InterceptorMetaData
 */
@XmlType(namespace = XMLMetaData.CUSTOMER_NAMESPACE, name = "interceptor")
public class InterceptorMetaDataImpl implements InterceptorMetaData {
	private static final long serialVersionUID = 557157560945069012L;

	/**
	 * The fully qualified classname of the interceptor implementation.
	 */
	private String className;
	@SuppressWarnings("java:S3077") // Double-checked locking needs volatile for safe publication of the interceptor reference.
	private volatile Interceptor interceptor;

	/**
	 * Returns the fully-qualified interceptor implementation class name.
	 *
	 * @return interceptor implementation class name, or {@code null}
	 */
	@Override
	public String getClassName() {
		return className;
	}

	/**
	 * Sets the fully-qualified interceptor implementation class name.
	 *
	 * @param className interceptor implementation class name
	 */
	@XmlAttribute(required = true)
	public void setClassName(String className) {
		this.className = UtilImpl.processStringValue(className);
	}

	/**
	 * Returns the lazily-instantiated interceptor implementation.
	 *
	 * <p>Threading: uses double-checked locking over a {@code volatile} field so
	 * exactly one interceptor instance is created per metadata instance.
	 *
	 * @return the interceptor instance, never {@code null}
	 * @throws MetaDataException if the configured class cannot be loaded or instantiated
	 */
	@Override
	public Interceptor getInterceptor() {
		if (interceptor == null) {
			synchronized (this) {
				if (interceptor == null) {
					try {
						Class<?> interceptorClass = Thread.currentThread().getContextClassLoader().loadClass(className);
						interceptor = (Interceptor) interceptorClass.getDeclaredConstructor().newInstance();
					}
					catch (Exception e) {
						throw new MetaDataException("Could not instantiate interceptor class " + className, e);
					}
				}
			}
		}
		
		return interceptor;
	}
}
