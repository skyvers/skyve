package org.skyve.impl.metadata.repository.customer;

import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.controller.Observer;
import org.skyve.metadata.customer.ObserverMetaData;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlType;

/**
 * JAXB-annotated descriptor for an {@code <observer>} element in a
 * {@code customer.xml} file.
 *
 * <p>Declares a fully-qualified observer class name registered with the Skyve
 * event bus to receive domain-lifecycle and persistence events for this customer.
 *
 * <p>Threading: not thread-safe.  Read-only after JAXB unmarshalling.
 *
 * @see org.skyve.metadata.customer.ObserverMetaData
 */
@XmlType(namespace = XMLMetaData.CUSTOMER_NAMESPACE, name = "observer")
public class ObserverMetaDataImpl implements ObserverMetaData {
	private static final long serialVersionUID = -892596276688464356L;

	/**
	 * The fully qualified classname of the observer implementation.
	 */
	private String className;
	@SuppressWarnings("java:S3077") // Double-checked locking needs volatile for safe publication of the observer reference.
	private volatile Observer observer;

	/**
	 * Returns the fully-qualified observer implementation class name.
	 *
	 * @return observer implementation class name, or {@code null}
	 */
	@Override
	public String getClassName() {
		return className;
	}

	/**
	 * Sets the fully-qualified observer implementation class name.
	 *
	 * @param className observer implementation class name
	 */
	@XmlAttribute(required = true)
	public void setClassName(String className) {
		this.className = UtilImpl.processStringValue(className);
	}

	/**
	 * Returns the lazily-instantiated observer implementation.
	 *
	 * <p>Threading: uses double-checked locking over a {@code volatile} field so
	 * exactly one observer instance is created per metadata instance.
	 *
	 * @return the observer instance, never {@code null}
	 * @throws MetaDataException if the configured class cannot be loaded or instantiated
	 */
	@Override
	public Observer getObserver() {
		if (observer == null) {
			synchronized (this) {
				if (observer == null) {
					try {
						Class<?> observerClass = Thread.currentThread().getContextClassLoader().loadClass(className);
						observer = (Observer) observerClass.getDeclaredConstructor().newInstance();
					}
					catch (Exception e) {
						throw new MetaDataException("Could not instantiate observer class " + className, e);
					}
				}
			}
		}
		
		return observer;
	}
}
