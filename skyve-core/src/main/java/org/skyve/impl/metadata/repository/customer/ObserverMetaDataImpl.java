package org.skyve.impl.metadata.repository.customer;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.controller.Observer;
import org.skyve.metadata.customer.ObserverMetaData;

@XmlType(namespace = XMLMetaData.CUSTOMER_NAMESPACE, name = "observer")
public class ObserverMetaDataImpl implements ObserverMetaData {
	private static final long serialVersionUID = -892596276688464356L;

	/**
	 * The fully qualified classname of the observer implementation.
	 */
	private String className;
	private Observer observer;

	@Override
	public String getClassName() {
		return className;
	}

	@XmlAttribute(required = true)
	public void setClassName(String className) {
		this.className = UtilImpl.processStringValue(className);
	}

	@Override
	public Observer getObserver() {
		if (observer == null) {
			try {
				Class<?> observerClass = Thread.currentThread().getContextClassLoader().loadClass(className);
				observer = (Observer) observerClass.getDeclaredConstructor().newInstance();
			}
			catch (Exception e) {
				throw new MetaDataException("Could not instantiate observer class " + className, e);
			}
		}
		
		return observer;
	}
}
