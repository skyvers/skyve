package org.skyve.impl.metadata.repository.customer;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.controller.Interceptor;
import org.skyve.metadata.customer.InterceptorMetaData;

@XmlType(namespace = XMLMetaData.CUSTOMER_NAMESPACE, name = "interceptor")
public class InterceptorMetaDataImpl implements InterceptorMetaData {
	private static final long serialVersionUID = 557157560945069012L;

	/**
	 * The fully qualified classname of the interceptor implementation.
	 */
	private String className;
	private Interceptor interceptor;

	@Override
	public String getClassName() {
		return className;
	}

	@XmlAttribute(required = true)
	public void setClassName(String className) {
		this.className = UtilImpl.processStringValue(className);
	}

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
