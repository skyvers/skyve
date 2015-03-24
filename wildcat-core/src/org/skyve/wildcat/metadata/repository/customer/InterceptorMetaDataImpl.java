package org.skyve.wildcat.metadata.repository.customer;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlType;

import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.controller.Interceptor;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.customer.InterceptorMetaData;
import org.skyve.wildcat.util.UtilImpl;
import org.skyve.wildcat.util.XMLUtil;

@XmlType(namespace = XMLUtil.CUSTOMER_NAMESPACE, name = "interceptor")
public class InterceptorMetaDataImpl implements InterceptorMetaData {
	/**
	 * For Serialization
	 */
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
	public Interceptor getInterceptor(Customer customer)
	throws MetaDataException {
		if (interceptor == null) {
			try {
				interceptor = (Interceptor) Thread.currentThread().getContextClassLoader().loadClass(className).newInstance();
			}
			catch (Exception e) {
				throw new MetaDataException("Could not instantiate interceptor class " + className, e);
			}
		}
		
		return interceptor;
	}
}
