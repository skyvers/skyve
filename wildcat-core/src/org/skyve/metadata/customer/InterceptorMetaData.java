package org.skyve.metadata.customer;

import org.skyve.metadata.MetaData;
import org.skyve.metadata.MetaDataException;

/**
 * 
 */
public interface InterceptorMetaData extends MetaData {
	/**
	 * 
	 * @return
	 */
	public String getClassName();
	
	public org.skyve.metadata.controller.Interceptor getInterceptor(Customer customer)
	throws MetaDataException;
}
