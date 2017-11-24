package org.skyve.metadata.customer;

import org.skyve.metadata.MetaData;
import org.skyve.metadata.controller.Interceptor;

/**
 * 
 */
public interface InterceptorMetaData extends MetaData {
	/**
	 * 
	 * @return
	 */
	public String getClassName();
	
	public Interceptor getInterceptor(Customer customer);
}
