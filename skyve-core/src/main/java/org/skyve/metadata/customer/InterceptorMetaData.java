package org.skyve.metadata.customer;

import org.skyve.metadata.SerializableMetaData;
import org.skyve.metadata.controller.Interceptor;

/**
 * 
 */
public interface InterceptorMetaData extends SerializableMetaData {
	/**
	 * 
	 * @return
	 */
	public String getClassName();
	
	public Interceptor getInterceptor();
}
