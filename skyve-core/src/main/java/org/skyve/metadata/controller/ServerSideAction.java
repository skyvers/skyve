package org.skyve.metadata.controller;

import org.skyve.domain.Bean;
import org.skyve.metadata.MetaData;
import org.skyve.web.WebContext;

/**
 * 
 * @param <T>
 */
public interface ServerSideAction<T extends Bean> extends MetaData {
	/**
	 * 
	 * @param bean the bean to execute on.
	 * @param webContext The context to manipulate
	 * @return The result - the bean and whether to execute the current location - postback.
	 * @throws Exception
	 */
	public ServerSideActionResult<T> execute(T bean, WebContext webContext) throws Exception;
}
