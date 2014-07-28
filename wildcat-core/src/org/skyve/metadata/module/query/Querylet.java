package org.skyve.metadata.module.query;

import java.util.List;

import org.skyve.domain.Bean;
import org.skyve.metadata.MetaData;
import org.skyve.web.WebContext;

/**
 * 
 * @param <T>
 */
public interface Querylet<T extends WebContext> extends MetaData {
	/**
	 * 
	 * @param webContext
	 * @return
	 * @throws Exception
	 */
	public List<Bean> execute(WebContext webContext) throws Exception;
}
