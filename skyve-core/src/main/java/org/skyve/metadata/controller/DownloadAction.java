package org.skyve.metadata.controller;

import org.skyve.domain.Bean;
import org.skyve.metadata.SerializableMetaData;
import org.skyve.web.WebContext;

/**
 * 
 * @param <T>
 */
public abstract class DownloadAction<T extends Bean> implements SerializableMetaData {
	private static final long serialVersionUID = -1360787878049667579L;

	/**
	 * Called before the download method.
	 * Use this method to validate the bean and prepare for the download.
	 * This is called in a separate request (and thread) from the download method.
	 * 
	 * @param bean	The bean to execute on.
	 * @param webContext	The context to manipulate.
	 * @throws Exception
	 */
	public abstract void prepare(T bean, WebContext webContext) throws Exception;
	
	/**
	 * Called to get the download stream/file.
	 * This method can not fail gracefully which is why prepare should be used to validate.
	 * This is called in a separate request (and thread) from the prepare method.
	 * 
	 * @param bean	The bean to execute on.
	 * @param webContext	The context to manipulate.
	 * @return file	The file to process.
	 * @throws Exception
	 */
	public abstract Download download(T bean, WebContext webContext) throws Exception;
}
