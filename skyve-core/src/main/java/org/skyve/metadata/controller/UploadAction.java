package org.skyve.metadata.controller;

import org.skyve.domain.Bean;
import org.skyve.domain.messages.UploadException;
import org.skyve.metadata.MetaData;
import org.skyve.web.WebContext;

/**
 * 
 * @param <T>
 */
public abstract class UploadAction<T extends Bean> implements MetaData {
	/**
	 * 
	 * @param bean
	 * @param upload
	 * @param exception
	 * @param webContext
	 * @return
	 * @throws Exception
	 */
	public abstract T upload(T bean, Upload upload, UploadException exception, WebContext webContext) throws Exception;
}
