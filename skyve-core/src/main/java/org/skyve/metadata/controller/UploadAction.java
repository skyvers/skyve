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
	private static final long serialVersionUID = 2843062742565336035L;

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
