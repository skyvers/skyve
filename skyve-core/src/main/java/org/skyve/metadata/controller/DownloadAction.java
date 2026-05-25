package org.skyve.metadata.controller;

import org.skyve.domain.Bean;
import org.skyve.metadata.MetaData;
import org.skyve.web.WebContext;

/**
 * A two-phase action contract that streams a generated file to the browser.
 *
 * <p>The download lifecycle is split across two HTTP requests to allow graceful
 * validation before committing to a potentially expensive file generation:
 * <ol>
 *   <li>{@link #prepare} — called in the first request. Validates the bean and primes
 *       any state needed by the download. If preparation fails, throw an exception to
 *       abort before the download request is made.</li>
 *   <li>{@link #download} — called in the second request. Generates and returns the
 *       {@link Download} transfer object. This method must not fail gracefully; use
 *       {@code prepare} for all validation.</li>
 * </ol>
 *
 * <p>Implementations must be stateless or request-scoped; Skyve creates a new instance
 * per invocation. State needed between the two phases must be stored in the bean or the
 * {@link org.skyve.web.WebContext}.
 *
 * @param <T>  the document bean type the action operates on
 * @see Download
 * @see ImplicitActionName#Download
 */
public abstract class DownloadAction<T extends Bean> implements MetaData {
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
