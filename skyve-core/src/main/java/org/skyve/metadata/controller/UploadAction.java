package org.skyve.metadata.controller;

import org.skyve.domain.Bean;
import org.skyve.domain.messages.UploadException;
import org.skyve.metadata.MetaData;
import org.skyve.web.WebContext;

/**
 * Processes a file uploaded by the user.
 *
 * <p>Implementations are registered in the document's {@code <upload>} action metadata
 * and invoked after Skyve has received the multipart POST from the browser. The
 * {@link #upload} method receives the file wrapped in an {@link Upload} transfer object
 * and may accumulate validation errors into an {@link UploadException}.
 *
 * <p>Implementations must be stateless or request-scoped; Skyve creates a new instance
 * per invocation.
 *
 * @param <T>  the document bean type the action operates on
 * @see Upload
 * @see ImplicitActionName#Upload
 */
public abstract class UploadAction<T extends Bean> implements MetaData {
	/**
	 * Processes the uploaded file.
	 *
	 * @param bean        the current bean from the view; never {@code null}
	 * @param upload      the uploaded file transfer object; never {@code null}
	 * @param exception   accumulates non-fatal validation problems; callers will throw
	 *                    the exception after this method returns if it is non-empty
	 * @param webContext  the current web context; never {@code null}
	 * @return the (potentially updated) bean to use for the next render cycle
	 * @throws Exception if the upload cannot proceed at all
	 */
	public abstract T upload(T bean, Upload upload, UploadException exception, WebContext webContext) throws Exception;
}
