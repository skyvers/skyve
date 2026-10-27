package modules.kitchensink.ActionsFixture.actions;

import org.skyve.domain.messages.UploadException;
import org.skyve.metadata.controller.Upload;
import org.skyve.metadata.controller.UploadAction;
import org.skyve.web.WebContext;

import modules.kitchensink.domain.ActionsFixture;

/**
 * Records that it ran, which is all the actions fixture needs to put a condition on an upload action.
 */
public class ActionsUpload extends UploadAction<ActionsFixture> {
	/**
	 * Records in the outcome that the upload ran.
	 *
	 * @param bean	The fixture.
	 * @param upload	The uploaded file.
	 * @param exception	The problems of the upload.
	 * @param webContext	The current web context.
	 * @return	The fixture.
	 */
	@Override
	public ActionsFixture upload(ActionsFixture bean, Upload upload, UploadException exception, WebContext webContext) {
		bean.setOutcome("Actions Upload ran");
		return bean;
	}
}
