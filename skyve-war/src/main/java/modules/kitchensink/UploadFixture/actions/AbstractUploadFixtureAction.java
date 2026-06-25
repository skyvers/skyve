package modules.kitchensink.UploadFixture.actions;

import org.skyve.domain.Bean;
import org.skyve.domain.messages.UploadException;
import org.skyve.metadata.controller.Upload;
import org.skyve.metadata.controller.UploadAction;
import org.skyve.web.WebContext;

/**
 * Leaves upload fixture beans unchanged after upload.xhtml action-route inspection.
 */
abstract class AbstractUploadFixtureAction extends UploadAction<Bean> {
	@Override
	public Bean upload(Bean bean, Upload upload, UploadException exception, WebContext webContext) {
		return bean;
	}
}
