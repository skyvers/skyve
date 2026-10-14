package modules.kitchensink.ConditionsFixture.actions;

import org.skyve.domain.messages.UploadException;
import org.skyve.metadata.controller.Upload;
import org.skyve.metadata.controller.UploadAction;
import org.skyve.web.WebContext;

import modules.kitchensink.domain.ConditionsFixture;

/**
 * Leaves the bean unchanged, which is all the conditions fixture needs to put a condition on an upload action.
 */
public class ConditionsUpload extends UploadAction<ConditionsFixture> {
	@Override
	public ConditionsFixture upload(ConditionsFixture bean, Upload upload, UploadException exception, WebContext webContext) {
		return bean;
	}
}
