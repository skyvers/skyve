package modules.kitchensink.UploadFixture.actions;

import org.skyve.domain.Bean;
import org.skyve.domain.messages.UploadException;
import org.skyve.metadata.controller.Upload;
import org.skyve.metadata.controller.UploadAction;
import org.skyve.web.WebContext;

import jakarta.annotation.Nonnull;

/**
 * Leaves upload fixture beans unchanged after upload.xhtml action-route inspection.
 */
abstract class AbstractUploadFixtureAction extends UploadAction<Bean> {
	@Override
	public @Nonnull Bean upload(@Nonnull Bean bean, @Nonnull Upload upload, @Nonnull UploadException exception, @Nonnull WebContext webContext) {
		return bean;
	}
}
