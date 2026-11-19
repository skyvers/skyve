package modules.kitchensink.ConditionsFixture.actions;

import org.skyve.content.MimeType;
import org.skyve.metadata.controller.Download;
import org.skyve.metadata.controller.DownloadAction;
import org.skyve.web.WebContext;

import modules.kitchensink.domain.ConditionsFixture;

/**
 * Downloads a line of text, which is all the conditions fixture needs to put a condition on a download action.
 */
public class ConditionsDownload extends DownloadAction<ConditionsFixture> {
	@Override
	public void prepare(ConditionsFixture bean, WebContext webContext) throws Exception {
		// nothing to prepare
	}

	@Override
	public Download download(ConditionsFixture bean, WebContext webContext) throws Exception {
		return new Download("conditions.txt", "Conditions Fixture", MimeType.plain);
	}
}
