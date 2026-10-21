package modules.kitchensink.ActionsFixture.actions;

import org.skyve.content.MimeType;
import org.skyve.metadata.controller.Download;
import org.skyve.metadata.controller.DownloadAction;
import org.skyve.web.WebContext;

import modules.kitchensink.domain.ActionsFixture;

/**
 * Downloads a line of text, and disables itself as it prepares the download, so the download that follows
 * the prepare step is requested while the action is no longer available.
 */
public class DownloadOnce extends DownloadAction<ActionsFixture> {
	/**
	 * Sets Downloaded, which disables this action, and records in the outcome that the download was prepared.
	 *
	 * @param bean	The fixture.
	 * @param webContext	The current web context.
	 */
	@Override
	public void prepare(ActionsFixture bean, WebContext webContext) {
		bean.setDownloaded(Boolean.TRUE);
		bean.setOutcome("Download Once prepared");
	}

	/**
	 * Returns a line of text.
	 *
	 * @param bean	The fixture.
	 * @param webContext	The current web context.
	 * @return	The download.
	 */
	@Override
	public Download download(ActionsFixture bean, WebContext webContext) {
		return new Download("actions.txt", "Actions Fixture", MimeType.plain);
	}
}
