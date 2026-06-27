package modules.admin.ControlPanel.actions;

import org.skyve.content.MimeType;
import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.ValidationException;
import org.skyve.metadata.controller.Download;
import org.skyve.metadata.controller.DownloadAction;
import org.skyve.web.WebContext;

import modules.admin.ControlPanel.ControlPanelExtension;

/**
 * Downloads the latest query results generated from Control Panel execution.
 */
public class DownloadResults extends DownloadAction<ControlPanelExtension> {
	private static final String FILE_TITLE = "SAIL.txt";

	/**
	 * Performs the prepare operation.
	 * @param bean the bean value
	 * @param webContext the webContext value
	 * @throws Exception if the operation fails
	 */
	@Override
	public void prepare(ControlPanelExtension bean, WebContext webContext) throws Exception {
		if (bean.getResults() ==  null) {
			throw new ValidationException(new Message("Results are null"));
		}	
	}

	/**
	 * Performs the download operation.
	 * @param bean the bean value
	 * @param webContext the webContext value
	 * @return the operation result
	 * @throws Exception if the operation fails
	 */
	@Override
	public Download download(ControlPanelExtension bean, WebContext webContext) throws Exception {
		
		String results = bean.getUnescapedResults();
		bean.setResults(null);
		
		Download result = new Download(FILE_TITLE, results, MimeType.richtext);
		return result;
	}
}
