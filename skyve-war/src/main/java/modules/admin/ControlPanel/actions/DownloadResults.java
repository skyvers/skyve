package modules.admin.ControlPanel.actions;

import org.skyve.content.MimeType;
import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.ValidationException;
import org.skyve.metadata.controller.Download;
import org.skyve.metadata.controller.DownloadAction;
import org.skyve.web.WebContext;

import modules.admin.ControlPanel.ControlPanelExtension;

public class DownloadResults extends DownloadAction<ControlPanelExtension> {
	private static final String FILE_TITLE = "sail.txt";

	@Override
	public void prepare(ControlPanelExtension bean, WebContext webContext) throws Exception {
		if (bean.getResults() ==  null) {
			throw new ValidationException(new Message("Results are null"));
		}	
	}

	@Override
	public Download download(ControlPanelExtension bean, WebContext webContext) throws Exception {
		
		String results = bean.getUnescapedResults();
		bean.setResults(null);
		
		Download result = new Download(FILE_TITLE, results, MimeType.richtext);
		return result;
	}
}
