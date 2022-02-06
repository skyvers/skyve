package modules.admin.ControlPanel.actions;

import org.skyve.metadata.controller.Download;
import org.skyve.metadata.controller.DownloadAction;
import org.skyve.web.WebContext;

import modules.admin.ControlPanel.ControlPanelExtension;
import modules.admin.domain.ControlPanel.SailExecutor;
import modules.admin.domain.ControlPanel.SailUserAgentType;
import router.UxUis;

public class DownloadClientSAIL extends DownloadAction<ControlPanelExtension> {
	@Override
	public void prepare(ControlPanelExtension bean, WebContext webContext)
			throws Exception {
		bean.setSailExecutor(SailExecutor.primeFacesInlineWebDriver);
		bean.setSailUxUi(UxUis.EXTERNAL.getName());
		bean.setSailUserAgentType(SailUserAgentType.desktop);

		new GenerateMenuSAIL().execute(bean, webContext);
		bean.setSail(bean.getUnescapedResults());

		new DownloadSAIL().prepare(bean, webContext);
	}

	@Override
	public Download download(ControlPanelExtension bean, WebContext webContext)
			throws Exception {
		return new DownloadSAIL().download(bean, webContext);
	}
}
