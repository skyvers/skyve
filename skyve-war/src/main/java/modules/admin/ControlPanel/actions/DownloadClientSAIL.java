package modules.admin.ControlPanel.actions;

import org.skyve.metadata.controller.Download;
import org.skyve.metadata.controller.DownloadAction;
import org.skyve.web.WebContext;

import modules.admin.ControlPanel.ControlPanelExtension;
import modules.admin.domain.ControlPanel.SailExecutor;
import modules.admin.domain.ControlPanel.SailUserAgentType;
import router.UxUis;

/**
 * Downloads generated client-side SAIL output.
 */
public class DownloadClientSAIL extends DownloadAction<ControlPanelExtension> {
	/**
	 * Performs the prepare operation.
	 * @param bean the bean value
	 * @param webContext the webContext value
	 * @throws Exception if the operation fails
	 */
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

	/**
	 * Performs the download operation.
	 * @param bean the bean value
	 * @param webContext the webContext value
	 * @return the operation result
	 * @throws Exception if the operation fails
	 */
	@Override
	public Download download(ControlPanelExtension bean, WebContext webContext)
			throws Exception {
		return new DownloadSAIL().download(bean, webContext);
	}
}
