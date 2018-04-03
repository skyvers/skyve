package modules.admin.ControlPanel.actions;

import org.skyve.CORE;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.web.WebContext;

import modules.admin.ControlPanel.ControlPanelExtension;

public class EvictFromCache implements ServerSideAction<ControlPanelExtension> {
	private static final long serialVersionUID = 3049239808434701310L;

	@Override
	public ServerSideActionResult<ControlPanelExtension> execute(ControlPanelExtension bean, WebContext webContext) 
	throws Exception {
		bean.setResults(null);
		bean.setTabIndex(null);

		try {
			CORE.getRepository().evictCachedMetaData(null);
		}
		catch (Exception e) {
			bean.trapException(e);
		}
		return new ServerSideActionResult<>(bean);
	}
}
