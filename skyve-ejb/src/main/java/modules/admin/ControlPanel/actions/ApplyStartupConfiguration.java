package modules.admin.ControlPanel.actions;

import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.web.WebContext;

import modules.admin.ControlPanel.ControlPanelExtension;

public class ApplyStartupConfiguration implements ServerSideAction<ControlPanelExtension> {
	@Override
	public ServerSideActionResult<ControlPanelExtension> execute(ControlPanelExtension bean, WebContext webContext) 
	throws Exception {
		
		bean.applyStartupConfiguration();
		
		return new ServerSideActionResult<>(bean);
	}
}
