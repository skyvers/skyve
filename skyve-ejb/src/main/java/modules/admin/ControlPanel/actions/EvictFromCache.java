package modules.admin.ControlPanel.actions;

import org.skyve.CORE;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.metadata.repository.CachedRepository;
import org.skyve.web.WebContext;

import modules.admin.ControlPanel.ControlPanelExtension;

public class EvictFromCache implements ServerSideAction<ControlPanelExtension> {
	@Override
	public ServerSideActionResult<ControlPanelExtension> execute(ControlPanelExtension bean, WebContext webContext) 
	throws Exception {
		bean.setResults(null);
		bean.setTabIndex(null);

		try {
			((CachedRepository) CORE.getRepository()).evictCachedMetaData((UtilImpl.CUSTOMER == null) ? CORE.getCustomer() : null);
		}
		catch (Exception e) {
			bean.trapException(e);
		}
		return new ServerSideActionResult<>(bean);
	}
}
