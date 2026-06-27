package modules.admin.ControlPanel.actions;

import org.skyve.CORE;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.metadata.repository.CachedRepository;
import org.skyve.web.WebContext;

import modules.admin.ControlPanel.ControlPanelExtension;

/**
 * Evicts a specific cache entry identified by cache name and key.
 */
public class EvictFromCache implements ServerSideAction<ControlPanelExtension> {
	/**
	 * Performs the execute operation.
	 * @param bean the bean value
	 * @param webContext the webContext value
	 * @return the operation result
	 * @throws Exception if the operation fails
	 */
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
