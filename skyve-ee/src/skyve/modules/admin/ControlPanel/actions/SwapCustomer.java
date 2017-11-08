package modules.admin.ControlPanel.actions;

import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.impl.metadata.repository.AbstractRepository;
import org.skyve.impl.metadata.user.UserImpl;
import org.skyve.CORE;
import org.skyve.web.WebContext;

import modules.admin.ControlPanel.ControlPanelExtension;

/**
 * Swap to the given customer.
 */
public class SwapCustomer implements ServerSideAction<ControlPanelExtension> {
	private static final long serialVersionUID = 6593556689106860234L;

	@Override
	public ServerSideActionResult<ControlPanelExtension> execute(ControlPanelExtension bean, WebContext webContext) 
	throws Exception {
		try {
			UserImpl user = (UserImpl) CORE.getPersistence().getUser();
			user.setCustomerName(bean.getCustomerNameToSwapTo());
			user.clearModuleMenus();
			AbstractRepository.get().resetMenus(user);
		}
		catch (Exception e) {
			bean.trapException(e);
		}
		
		return new ServerSideActionResult<>(bean);
	}
}
