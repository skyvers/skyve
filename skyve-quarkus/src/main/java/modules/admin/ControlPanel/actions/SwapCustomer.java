package modules.admin.ControlPanel.actions;

import org.skyve.CORE;
import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.ValidationException;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.impl.metadata.user.UserImpl;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.web.WebContext;

import modules.admin.ControlPanel.ControlPanelExtension;
import modules.admin.domain.ControlPanel;

/**
 * Swap to the given customer.
 */
public class SwapCustomer implements ServerSideAction<ControlPanelExtension> {
	@Override
	public ServerSideActionResult<ControlPanelExtension> execute(ControlPanelExtension bean, WebContext webContext) 
	throws Exception {
		bean.setTabIndex(null);
		String customer = bean.getCustomerNameToSwapTo();
		if (customer == null) {
			throw new ValidationException(new Message(ControlPanel.customerNameToSwapToPropertyName,
														"Select a customer"));
		}
		
		try {
			UserImpl user = (UserImpl) CORE.getPersistence().getUser();
			user.setCustomerName(customer);
			user.clearModuleMenus();
			ProvidedRepositoryFactory.get().resetMenus(user);
		}
		catch (Exception e) {
			bean.trapException(e);
		}
		return new ServerSideActionResult<>(bean);
	}
}
