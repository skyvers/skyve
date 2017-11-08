package modules.admin.ControlPanel.actions;

import org.skyve.CORE;
import org.skyve.impl.generate.ViewGenerator;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.persistence.Persistence;
import org.skyve.web.WebContext;

import modules.admin.ControlPanel.ControlPanelExtension;

public class GenerateEditView implements ServerSideAction<ControlPanelExtension> {
	private static final long serialVersionUID = 5990074876826469688L;

	@Override
	public ServerSideActionResult<ControlPanelExtension> execute(ControlPanelExtension bean, WebContext webContext)
	throws Exception {
		if (bean.getDesignModuleDocumentName() != null) {
			try {
				Persistence persistence = CORE.getPersistence();
				User user = persistence.getUser();
				Customer customer = user.getCustomer();
				
				String[] modoc = bean.getDesignModuleDocumentName().split("\\.");
				Module module = customer.getModule(modoc[0]);
				Document document = module.getDocument(customer, modoc[1]);
				
				bean.setResults(ViewGenerator.generateEditViewXML(customer, document, false, false));
			}
			catch (Exception e) {
				bean.trapException(e);
			}
		}
		else {
			bean.setResults(null);
		}
		return new ServerSideActionResult<>(bean);
	}
}
