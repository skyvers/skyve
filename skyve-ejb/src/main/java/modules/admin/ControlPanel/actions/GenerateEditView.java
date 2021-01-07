package modules.admin.ControlPanel.actions;

import org.skyve.CORE;
import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.ValidationException;
import org.skyve.impl.generate.ViewGenerator;
import org.skyve.impl.metadata.repository.AbstractRepository;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.persistence.Persistence;
import org.skyve.web.WebContext;

import modules.admin.ControlPanel.ControlPanelExtension;
import modules.admin.domain.ControlPanel;

public class GenerateEditView implements ServerSideAction<ControlPanelExtension> {
	private static final long serialVersionUID = 5990074876826469688L;

	@Override
	public ServerSideActionResult<ControlPanelExtension> execute(ControlPanelExtension bean, WebContext webContext)
	throws Exception {
		bean.setResults(null);
		bean.setTabIndex(null);

		String moduleDocumentName = bean.getDesignModuleDocumentName();
		if (moduleDocumentName == null) {
			throw new ValidationException(new Message(ControlPanel.designModuleDocumentNamePropertyName,
														"Select a module.document"));
		}

		try {
			Persistence persistence = CORE.getPersistence();
			User user = persistence.getUser();
			Customer customer = user.getCustomer();
			
			String[] modoc = moduleDocumentName.split("\\.");
			Module module = customer.getModule(modoc[0]);
			Document document = module.getDocument(customer, modoc[1]);
			
			bean.setResults(new ViewGenerator(AbstractRepository.get()).generateEditViewXML(customer, document, false, false));
		}
		catch (Exception e) {
			bean.trapException(e);
		}
		bean.setTabIndex(Integer.valueOf(2));
		return new ServerSideActionResult<>(bean);
	}
}
