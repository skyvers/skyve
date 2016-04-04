package modules.admin.DataMaintenance.actions;

import modules.ModulesUtil;
import modules.admin.domain.DataMaintenance;

import org.skyve.CORE;
import org.skyve.bizport.BizPortWorkbook;
import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.ValidationException;
import org.skyve.metadata.controller.BizExportAction;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.web.WebContext;

public class DataMaintenanceExportAction extends BizExportAction {
	private static final long serialVersionUID = -4559826499532491074L;

	@Override
	public BizPortWorkbook bizExport(WebContext webContext) throws Exception {

		DataMaintenance d = (DataMaintenance) webContext.getCurrentBean();
		String modocName = d.getModDocName();
		if (modocName == null) {
			Customer customer = CORE.getPersistence().getUser().getCustomer();
			Module module = customer.getModule(DataMaintenance.MODULE_NAME);
			Document document = module.getDocument(customer, DataMaintenance.DOCUMENT_NAME);

			StringBuilder sb = new StringBuilder(64);
			sb.append("Select ").append(document.getAttribute(DataMaintenance.modDocNamePropertyName).getDisplayName());
			
			throw new ValidationException(new Message(DataMaintenance.modDocNamePropertyName, sb.toString()));
		}

		String[] refs = modocName.split("\\.");
		return ModulesUtil.standardBeanBizExport(refs[0], refs[1], null);
	}
}
