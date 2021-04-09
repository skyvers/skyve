package modules.admin.DataMaintenance.actions;

import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.MessageSeverity;
import org.skyve.domain.messages.ValidationException;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.JobMetaData;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.web.WebContext;

import modules.admin.domain.DataMaintenance;

public class Restore implements ServerSideAction<DataMaintenance> {
	private static final long serialVersionUID = 8521252561712649481L;

	@Override
	public ServerSideActionResult<DataMaintenance> execute(DataMaintenance bean, WebContext webContext)
	throws Exception {
		User u = CORE.getUser();
		Customer c = u.getCustomer();
		Module m = c.getModule(DataMaintenance.MODULE_NAME);
		
		if (bean.getRestorePreProcess() == null) {
			Document d = m.getDocument(c, DataMaintenance.DOCUMENT_NAME);
			
			StringBuilder sb = new StringBuilder(64);
			sb.append("You must select a ");
			sb.append(d.getAttribute(DataMaintenance.restorePreProcessPropertyName).getLocalisedDisplayName());
			sb.append(" before you can perform this action.");
			
			throw new ValidationException(new Message(DataMaintenance.restorePreProcessPropertyName, sb.toString()));
		}
		
		JobMetaData job = m.getJob("jRestore");
		EXT.runOneShotJob(job, bean, u);
		webContext.growl(MessageSeverity.info, "Restore Job has been started");

		return new ServerSideActionResult<>(bean);
	}
}
