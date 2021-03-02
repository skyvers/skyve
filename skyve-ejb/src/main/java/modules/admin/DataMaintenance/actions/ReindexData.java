package modules.admin.DataMaintenance.actions;

import modules.admin.domain.DataMaintenance;

import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.domain.messages.MessageSeverity;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.module.JobMetaData;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.web.WebContext;

public class ReindexData implements ServerSideAction<DataMaintenance> {
	private static final long serialVersionUID = -5036413477264983775L;

	@Override
	public ServerSideActionResult<DataMaintenance> execute(DataMaintenance bean, WebContext webContext)
	throws Exception {
		User u = CORE.getUser();
		Customer c = u.getCustomer();
		Module m = c.getModule(DataMaintenance.MODULE_NAME);
		
		JobMetaData job = m.getJob("jReindexBeans");
		EXT.runOneShotJob(job, bean, u);
		webContext.growl(MessageSeverity.info, "Reindex Job has been started");
		return new ServerSideActionResult<>(bean);
	}
}
