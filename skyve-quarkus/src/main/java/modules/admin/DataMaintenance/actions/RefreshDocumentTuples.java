package modules.admin.DataMaintenance.actions;

import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.MessageSeverity;
import org.skyve.domain.messages.ValidationException;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.module.JobMetaData;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.persistence.Persistence;
import org.skyve.web.WebContext;

import modules.admin.domain.DataMaintenance;

public class RefreshDocumentTuples implements ServerSideAction<DataMaintenance> {
	@Override
	public ServerSideActionResult<DataMaintenance> execute(DataMaintenance bean, WebContext webContext)
			throws Exception {
		
		if(bean.getRefreshOption()==null){
			throw new ValidationException(new Message(DataMaintenance.refreshOptionPropertyName, "Value required."));
		}

		Persistence pers = CORE.getPersistence();
		User user = pers.getUser();
		Customer customer = user.getCustomer();
		Module module = customer.getModule(DataMaintenance.MODULE_NAME);
		JobMetaData job = module.getJob("jRefreshDocumentTuples");
		
		EXT.getJobScheduler().runOneShotJob(job, bean, user);
	
		bean.setAuditResponse("Job commenced.");
		webContext.growl(MessageSeverity.info, "Refresh Job has been started");

		return new ServerSideActionResult<>(bean);
	}
	
}
