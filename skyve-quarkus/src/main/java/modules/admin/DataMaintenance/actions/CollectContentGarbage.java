package modules.admin.DataMaintenance.actions;

import org.skyve.EXT;
import org.skyve.domain.messages.MessageSeverity;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.web.WebContext;

import modules.admin.domain.DataMaintenance;

public class CollectContentGarbage implements ServerSideAction<DataMaintenance> {
	@Override
	public ServerSideActionResult<DataMaintenance> execute(DataMaintenance bean, WebContext webContext) throws Exception {
		EXT.getJobScheduler().runContentGarbageCollector();
		webContext.growl(MessageSeverity.info, "Content Garbage Collection Job has been started");
		return new ServerSideActionResult<>(bean);
	}
}
