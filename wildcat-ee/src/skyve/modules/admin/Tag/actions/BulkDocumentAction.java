package modules.admin.Tag.actions;

import modules.admin.domain.Tag;

import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.module.Job;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.persistence.Persistence;
import org.skyve.web.WebContext;

public class BulkDocumentAction implements ServerSideAction<Tag> {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = 2886341074753936987L;

	/**
	 * Update the payment batch details.
	 */
	@Override
	public ServerSideActionResult execute(Tag tag, WebContext webContext)
	throws Exception {
		
		Persistence pers = CORE.getPersistence();
		User user = pers.getUser();
		Customer customer= user.getCustomer();
		Module module = customer.getModule(Tag.MODULE_NAME);
		Job job = module.getJob("jPerformDocumentActionForTag");

		EXT.runOneShotJob(job, tag, user);
		
		tag.setDocumentActionResults("Actions Job started successfully");
		
		return new ServerSideActionResult(tag);
	}
}
