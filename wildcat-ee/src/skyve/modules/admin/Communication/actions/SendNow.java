package modules.admin.Communication.actions;

import modules.admin.domain.Communication;

import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.ValidationException;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.module.Job;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.persistence.Persistence;
import org.skyve.web.WebContext;

public class SendNow implements ServerSideAction<Communication> {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = 2886341074753936987L;

	/**
	 * Kick off the annual returns job.
	 */
	@Override
	public ServerSideActionResult execute(Communication mailout, WebContext webContext) throws Exception {

//		CommunicationBizlet.checkForUnsavedData(mailout);

//		preProcess(mailout);

		Persistence persistence = CORE.getPersistence();
		User user = persistence.getUser();
		Customer customer = user.getCustomer();
		Module module = customer.getModule(Communication.MODULE_NAME);
		Job job = module.getJob("jMailout");

		EXT.runOneShotJob(job, mailout, user);

		StringBuilder sb = new StringBuilder();
		sb.append("The generation job has been commenced - check Admin->Jobs for the log.");
		sb.append("\nEmails will be created into the directory ");
//		sb.append(system.getMailoutDirectory());

		mailout.setResults(sb.toString());

		return new ServerSideActionResult(mailout);
	}

	/**
	 * This should be called from any action kicking off this job also.
	 * 
	 * @throws Exception
	 */
	public static void preProcess(Communication mailout, System system) throws Exception {
		if (mailout.getTag() == null) {
			throw new ValidationException(new Message(Communication.tagPropertyName, "A tag must be selected for the mailout."));
		}

	}
}
