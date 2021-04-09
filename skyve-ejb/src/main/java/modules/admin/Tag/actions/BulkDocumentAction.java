package modules.admin.Tag.actions;

import modules.admin.Tag.TagDefaultAction;
import modules.admin.domain.Tag;

import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.domain.messages.MessageSeverity;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.JobMetaData;
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
	public ServerSideActionResult<Tag> execute(Tag tag, WebContext webContext)
	throws Exception {
		
		Persistence pers = CORE.getPersistence();
		User user = pers.getUser();
		Customer customer= user.getCustomer();
		Module module = customer.getModule(Tag.MODULE_NAME);
		JobMetaData job = module.getJob("jPerformDocumentActionForTag");

		EXT.runOneShotJob(job, tag, user);
		
		StringBuilder sb = new StringBuilder(128);
		sb.append("Perform action: ");
		
		if (TagDefaultAction.isDefaultTagAction(tag.getDocumentAction())) {
			TagDefaultAction defaultAction  = TagDefaultAction.fromCode(tag.getDocumentAction());
			sb.append(defaultAction.toDescription());	
		} else {
			sb.append(tag.getDocumentAction());
		}
		
		Module docMod = customer.getModule(tag.getActionModuleName());
		Document document = docMod.getDocument(customer, tag.getActionDocumentName());		
		sb.append("\nFor all tagged instances of: ").append(docMod.getLocalisedTitle());
		sb.append(" ").append(document.getLocalisedPluralAlias());
		
		if(tag.getDocumentCondition()!=null){
			sb.append("\nMeeting condition: ").append(tag.getDocumentCondition());
		}
		
		sb.append("\n\nTag action Job started successfully. Check Job for results.");
		
		tag.setDocumentActionResults(sb.toString());
		
		webContext.growl(MessageSeverity.info, "Tag action Job has been started");		
		
		return new ServerSideActionResult<>(tag);
	}
}
