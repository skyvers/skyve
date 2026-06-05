package modules.admin.Tag.actions;

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

import modules.admin.Tag.TagDefaultAction;
import modules.admin.Tag.TagExtension;
import modules.admin.domain.Tag;

/**
 * Server-side action that initiates bulk document actions on tagged items.
 * Launches the PerformDocumentActionForTagJob to execute the configured action
 * (custom or default) on all tagged items, optionally filtered by condition.
 */
public class BulkDocumentAction implements ServerSideAction<TagExtension> {
	/**
	 * Schedules the perform-document-action job and records a human-readable summary.
	 *
	 * @param tag The tag containing action configuration.
	 * @param webContext The current web context.
	 * @return The same tag bean.
	 * @throws Exception If scheduling or metadata lookup fails.
	 */
	@Override
	public ServerSideActionResult<TagExtension> execute(TagExtension tag, WebContext webContext)
			throws Exception {
		User user = getUser();
		Customer customer = user.getCustomer();
		JobMetaData job = getJob(customer);

		runJob(job, tag, user);

		StringBuilder sb = new StringBuilder(128);
		sb.append("Perform action: ");

		if (TagDefaultAction.isDefaultTagAction(tag.getDocumentAction())) {
			TagDefaultAction defaultAction = TagDefaultAction.fromCode(tag.getDocumentAction());
			sb.append(defaultAction.toLocalisedDescription());
		} else {
			sb.append(tag.getDocumentAction());
		}

		Module docMod = getModule(customer, tag.getActionModuleName());
		Document document = getDocument(docMod, customer, tag.getActionDocumentName());
		sb.append("\nFor all tagged instances of: ").append(docMod.getLocalisedTitle());
		sb.append(" ").append(document.getLocalisedPluralAlias());

		if (tag.getDocumentCondition() != null) {
			sb.append("\nMeeting condition: ").append(tag.getDocumentCondition());
		}

		sb.append("\n\nTag action Job started successfully. Check Job for results.");

		tag.setDocumentActionResults(sb.toString());

		growl(webContext);

		return new ServerSideActionResult<>(tag);
	}

	@SuppressWarnings("static-method") // test seam
	protected User getUser() {
		Persistence pers = CORE.getPersistence();
		return pers.getUser();
	}

	@SuppressWarnings("static-method") // test seam
	protected JobMetaData getJob(Customer customer) {
		Module module = customer.getModule(Tag.MODULE_NAME);
		return module.getJob("jPerformDocumentActionForTag");
	}

	@SuppressWarnings("static-method") // test seam
	protected void runJob(JobMetaData job, TagExtension tag, User user) {
		EXT.getJobScheduler().runOneShotJob(job, tag, user);
	}

	@SuppressWarnings("static-method") // test seam
	protected Module getModule(Customer customer, String moduleName) {
		return customer.getModule(moduleName);
	}

	@SuppressWarnings("static-method") // test seam
	protected Document getDocument(Module module, Customer customer, String documentName) {
		return module.getDocument(customer, documentName);
	}

	@SuppressWarnings("static-method") // test seam
	protected void growl(WebContext webContext) {
		webContext.growl(MessageSeverity.info, "Tag action Job has been started");
	}
}
