package modules.admin.ControlPanel.actions;

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
import org.skyve.persistence.DocumentQuery;
import org.skyve.web.WebContext;

import modules.admin.ControlPanel.ControlPanelExtension;
import modules.admin.domain.ControlPanel;
import modules.admin.domain.Tag;

public class DeleteTestData implements ServerSideAction<ControlPanelExtension> {

	@Override
	public ServerSideActionResult<ControlPanelExtension> execute(ControlPanelExtension bean, WebContext webContext)
			throws Exception {
		
		if(bean.getTestTagName()==null) {
			throw new ValidationException(new Message(ControlPanel.testTagNamePropertyName, "Enter a tag for the delete"));
		}
		Customer customer = CORE.getCustomer();
		Module m = customer.getModule(ControlPanel.MODULE_NAME);
		JobMetaData job = m.getJob("jDeleteAllTaggedDataForTag");
		
		// Retrieve the Tag for the delete
		DocumentQuery q = CORE.getPersistence().newDocumentQuery(Tag.MODULE_NAME, Tag.DOCUMENT_NAME);
		q.getFilter().addEquals(Tag.namePropertyName, bean.getTestTagName());
		Tag tagForDelete = q.beanResult();
		if(tagForDelete!=null) {
			EXT.getJobScheduler().runOneShotJob(job, tagForDelete, CORE.getUser());
			webContext.growl(MessageSeverity.info, "Delete All Tagged Data For Tag Job has been started");
		} else {
			throw new ValidationException(new Message(ControlPanel.testTagNamePropertyName, "You have no tag by that name"));
		}
		
		return new ServerSideActionResult<>(bean);
	}

}