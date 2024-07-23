package modules.admin.Communication.actions;

import java.util.List;

import org.skyve.domain.Bean;
import org.skyve.domain.app.admin.Communication.ActionType;
import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.ValidationException;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.util.CommunicationUtil;
import org.skyve.web.WebContext;

import modules.admin.ModulesUtil;
import modules.admin.Tag.TagBizlet;
import modules.admin.domain.Communication;
import modules.admin.domain.Contact;

public class TestSend implements ServerSideAction<Communication> {

	@Override
	public ServerSideActionResult<Communication> execute(Communication communication, WebContext webContext) throws Exception {

		communication.setActionType(ActionType.sendImmediately);

		// set send to our own address
		Contact me = ModulesUtil.currentAdminUserProxy().getContact();

		// Get First tagged item to test
		List<Bean> beans = TagBizlet.getTaggedItemsForDocument(communication.getTag(), communication.getModuleName(), communication.getDocumentName());

		if (beans.isEmpty()) {
			throw new ValidationException(new Message("There are no tagged items - tag at least 1 (one) item to test this communication."));
		}

		String previousSendToOverride = communication.getSendToOverride();

		// override the recipient to the current logged in user's email address
		try {
			communication.setSendToOverride(me.getEmail1());
			CommunicationUtil.send(webContext, communication, CommunicationUtil.RunMode.ACTION, CommunicationUtil.ResponseMode.EXPLICIT, null, beans.get(0));
		}
		finally {
			// revert the recipient if there was one
			communication.setSendToOverride(previousSendToOverride);
		}
		
		return new ServerSideActionResult<>(communication);
	}
}
