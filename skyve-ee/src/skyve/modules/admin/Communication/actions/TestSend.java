package modules.admin.Communication.actions;

import java.util.List;

import modules.ModulesUtil;
import modules.admin.Communication.CommunicationUtil;
import modules.admin.Tag.TagBizlet;
import modules.admin.domain.Communication;
import modules.admin.domain.Communication.ActionType;
import modules.admin.domain.Contact;

import org.skyve.domain.Bean;
import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.ValidationException;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.web.WebContext;

public class TestSend implements ServerSideAction<Communication> {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = 2886341074753936987L;

	/**
	 * Kick off the annual returns job.
	 */
	@Override
	public ServerSideActionResult<Communication> execute(Communication communication, WebContext webContext) throws Exception {

		communication.setActionType(ActionType.sendImmediately);

		// set send to our own address
		Contact me = ModulesUtil.currentAdminUser().getContact();

		// Get First tagged item to test
		List<Bean> beans = TagBizlet.getTaggedItemsForDocument(communication.getTag(), communication.getModuleName(), communication.getDocumentName());

		if (beans.isEmpty()) {
			throw new ValidationException(new Message("There are no tagged items - tag at least 1 (one) item to test this communication."));
		}

		communication.setSendToOverride(me.getEmail1());
		CommunicationUtil.send(webContext, communication, CommunicationUtil.RunMode.ACTION, CommunicationUtil.ResponseMode.EXPLICIT, null, beans.get(0));

		return new ServerSideActionResult<>(communication);
	}
}
