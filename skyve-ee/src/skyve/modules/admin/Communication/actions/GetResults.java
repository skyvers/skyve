package modules.admin.Communication.actions;

import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.ValidationException;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.web.WebContext;

import modules.admin.Communication.CommunicationUtil;
import modules.admin.Tag.TagBizlet;
import modules.admin.domain.Communication;
import modules.admin.domain.Communication.ActionType;

public class GetResults implements ServerSideAction<Communication> {

	private static final long serialVersionUID = -215761915817921911L;

	@Override
	public ServerSideActionResult<Communication> execute(Communication communication, WebContext webContext) throws Exception {

		communication.setActionType(ActionType.testBindingsAndOutput);

		String results = getResults(communication);

		communication.setResults(results);

		Communication result = CommunicationUtil.kickOffJob(communication);

		return new ServerSideActionResult<>(result);
	}

	public static String getResults(Communication bean) throws Exception {

		// validate required fields
		if (bean.getModuleName() == null) {
			throw new ValidationException(
					new Message(Communication.moduleNamePropertyName, "A module must be selected for results."));
		}
		if (bean.getDocumentName() == null) {
			throw new ValidationException(
					new Message(Communication.documentNamePropertyName, "A document must be selected for results."));
		}
		if (bean.getTag() == null) {
			throw new ValidationException(new Message(Communication.tagPropertyName, "A tag must be selected for results."));
		}

		Long count = TagBizlet.getCountOfDocument(bean.getTag(), bean.getModuleName(), bean.getDocumentName());

		StringBuilder results = new StringBuilder();
		results.append(count).append(" communications for ");
		results.append(bean.getDocumentName());

		if (bean.getActionType() != null) {
			results.append(" will be ");
			switch (bean.getActionType()) {
			case saveForBulkSend:
				results.append("created as a batch for download.");
				break;

			case sendImmediately:
				results.append("sent immediately.");
				break;

			case testBindingsAndOutput:
				results.append("tested.");
				break;

			default:
				break;
			}
		}

		return results.toString();
	}
}
