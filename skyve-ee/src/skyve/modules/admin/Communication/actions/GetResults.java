package modules.admin.Communication.actions;

import modules.admin.Communication.CommunicationUtil;
import modules.admin.Tag.TagBizlet;
import modules.admin.domain.Communication;
import modules.admin.domain.Communication.ActionType;

import org.skyve.CORE;
import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.ValidationException;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.persistence.Persistence;
import org.skyve.web.WebContext;

public class GetResults implements ServerSideAction<Communication> {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = 2886341074753936987L;

	/**
	 * Kick off the annual returns job.
	 */
	@Override
	public ServerSideActionResult<Communication> execute(Communication communication, WebContext webContext) throws Exception {

		communication.setActionType(ActionType.testBindingsAndOutput);

		Persistence pers = CORE.getPersistence();

		Communication result = pers.save(communication);

		String results = getResults(result);

		result.setResults(results);

		result = CommunicationUtil.kickOffJob(result);

		return new ServerSideActionResult<>(result);
	}

	public static String getResults(Communication bean) throws Exception {

		if (bean.getTag() == null) {
			throw new ValidationException(new Message(Communication.tagPropertyName, "A tag must be selected for results."));
		}

		Persistence pers = CORE.getPersistence();
		Communication communication = pers.save(bean);

		Long count = TagBizlet.getTaggedCountForDocument(communication.getTag(), communication.getModuleName(), communication.getDocumentName());

		StringBuilder results = new StringBuilder();
		results.append(count).append(" communications for ");
		results.append(communication.getDocumentName());

		if (communication.getActionType() != null) {
			results.append(" will be ");
			switch (communication.getActionType()) {
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
