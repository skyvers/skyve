package modules.kitchensink.DialogsFixture.actions;

import org.skyve.EXT;
import org.skyve.dialog.Question;
import org.skyve.dialog.QuestionException;
import org.skyve.domain.messages.DomainException;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.web.WebContext;

import modules.kitchensink.domain.DialogsFixture;

/**
 * Asks a question and wraps the question exception in another exception, as an interceptor or helper might,
 * to show that a wrapped question is still asked. Cancel aborts the action.
 */
public class AskWrapped implements ServerSideAction<DialogsFixture> {
	/**
	 * Asks the question, wrapping its exception, and records that it was answered OK.
	 *
	 * @param bean	The fixture.
	 * @param webContext	The current web context.
	 * @return	The fixture.
	 * @throws DomainException	wrapping the question exception until the question is answered.
	 */
	@Override
	public ServerSideActionResult<DialogsFixture> execute(DialogsFixture bean, WebContext webContext) throws Exception {
		try {
			EXT.getDialogs().ask(Question.okCancel("DialogsFixture.AskWrapped", "Ask wrapped", "Carry on with Ask wrapped?").cancelAborts());
		}
		catch (QuestionException e) {
			throw new DomainException("Wrapped by AskWrapped", e);
		}
		bean.setOutcome("Ask wrapped answered OK");
		return new ServerSideActionResult<>(bean);
	}
}
