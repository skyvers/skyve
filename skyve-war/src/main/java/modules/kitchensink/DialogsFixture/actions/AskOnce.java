package modules.kitchensink.DialogsFixture.actions;

import org.skyve.EXT;
import org.skyve.dialog.Question;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.web.WebContext;

import modules.kitchensink.domain.DialogsFixture;

/**
 * Asks one OK/Cancel question, with OK as the default, and records the answer in the outcome.
 * Cancel aborts the action.
 */
public class AskOnce implements ServerSideAction<DialogsFixture> {
	/**
	 * Asks the question and records that it was answered OK.
	 *
	 * @param bean	The fixture.
	 * @param webContext	The current web context.
	 * @return	The fixture.
	 * @throws org.skyve.dialog.QuestionException	until the question is answered.
	 */
	@Override
	public ServerSideActionResult<DialogsFixture> execute(DialogsFixture bean, WebContext webContext) throws Exception {
		EXT.getDialogs().ask(Question.okCancel("DialogsFixture.AskOnce", "Ask once", "Carry on with Ask once?")
										.defaultButton(Question.Button.OK)
										.cancelAborts());
		bean.setOutcome("Ask once answered OK");
		return new ServerSideActionResult<>(bean);
	}
}
