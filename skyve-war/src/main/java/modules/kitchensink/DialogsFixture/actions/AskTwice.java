package modules.kitchensink.DialogsFixture.actions;

import org.skyve.EXT;
import org.skyve.dialog.Dialogs;
import org.skyve.dialog.Question;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.web.WebContext;

import modules.kitchensink.domain.DialogsFixture;

/**
 * Asks a Yes/No question, then an OK/Cancel question whose text includes the first answer,
 * and records both answers in the outcome. No answers the first question; Cancel aborts the second.
 */
public class AskTwice implements ServerSideAction<DialogsFixture> {
	/**
	 * Asks both questions in order and records both answers.
	 *
	 * @param bean	The fixture.
	 * @param webContext	The current web context.
	 * @return	The fixture.
	 * @throws org.skyve.dialog.QuestionException	until both questions are answered.
	 */
	@Override
	public ServerSideActionResult<DialogsFixture> execute(DialogsFixture bean, WebContext webContext) throws Exception {
		Dialogs dialogs = EXT.getDialogs();
		String first = dialogs.ask(Question.yesNo("DialogsFixture.AskTwice#1", "Ask twice", "First question: Yes or No?"));
		String second = dialogs.ask(Question.okCancel("DialogsFixture.AskTwice#2",
														"Ask twice",
														"You answered " + first + " to the first question. Carry on?")
											.cancelAborts());
		bean.setOutcome("Ask twice answered " + first + " then " + second);
		return new ServerSideActionResult<>(bean);
	}
}
