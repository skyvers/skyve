package modules.kitchensink.DialogsFixture.actions;

import org.skyve.EXT;
import org.skyve.dialog.Question;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.web.WebContext;

import modules.kitchensink.domain.DialogsFixture;

/**
 * Asks a Yes/No question with no default button, so No has the focus, and records the answer in the outcome.
 * Both answers carry on.
 */
public class AskYesNo implements ServerSideAction<DialogsFixture> {
	/**
	 * Asks the question and records whether the answer was Yes or No.
	 *
	 * @param bean	The fixture.
	 * @param webContext	The current web context.
	 * @return	The fixture.
	 * @throws org.skyve.dialog.QuestionException	until the question is answered.
	 */
	@Override
	public ServerSideActionResult<DialogsFixture> execute(DialogsFixture bean, WebContext webContext) throws Exception {
		boolean yes = EXT.getDialogs().confirm(Question.yesNo("DialogsFixture.AskYesNo", "Yes or No", "Answer Yes or No. Either answer carries on."));
		bean.setOutcome("Yes or No answered " + (yes ? "Yes" : "No"));
		return new ServerSideActionResult<>(bean);
	}
}
