package modules.kitchensink.DialogsFixture.actions;

import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.dialog.Question;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.web.WebContext;

import modules.kitchensink.domain.DialogsFixture;

/**
 * Saves the fixture and then asks whether to keep the save. Cancel aborts the action, which rolls the save back.
 */
public class SaveThenAsk implements ServerSideAction<DialogsFixture> {
	/**
	 * Saves the fixture, asks the question and records that it was answered OK.
	 *
	 * <p>Side effects: writes the fixture to the database; the write is rolled back each time the question is asked.
	 *
	 * @param bean	The fixture.
	 * @param webContext	The current web context.
	 * @return	The saved fixture.
	 * @throws org.skyve.dialog.QuestionException	until the question is answered.
	 */
	@Override
	public ServerSideActionResult<DialogsFixture> execute(DialogsFixture bean, WebContext webContext) throws Exception {
		DialogsFixture result = CORE.getPersistence().save(bean);
		EXT.getDialogs().ask(Question.okCancel("DialogsFixture.SaveThenAsk", "Save then ask", "The fixture has been saved. Keep the save?")
										.cancelAborts());
		result.setOutcome("Save then ask answered OK");
		return new ServerSideActionResult<>(result);
	}
}
