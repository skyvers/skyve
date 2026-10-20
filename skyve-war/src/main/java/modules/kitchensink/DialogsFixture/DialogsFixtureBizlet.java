package modules.kitchensink.DialogsFixture;

import org.skyve.EXT;
import org.skyve.dialog.Question;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.web.WebContext;

import modules.kitchensink.domain.DialogsFixture;

/**
 * Asks questions from the lifecycle of the dialogs fixture: on save, on delete and when its text changes.
 */
public class DialogsFixtureBizlet extends Bizlet<DialogsFixture> {
	/**
	 * Asks whether to save when the fixture asks on save, and records the answer in the outcome.
	 * Cancel aborts the save.
	 *
	 * @param bean	The fixture being saved.
	 * @throws org.skyve.dialog.QuestionException	until the question is answered.
	 */
	@Override
	public void preSave(DialogsFixture bean) throws Exception {
		if (Boolean.TRUE.equals(bean.getAskOnSave())) {
			EXT.getDialogs().ask(Question.okCancel("DialogsFixture.preSave", "Save", "Save this dialogs fixture?").cancelAborts());
			bean.setOutcome("preSave answered OK");
		}
	}

	/**
	 * Asks whether to delete when the fixture asks on delete. Cancel aborts the delete.
	 *
	 * @param bean	The fixture being deleted.
	 * @throws org.skyve.dialog.QuestionException	until the question is answered.
	 */
	@Override
	public void preDelete(DialogsFixture bean) throws Exception {
		if (Boolean.TRUE.equals(bean.getAskOnDelete())) {
			EXT.getDialogs().ask(Question.okCancel("DialogsFixture.preDelete", "Delete", "Delete this dialogs fixture?").cancelAborts());
		}
	}

	/**
	 * Asks whether to carry on when the text changes, with the new text in the question, and records the answer
	 * in the outcome. Cancel aborts the rest of the change logic and keeps the new text.
	 *
	 * @param source	The binding that changed.
	 * @param bean	The fixture.
	 * @param webContext	The current web context.
	 * @throws org.skyve.dialog.QuestionException	until the question is answered.
	 */
	@Override
	public void preRerender(String source, DialogsFixture bean, WebContext webContext) throws Exception {
		if (DialogsFixture.textPropertyName.equals(source)) {
			String text = bean.getText();
			EXT.getDialogs().ask(Question.okCancel("DialogsFixture.preRerender",
													"Text changed",
													"You changed the text to " + text + ". Carry on?")
										.defaultButton(Question.Button.OK)
										.cancelAborts());
			bean.setOutcome("Text change to " + text + " answered OK");
		}
	}
}
