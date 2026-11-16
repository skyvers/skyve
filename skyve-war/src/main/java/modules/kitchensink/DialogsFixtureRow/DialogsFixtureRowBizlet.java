package modules.kitchensink.DialogsFixtureRow;

import org.skyve.EXT;
import org.skyve.dialog.Question;
import org.skyve.domain.Bean;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.web.WebContext;

import modules.kitchensink.domain.DialogsFixtureRow;

/**
 * Asks before zooming out of a dialogs fixture row and before removing one.
 */
public class DialogsFixtureRowBizlet extends Bizlet<DialogsFixtureRow> {
	/**
	 * Asks whether to carry on before ZoomOut and Remove. Cancel aborts the action.
	 *
	 * @param actionName	The implicit action being run.
	 * @param bean	The row.
	 * @param parentBean	The row's parent, if known.
	 * @param webContext	The current web context.
	 * @return	The row.
	 * @throws org.skyve.dialog.QuestionException	until the question is answered.
	 */
	@Override
	public DialogsFixtureRow preExecute(ImplicitActionName actionName,
											DialogsFixtureRow bean,
											Bean parentBean,
											WebContext webContext)
	throws Exception {
		if (ImplicitActionName.ZoomOut.equals(actionName) || ImplicitActionName.Remove.equals(actionName)) {
			EXT.getDialogs().ask(Question.okCancel("DialogsFixtureRow." + actionName,
													actionName.toString(),
													actionName + " this row?")
										.cancelAborts());
		}
		return bean;
	}
}
