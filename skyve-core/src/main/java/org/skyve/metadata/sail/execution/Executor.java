package org.skyve.metadata.sail.execution;

import org.skyve.metadata.sail.language.Automation;
import org.skyve.metadata.sail.language.Interaction;
import org.skyve.metadata.sail.language.step.Comment;
import org.skyve.metadata.sail.language.step.Execute;
import org.skyve.metadata.sail.language.step.Pause;
import org.skyve.metadata.sail.language.step.TestFailure;
import org.skyve.metadata.sail.language.step.TestSuccess;
import org.skyve.metadata.sail.language.step.TestValue;
import org.skyve.metadata.sail.language.step.context.ClearContext;
import org.skyve.metadata.sail.language.step.context.PopContext;
import org.skyve.metadata.sail.language.step.context.PushEditContext;
import org.skyve.metadata.sail.language.step.context.PushListContext;
import org.skyve.metadata.sail.language.step.interaction.DataEnter;
import org.skyve.metadata.sail.language.step.interaction.TabSelect;
import org.skyve.metadata.sail.language.step.interaction.TestDataEnter;
import org.skyve.metadata.sail.language.step.interaction.actions.Action;
import org.skyve.metadata.sail.language.step.interaction.actions.Cancel;
import org.skyve.metadata.sail.language.step.interaction.actions.Delete;
import org.skyve.metadata.sail.language.step.interaction.actions.Ok;
import org.skyve.metadata.sail.language.step.interaction.actions.Remove;
import org.skyve.metadata.sail.language.step.interaction.actions.Save;
import org.skyve.metadata.sail.language.step.interaction.actions.ZoomIn;
import org.skyve.metadata.sail.language.step.interaction.actions.ZoomOut;
import org.skyve.metadata.sail.language.step.interaction.grids.DataGridEdit;
import org.skyve.metadata.sail.language.step.interaction.grids.DataGridNew;
import org.skyve.metadata.sail.language.step.interaction.grids.DataGridRemove;
import org.skyve.metadata.sail.language.step.interaction.grids.DataGridSelect;
import org.skyve.metadata.sail.language.step.interaction.grids.DataGridZoom;
import org.skyve.metadata.sail.language.step.interaction.grids.ListGridNew;
import org.skyve.metadata.sail.language.step.interaction.grids.ListGridSelect;
import org.skyve.metadata.sail.language.step.interaction.grids.ListGridZoom;
import org.skyve.metadata.sail.language.step.interaction.lookup.LookupDescriptionAutoComplete;
import org.skyve.metadata.sail.language.step.interaction.lookup.LookupDescriptionEdit;
import org.skyve.metadata.sail.language.step.interaction.lookup.LookupDescriptionNew;
import org.skyve.metadata.sail.language.step.interaction.lookup.LookupDescriptionPick;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateCalendar;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateEdit;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateLink;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateList;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateMap;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateTree;
import org.skyve.metadata.sail.language.step.interaction.session.Login;
import org.skyve.metadata.sail.language.step.interaction.session.Logout;

/**
 * Visitor over the SAIL language model that drives execution of automation scripts.
 *
 * <p>Each method in this interface corresponds to one concrete {@link org.skyve.metadata.sail.language.Step}
 * subtype. When a step is executed via {@link org.skyve.metadata.sail.language.Executable#execute(Executor)},
 * it calls the matching {@code execute*()} method here, passing itself as the argument.
 *
 * <p>Implementations include browser-driven executors (via Selenium/WebDriver in
 * {@code skyve-web}), mock executors used by the test harness, and code-generation
 * visitors that emit test source. Adding a new executor does not require changing
 * the language model.
 *
 * <p>Threading: executor instances are not thread-safe and must be used from a
 * single thread per execution run.
 *
 * @see org.skyve.metadata.sail.language.Automation
 * @see org.skyve.metadata.sail.language.Interaction
 */
public interface Executor {
	/**
	 * Executes a complete {@link Automation} script, including its before/after procedures
	 * and all contained {@link Interaction} steps.
	 */
	public void executeAutomation(Automation automation);

	/**
	 * Executes a single named {@link Interaction} within an automation run.
	 */
	public void executeInteraction(Interaction interaction);

	/**
	 * Executes executePushListContext.
	 * @param push the push
	 */
	public void executePushListContext(PushListContext push);

	/**
	 * Executes executePushEditContext.
	 * @param push the push
	 */
	public void executePushEditContext(PushEditContext push);

	/**
	 * Executes executePopContext.
	 * @param pop the pop
	 */
	public void executePopContext(PopContext pop);

	/**
	 * Executes executeClearContext.
	 * @param clear the clear
	 */
	public void executeClearContext(ClearContext clear);
	
	/**
	 * Executes executeLogin.
	 * @param login the login
	 */
	public void executeLogin(Login login);

	/**
	 * Executes executeLogout.
	 * @param logout the logout
	 */
	public void executeLogout(Logout logout);

	/**
	 * Executes executeNavigateList.
	 * @param list the list
	 */
	public void executeNavigateList(NavigateList list);

	/**
	 * Executes executeNavigateEdit.
	 * @param edit the edit
	 */
	public void executeNavigateEdit(NavigateEdit edit);

	/**
	 * Executes executeNavigateTree.
	 * @param tree the tree
	 */
	public void executeNavigateTree(NavigateTree tree);

	/**
	 * Executes executeNavigateMap.
	 * @param map the map
	 */
	public void executeNavigateMap(NavigateMap map);

	/**
	 * Executes executeNavigateCalendar.
	 * @param calendar the calendar
	 */
	public void executeNavigateCalendar(NavigateCalendar calendar);

	/**
	 * Executes executeNavigateLink.
	 * @param link the link
	 */
	public void executeNavigateLink(NavigateLink link);
	
	/**
	 * Executes executeTabSelect.
	 * @param tabSelect the tabSelect
	 */
	public void executeTabSelect(TabSelect tabSelect);

	/**
	 * Executes executeTestDataEnter.
	 * @param testDataEnter the testDataEnter
	 */
	public void executeTestDataEnter(TestDataEnter testDataEnter);

	/**
	 * Executes executeDataEnter.
	 * @param dataEnter the dataEnter
	 */
	public void executeDataEnter(DataEnter dataEnter);
	
	/**
	 * Executes executeOk.
	 * @param ok the ok
	 */
	public void executeOk(Ok ok);

	/**
	 * Executes executeSave.
	 * @param save the save
	 */
	public void executeSave(Save save);

	/**
	 * Executes executeCancel.
	 * @param cancel the cancel
	 */
	public void executeCancel(Cancel cancel);

	/**
	 * Executes executeDelete.
	 * @param delete the delete
	 */
	public void executeDelete(Delete delete);

	/**
	 * Executes executeZoomOut.
	 * @param zoomOut the zoomOut
	 */
	public void executeZoomOut(ZoomOut zoomOut);

	/**
	 * Executes executeRemove.
	 * @param remove the remove
	 */
	public void executeRemove(Remove remove);

	/**
	 * Executes executeAction.
	 * @param action the action
	 */
	public void executeAction(Action action);

	/**
	 * Executes executeLookupDescriptionAutoComplete.
	 * @param complete the complete
	 */
	public void executeLookupDescriptionAutoComplete(LookupDescriptionAutoComplete complete);

	/**
	 * Executes executeLookupDescriptionPick.
	 * @param pick the pick
	 */
	public void executeLookupDescriptionPick(LookupDescriptionPick pick);

	/**
	 * Executes executeLookupDescriptionNew.
	 * @param nu the nu
	 */
	public void executeLookupDescriptionNew(LookupDescriptionNew nu);

	/**
	 * Executes executeLookupDescriptionEdit.
	 * @param edit the edit
	 */
	public void executeLookupDescriptionEdit(LookupDescriptionEdit edit);

	/**
	 * Executes executeZoomIn.
	 * @param zoom the zoom
	 */
	public void executeZoomIn(ZoomIn zoom);
	
	/**
	 * Executes executeDataGridNew.
	 * @param nu the nu
	 */
	public void executeDataGridNew(DataGridNew nu);

	/**
	 * Executes executeDataGridZoom.
	 * @param zoom the zoom
	 */
	public void executeDataGridZoom(DataGridZoom zoom);

	/**
	 * Executes executeDataGridEdit.
	 * @param edit the edit
	 */
	public void executeDataGridEdit(DataGridEdit edit);

	/**
	 * Executes executeDataGridRemove.
	 * @param remove the remove
	 */
	public void executeDataGridRemove(DataGridRemove remove);

	/**
	 * Executes executeDataGridSelect.
	 * @param select the select
	 */
	public void executeDataGridSelect(DataGridSelect select);
	
	/**
	 * Executes executeListGridNew.
	 * @param nu the nu
	 */
	public void executeListGridNew(ListGridNew nu);

	/**
	 * Executes executeListGridZoom.
	 * @param zoom the zoom
	 */
	public void executeListGridZoom(ListGridZoom zoom);

	/**
	 * Executes executeListGridSelect.
	 * @param select the select
	 */
	public void executeListGridSelect(ListGridSelect select);

	/**
	 * Executes executeTestValue.
	 * @param test the test
	 */
	public void executeTestValue(TestValue test);

	/**
	 * Executes executeTestSuccess.
	 * @param test the test
	 */
	public void executeTestSuccess(TestSuccess test);

	/**
	 * Executes executeTestFailure.
	 * @param test the test
	 */
	public void executeTestFailure(TestFailure test);

	/**
	 * Executes executeExecute.
	 * @param execute the execute
	 */
	public void executeExecute(Execute execute);

	/**
	 * Executes executeComment.
	 * @param comment the comment
	 */
	public void executeComment(Comment comment);

	/**
	 * Executes executePause.
	 * @param pause the pause
	 */
	public void executePause(Pause pause);
}
