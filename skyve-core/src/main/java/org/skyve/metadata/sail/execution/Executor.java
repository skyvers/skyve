package org.skyve.metadata.sail.execution;

import org.skyve.metadata.sail.language.Automation;
import org.skyve.metadata.sail.language.Interaction;
import org.skyve.metadata.sail.language.step.CaptureCell;
import org.skyve.metadata.sail.language.step.CaptureTitle;
import org.skyve.metadata.sail.language.step.CaptureValue;
import org.skyve.metadata.sail.language.step.Comment;
import org.skyve.metadata.sail.language.step.Execute;
import org.skyve.metadata.sail.language.step.Pause;
import org.skyve.metadata.sail.language.step.TestFailure;
import org.skyve.metadata.sail.language.step.TestGridRows;
import org.skyve.metadata.sail.language.step.TestMessage;
import org.skyve.metadata.sail.language.step.TestSuccess;
import org.skyve.metadata.sail.language.step.TestValue;
import org.skyve.metadata.sail.language.step.TestWidget;
import org.skyve.metadata.sail.language.step.context.ClearContext;
import org.skyve.metadata.sail.language.step.context.PopContext;
import org.skyve.metadata.sail.language.step.context.PushEditContext;
import org.skyve.metadata.sail.language.step.context.PushListContext;
import org.skyve.metadata.sail.language.step.interaction.DataEnter;
import org.skyve.metadata.sail.language.step.interaction.TabSelect;
import org.skyve.metadata.sail.language.step.interaction.TestDataEnter;
import org.skyve.metadata.sail.language.step.interaction.Upload;
import org.skyve.metadata.sail.language.step.interaction.actions.Action;
import org.skyve.metadata.sail.language.step.interaction.actions.Cancel;
import org.skyve.metadata.sail.language.step.interaction.actions.Delete;
import org.skyve.metadata.sail.language.step.interaction.actions.Ok;
import org.skyve.metadata.sail.language.step.interaction.actions.Remove;
import org.skyve.metadata.sail.language.step.interaction.actions.Save;
import org.skyve.metadata.sail.language.step.interaction.actions.ZoomIn;
import org.skyve.metadata.sail.language.step.interaction.actions.ZoomOut;
import org.skyve.metadata.sail.language.step.interaction.grids.DataGridEdit;
import org.skyve.metadata.sail.language.step.interaction.grids.DataGridLink;
import org.skyve.metadata.sail.language.step.interaction.grids.DataGridMove;
import org.skyve.metadata.sail.language.step.interaction.grids.DataGridNew;
import org.skyve.metadata.sail.language.step.interaction.grids.DataGridRemove;
import org.skyve.metadata.sail.language.step.interaction.grids.DataGridSelect;
import org.skyve.metadata.sail.language.step.interaction.grids.DataGridZoom;
import org.skyve.metadata.sail.language.step.interaction.grids.ListGridClearFilter;
import org.skyve.metadata.sail.language.step.interaction.grids.ListGridFilter;
import org.skyve.metadata.sail.language.step.interaction.grids.ListGridNew;
import org.skyve.metadata.sail.language.step.interaction.grids.ListGridSelect;
import org.skyve.metadata.sail.language.step.interaction.grids.ListGridSort;
import org.skyve.metadata.sail.language.step.interaction.grids.ListGridTool;
import org.skyve.metadata.sail.language.step.interaction.grids.ListGridZoom;
import org.skyve.metadata.sail.language.step.interaction.grids.ListMembershipMove;
import org.skyve.metadata.sail.language.step.interaction.lookup.LookupDescriptionAutoComplete;
import org.skyve.metadata.sail.language.step.interaction.lookup.LookupDescriptionEdit;
import org.skyve.metadata.sail.language.step.interaction.lookup.LookupDescriptionNew;
import org.skyve.metadata.sail.language.step.interaction.lookup.LookupDescriptionPick;
import org.skyve.metadata.sail.language.step.interaction.navigation.CloseWindow;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateBack;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateCalendar;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateEdit;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateLink;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateList;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateMap;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateMenu;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateTree;
import org.skyve.metadata.sail.language.step.interaction.navigation.Refresh;
import org.skyve.metadata.sail.language.step.interaction.navigation.TestMenu;
import org.skyve.metadata.sail.language.step.interaction.session.Login;
import org.skyve.metadata.sail.language.step.interaction.session.Logout;
import org.skyve.metadata.sail.language.step.interaction.session.SwitchMode;

import jakarta.annotation.Nonnull;

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
 * <p>Tokens: executors that extend {@code org.skyve.impl.sail.execution.ContextualExecutor}
 * must pass every value-like attribute through {@code substituteTokens(String)} before
 * using it, so that {@code {run}} in a script resolves to the run token and {@code {var:name}}
 * to a value captured by {@link #executeCaptureValue(CaptureValue)}. The attributes are
 * {@code dataEnter@value}, {@code testValue@value}, {@code lookupDescriptionAutoComplete@search},
 * {@code lookupDescriptionAutoComplete@pick}, {@code listGridFilter@value}, {@code login@user},
 * {@code login@password}, {@code login@customer}, {@code testFailure@message}, {@code testMessage@contains},
 * {@code navigateLink@href}, {@code navigateLink@text}, {@code dataGridLink@text}, {@code navigateEdit@bizId},
 * {@code comment} text and every {@code rowContaining}.
 *
 * @see org.skyve.metadata.sail.language.Automation
 * @see org.skyve.metadata.sail.language.Interaction
 */
public interface Executor {
	/**
	 * Executes a complete {@link Automation} script, including its before/after procedures
	 * and all contained {@link Interaction} steps.
	 */
	public void executeAutomation(@Nonnull Automation automation);

	/**
	 * Executes a single named {@link Interaction} within an automation run.
	 */
	public void executeInteraction(@Nonnull Interaction interaction);

	/**
	 * Executes executePushListContext.
	 * @param push the push
	 */
	public void executePushListContext(@Nonnull PushListContext push);

	/**
	 * Executes executePushEditContext.
	 * @param push the push
	 */
	public void executePushEditContext(@Nonnull PushEditContext push);

	/**
	 * Executes executePopContext.
	 * @param pop the pop
	 */
	public void executePopContext(@Nonnull PopContext pop);

	/**
	 * Executes executeClearContext.
	 * @param clear the clear
	 */
	public void executeClearContext(@Nonnull ClearContext clear);
	
	/**
	 * Executes executeLogin.
	 * @param login the login
	 */
	public void executeLogin(@Nonnull Login login);

	/**
	 * Executes executeLogout.
	 * @param logout the logout
	 */
	public void executeLogout(@Nonnull Logout logout);

	/**
	 * Executes executeNavigateList.
	 * @param list the list
	 */
	public void executeNavigateList(@Nonnull NavigateList list);

	/**
	 * Executes executeNavigateEdit.
	 * @param edit the edit
	 */
	public void executeNavigateEdit(@Nonnull NavigateEdit edit);

	/**
	 * Executes executeNavigateTree.
	 * @param tree the tree
	 */
	public void executeNavigateTree(@Nonnull NavigateTree tree);

	/**
	 * Executes executeNavigateMap.
	 * @param map the map
	 */
	public void executeNavigateMap(@Nonnull NavigateMap map);

	/**
	 * Executes executeNavigateCalendar.
	 * @param calendar the calendar
	 */
	public void executeNavigateCalendar(@Nonnull NavigateCalendar calendar);

	/**
	 * Executes executeNavigateLink.
	 * @param link the link
	 */
	public void executeNavigateLink(@Nonnull NavigateLink link);
	
	/**
	 * Executes executeTabSelect.
	 * @param tabSelect the tabSelect
	 */
	public void executeTabSelect(@Nonnull TabSelect tabSelect);

	/**
	 * Executes executeTestDataEnter.
	 * @param testDataEnter the testDataEnter
	 */
	public void executeTestDataEnter(@Nonnull TestDataEnter testDataEnter);

	/**
	 * Executes executeDataEnter.
	 * @param dataEnter the dataEnter
	 */
	public void executeDataEnter(@Nonnull DataEnter dataEnter);
	
	/**
	 * Executes executeOk.
	 * @param ok the ok
	 */
	public void executeOk(@Nonnull Ok ok);

	/**
	 * Executes executeSave.
	 * @param save the save
	 */
	public void executeSave(@Nonnull Save save);

	/**
	 * Executes executeCancel.
	 * @param cancel the cancel
	 */
	public void executeCancel(@Nonnull Cancel cancel);

	/**
	 * Executes executeDelete.
	 * @param delete the delete
	 */
	public void executeDelete(@Nonnull Delete delete);

	/**
	 * Executes executeZoomOut.
	 * @param zoomOut the zoomOut
	 */
	public void executeZoomOut(@Nonnull ZoomOut zoomOut);

	/**
	 * Executes executeRemove.
	 * @param remove the remove
	 */
	public void executeRemove(@Nonnull Remove remove);

	/**
	 * Executes executeAction.
	 * @param action the action
	 */
	public void executeAction(@Nonnull Action action);

	/**
	 * Executes executeLookupDescriptionAutoComplete.
	 * @param complete the complete
	 */
	public void executeLookupDescriptionAutoComplete(@Nonnull LookupDescriptionAutoComplete complete);

	/**
	 * Executes executeLookupDescriptionPick.
	 * @param pick the pick
	 */
	public void executeLookupDescriptionPick(@Nonnull LookupDescriptionPick pick);

	/**
	 * Executes executeLookupDescriptionNew.
	 * @param nu the nu
	 */
	public void executeLookupDescriptionNew(@Nonnull LookupDescriptionNew nu);

	/**
	 * Executes executeLookupDescriptionEdit.
	 * @param edit the edit
	 */
	public void executeLookupDescriptionEdit(@Nonnull LookupDescriptionEdit edit);

	/**
	 * Executes executeZoomIn.
	 * @param zoom the zoom
	 */
	public void executeZoomIn(@Nonnull ZoomIn zoom);
	
	/**
	 * Executes executeDataGridNew.
	 * @param nu the nu
	 */
	public void executeDataGridNew(@Nonnull DataGridNew nu);

	/**
	 * Executes executeDataGridZoom.
	 * @param zoom the zoom
	 */
	public void executeDataGridZoom(@Nonnull DataGridZoom zoom);

	/**
	 * Executes executeDataGridEdit.
	 * @param edit the edit
	 */
	public void executeDataGridEdit(@Nonnull DataGridEdit edit);

	/**
	 * Executes executeDataGridRemove.
	 * @param remove the remove
	 */
	public void executeDataGridRemove(@Nonnull DataGridRemove remove);

	/**
	 * Executes executeDataGridSelect.
	 * @param select the select
	 */
	public void executeDataGridSelect(@Nonnull DataGridSelect select);
	
	/**
	 * Executes executeListGridNew.
	 * @param nu the nu
	 */
	public void executeListGridNew(@Nonnull ListGridNew nu);

	/**
	 * Executes executeListGridZoom.
	 * @param zoom the zoom
	 */
	public void executeListGridZoom(@Nonnull ListGridZoom zoom);

	/**
	 * Executes executeListGridSelect.
	 * @param select the select
	 */
	public void executeListGridSelect(@Nonnull ListGridSelect select);

	/**
	 * Executes executeTestValue.
	 * @param test the test
	 */
	public void executeTestValue(@Nonnull TestValue test);

	/**
	 * Executes executeTestSuccess.
	 * @param test the test
	 */
	public void executeTestSuccess(@Nonnull TestSuccess test);

	/**
	 * Executes executeTestFailure.
	 * @param test the test
	 */
	public void executeTestFailure(@Nonnull TestFailure test);

	/**
	 * Executes executeExecute.
	 * @param execute the execute
	 */
	public void executeExecute(@Nonnull Execute execute);

	/**
	 * Executes executeComment.
	 * @param comment the comment
	 */
	public void executeComment(@Nonnull Comment comment);

	/**
	 * Executes executePause.
	 * @param pause the pause
	 */
	public void executePause(@Nonnull Pause pause);

	/**
	 * Uploads a classpath file into a content widget or through an upload action.
	 *
	 * <p>Precondition: exactly one of {@link Upload#getBinding()} and {@link Upload#getActionName()}
	 * is set; {@link Upload#execute(Executor)} checks this before dispatching.
	 *
	 * @param upload the upload step
	 */
	public void executeUpload(@Nonnull Upload upload);

	/**
	 * Asserts that a message containing the given text, and optionally of the given severity, is shown.
	 *
	 * @param test the message assertion step
	 */
	public void executeTestMessage(@Nonnull TestMessage test);

	/**
	 * Asserts the number of rows, optionally of rows containing the given text, in a list or data grid.
	 *
	 * <p>When {@link TestGridRows#getTimeoutMillis()} is set, the grid is refreshed and counted
	 * again until the assertion holds or the timeout elapses. Use
	 * {@link TestGridRows#getEffectiveMinRows()} for the lower bound.
	 *
	 * @param test the grid assertion step
	 */
	public void executeTestGridRows(@Nonnull TestGridRows test);

	/**
	 * Asserts the visibility and enabled state of a widget, action button or tab.
	 *
	 * <p>Use {@link TestWidget#isExpectedVisible()} for the expected visibility.
	 *
	 * @param test the widget assertion step
	 */
	public void executeTestWidget(@Nonnull TestWidget test);

	/**
	 * Navigates by clicking through the rendered menu, then sets the context for the menu item's target.
	 *
	 * @param menu the menu navigation step
	 */
	public void executeNavigateMenu(@Nonnull NavigateMenu menu);

	/**
	 * Asserts whether a module's menu or one of its items is shown to the current user.
	 *
	 * @param test the menu assertion step
	 */
	public void executeTestMenu(@Nonnull TestMenu test);

	/**
	 * Switches the browser session to the automation's renderer (SmartClient for the {@code desktop}
	 * UX/UI, PrimeFaces otherwise) through the user-visible "Switch mode" control.
	 *
	 * <p>Does nothing if the session already shows that renderer; fails if the control is not
	 * available because the user cannot switch modes.
	 *
	 * @param switchMode the switch mode step
	 */
	public void executeSwitchMode(@Nonnull SwitchMode switchMode);

	/**
	 * Filters one column of a list grid, or clears that column's filter when the step has no value.
	 *
	 * <p>Filters on different columns are ANDed. Use {@link ListGridFilter#getEffectiveOperator()} for the
	 * operator; {@link ListGridFilter#execute(Executor)} has already checked that an operator comes with a value.
	 *
	 * @param filter the list grid filter step
	 */
	public void executeListGridFilter(@Nonnull ListGridFilter filter);

	/**
	 * Clears every column filter of a list grid.
	 *
	 * @param clear the list grid clear filter step
	 */
	public void executeListGridClearFilter(@Nonnull ListGridClearFilter clear);

	/**
	 * Sorts a list grid by one column, ascending unless {@link ListGridSort#getDescending()} is {@code true}.
	 *
	 * @param sort the list grid sort step
	 */
	public void executeListGridSort(@Nonnull ListGridSort sort);

	/**
	 * Reads the value a widget shows into a variable that later steps use as {@code {var:name}}.
	 *
	 * <p>Precondition: {@link CaptureValue#execute(Executor)} has checked the variable name.
	 *
	 * @param capture the capture value step
	 */
	public void executeCaptureValue(@Nonnull CaptureValue capture);

	/**
	 * Reads the text a list or data grid cell shows into a variable that later steps use as {@code {var:name}}.
	 *
	 * <p>Precondition: {@link CaptureCell#execute(Executor)} has checked the row, the grid attributes, the variable name
	 * and the pattern.
	 *
	 * @param capture the capture cell step
	 */
	public void executeCaptureCell(@Nonnull CaptureCell capture);

	/**
	 * Reads the title the current view shows into a variable that later steps use as {@code {var:name}}.
	 *
	 * <p>Precondition: {@link CaptureTitle#execute(Executor)} has checked the variable name and the pattern.
	 *
	 * @param capture the capture title step
	 */
	public void executeCaptureTitle(@Nonnull CaptureTitle capture);

	/**
	 * Clicks a link in a row of a data grid: an action's link is pressed as an action step presses its button
	 * (answering confirmations per {@link DataGridLink#getConfirm()} and checking success per
	 * {@link DataGridLink#getTestSuccess()}); any other link is followed as {@code navigateLink} follows one.
	 *
	 * <p>Precondition: {@link DataGridLink#execute(Executor)} has checked the row and the text.
	 *
	 * @param link the data grid link step
	 */
	public void executeDataGridLink(@Nonnull DataGridLink link);

	/**
	 * Moves a row of an ordered data grid to another position with a drag-and-drop gesture, as a user reorders rows.
	 * The context is unchanged.
	 *
	 * <p>Precondition: {@link DataGridMove#execute(Executor)} has checked the row and that {@code to} is 0 or more.
	 *
	 * @param move the data grid move step
	 */
	public void executeDataGridMove(@Nonnull DataGridMove move);

	/**
	 * Moves a member of an ordered list membership to another position in its member list with a drag-and-drop gesture,
	 * as a user reorders members. The context is unchanged.
	 *
	 * <p>Precondition: {@link ListMembershipMove#execute(Executor)} has checked the member and that {@code to} is 0 or
	 * more.
	 *
	 * @param move the list membership move step
	 */
	public void executeListMembershipMove(@Nonnull ListMembershipMove move);

	/**
	 * Goes back to the previous page in the browser's history, as the browser's Back button does, and waits for it.
	 * The top context, if any, loses its document and view type, as after {@link #executeNavigateLink(NavigateLink)}.
	 *
	 * @param back the navigate back step
	 */
	public void executeNavigateBack(@Nonnull NavigateBack back);

	/**
	 * Reloads the page, as the browser's Reload button does, and waits for it. The contexts are unchanged.
	 *
	 * @param refresh the refresh step
	 */
	public void executeRefresh(@Nonnull Refresh refresh);

	/**
	 * Closes the window that a {@code <navigateLink window="new"/>} opened and returns to the window that opened it,
	 * whose contexts are restored as they were; the contexts pushed in the closed window are dropped.
	 *
	 * <p>Fails in the window the automation started in, where there is no window to close.
	 *
	 * @param close the close window step
	 */
	public void executeCloseWindow(@Nonnull CloseWindow close);

	/**
	 * Presses a list grid's toolbar tool, such as Refresh or Deselect. The context is unchanged.
	 *
	 * <p>Precondition: {@link ListGridTool#execute(Executor)} has checked that the step names a tool.
	 *
	 * @param tool the list grid tool step
	 */
	public void executeListGridTool(@Nonnull ListGridTool tool);
}
