package org.skyve.metadata.sail.execution;

import org.skyve.metadata.sail.language.Interaction;
import org.skyve.metadata.sail.language.Automation;
import org.skyve.metadata.sail.language.step.Execute;
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
import org.skyve.metadata.user.User;

public interface Executor {
	public User getUser();
	public void setUser(User user);
	
	public void executeAutomation(Automation automation);
	public void executeInteraction(Interaction interaction);

	public void executePushListContext(PushListContext push);
	public void executePushEditContext(PushEditContext push);
	public void executePopContext(PopContext pop);
	public void executeClearContext(ClearContext clear);
	
	public void executeNavigateList(NavigateList list);
	public void executeNavigateEdit(NavigateEdit edit);
	public void executeNavigateTree(NavigateTree tree);
	public void executeNavigateMap(NavigateMap map);
	public void executeNavigateCalendar(NavigateCalendar calendar);
	public void executeNavigateLink(NavigateLink link);
	
	public void executeTabSelect(TabSelect tabSelect);
	public void executeTestDataEnter(TestDataEnter testDataEnter);
	public void executeDataEnter(DataEnter dataEnter);
	
	public void executeOk(Ok ok);
	public void executeSave(Save save);
	public void executeCancel(Cancel cancel);
	public void executeDelete(Delete delete);
	public void executeZoomOut(ZoomOut zoomOut);
	public void executeRemove(Remove remove);
	public void executeAction(Action action);

	public void executeLookupDescriptionAutoComplete(LookupDescriptionAutoComplete complete);
	public void executeLookupDescriptionPick(LookupDescriptionPick pick);
	public void executeLookupDescriptionNew(LookupDescriptionNew nu);
	public void executeLookupDescriptionEdit(LookupDescriptionEdit edit);

	public void executeDataGridNew(DataGridNew nu);
	public void executeDataGridZoom(DataGridZoom zoom);
	public void executeDataGridEdit(DataGridEdit edit);
	public void executeDataGridRemove(DataGridRemove remove);
	public void executeDataGridSelect(DataGridSelect select);
	
	public void executeListGridNew(ListGridNew nu);
	public void executeListGridZoom(ListGridZoom zoom);
	public void executeListGridSelect(ListGridSelect select);

	public void executeTestValue(TestValue test);
	public void executeTestSuccess(TestSuccess test);
	public void executeTestFailure(TestFailure test);
	public void executeExecute(Execute execute);
}
