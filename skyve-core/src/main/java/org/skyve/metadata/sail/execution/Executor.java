package org.skyve.metadata.sail.execution;

import org.skyve.metadata.sail.language.Interaction;
import org.skyve.metadata.sail.language.Automation;
import org.skyve.metadata.sail.language.step.Execute;
import org.skyve.metadata.sail.language.step.Test;
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
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateMenu;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateTree;

public interface Executor {
	public void execute(Automation automation);
	public void execute(Interaction interaction);

	public void execute(PushListContext push);
	public void execute(PushEditContext push);
	public void execute(PopContext pop);
	public void execute(ClearContext clear);
	
	public void execute(NavigateMenu menu);
	public void execute(NavigateList list);
	public void execute(NavigateEdit edit);
	public void execute(NavigateTree tree);
	public void execute(NavigateMap map);
	public void execute(NavigateCalendar calendar);
	public void execute(NavigateLink link);
	
	public void execute(TabSelect tabSelect);
	public void execute(TestDataEnter testDataEnter);
	public void execute(DataEnter dataEnter);
	
	public void execute(Ok ok);
	public void execute(Save save);
	public void execute(Cancel cancel);
	public void execute(Delete delete);
	public void execute(ZoomOut zoomOut);
	public void execute(Remove remove);
	public void execute(Action action);

	public void execute(LookupDescriptionAutoComplete complete);
	public void execute(LookupDescriptionPick pick);
	public void execute(LookupDescriptionNew nu);
	public void execute(LookupDescriptionEdit edit);

	public void execute(DataGridNew nu);
	public void execute(DataGridZoom zoom);
	public void execute(DataGridEdit edit);
	public void execute(DataGridRemove remove);
	public void execute(DataGridSelect select);
	
	public void execute(ListGridNew nu);
	public void execute(ListGridZoom zoom);
	public void execute(ListGridSelect select);

	public void execute(Test test);
	public void execute(Execute execute);
}
