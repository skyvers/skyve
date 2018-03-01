package org.skyve.impl.tools.test.sail.execution;

import org.skyve.impl.tools.test.sail.language.TestCase;
import org.skyve.impl.tools.test.sail.language.TestSuite;
import org.skyve.impl.tools.test.sail.language.step.Execute;
import org.skyve.impl.tools.test.sail.language.step.Test;
import org.skyve.impl.tools.test.sail.language.step.interaction.DataEnter;
import org.skyve.impl.tools.test.sail.language.step.interaction.TabSelect;
import org.skyve.impl.tools.test.sail.language.step.interaction.TestDataEnter;
import org.skyve.impl.tools.test.sail.language.step.interaction.actions.Action;
import org.skyve.impl.tools.test.sail.language.step.interaction.actions.Cancel;
import org.skyve.impl.tools.test.sail.language.step.interaction.actions.Delete;
import org.skyve.impl.tools.test.sail.language.step.interaction.actions.Ok;
import org.skyve.impl.tools.test.sail.language.step.interaction.actions.Remove;
import org.skyve.impl.tools.test.sail.language.step.interaction.actions.Save;
import org.skyve.impl.tools.test.sail.language.step.interaction.actions.ZoomOut;
import org.skyve.impl.tools.test.sail.language.step.interaction.grids.DataGridEdit;
import org.skyve.impl.tools.test.sail.language.step.interaction.grids.DataGridNew;
import org.skyve.impl.tools.test.sail.language.step.interaction.grids.DataGridRemove;
import org.skyve.impl.tools.test.sail.language.step.interaction.grids.DataGridSelect;
import org.skyve.impl.tools.test.sail.language.step.interaction.grids.DataGridZoom;
import org.skyve.impl.tools.test.sail.language.step.interaction.grids.ListGridSelect;
import org.skyve.impl.tools.test.sail.language.step.interaction.grids.ListGridZoom;
import org.skyve.impl.tools.test.sail.language.step.interaction.lookup.LookupDescriptionAutoComplete;
import org.skyve.impl.tools.test.sail.language.step.interaction.lookup.LookupDescriptionEdit;
import org.skyve.impl.tools.test.sail.language.step.interaction.lookup.LookupDescriptionNew;
import org.skyve.impl.tools.test.sail.language.step.interaction.lookup.LookupDescriptionPick;
import org.skyve.impl.tools.test.sail.language.step.interaction.navigation.NavigateCalendar;
import org.skyve.impl.tools.test.sail.language.step.interaction.navigation.NavigateEdit;
import org.skyve.impl.tools.test.sail.language.step.interaction.navigation.NavigateLink;
import org.skyve.impl.tools.test.sail.language.step.interaction.navigation.NavigateList;
import org.skyve.impl.tools.test.sail.language.step.interaction.navigation.NavigateMap;
import org.skyve.impl.tools.test.sail.language.step.interaction.navigation.NavigateMenu;
import org.skyve.impl.tools.test.sail.language.step.interaction.navigation.NavigateTree;
import org.skyve.metadata.model.document.Document;

public interface Executor {
	public void execute(TestSuite testSuite);
	public void execute(TestCase testCase);

	public Document execute(NavigateMenu menu);
	public Document execute(NavigateList list);
	public Document execute(NavigateEdit edit);
	public Document execute(NavigateTree tree);
	public Document execute(NavigateMap map);
	public Document execute(NavigateCalendar calendar);
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
	
	public void execute(ListGridZoom zoom);
	public void execute(ListGridSelect select);

	public void execute(Test test);
	public void execute(Execute execute);
}
