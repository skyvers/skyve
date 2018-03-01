package org.skyve.impl.tools.test.sail.execution;

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
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.user.User;

public class PrimeFacesInlineSeleneseExecutor extends InlineSeleneseExecutor {

	public PrimeFacesInlineSeleneseExecutor(User user) {
		super(user);
	}

	@Override
	public Document execute(NavigateMenu menu) {
		// TODO Auto-generated method stub
		return super.execute(menu);
	}

	@Override
	public Document execute(NavigateList list) {
		String documentName = list.getDocumentName();
		String queryName = list.getQueryName();
		String modelName = list.getModelName();

		if (queryName != null) {
			command("open", String.format("/?a=l&m=%s&q=%s", list.getModuleName(), queryName));
		}
		else if (documentName != null) {
			if (modelName != null) {
				command("open", String.format(".?a=l&m=%s&d=%s&q=%s", list.getModuleName(), documentName, modelName));
			}
			else {
				command("open", String.format(".?a=l&m=%s&q=%s", list.getModuleName(), documentName));
			}
		}
		else {
			throw new MetaDataException("NavigateList must have module and one of (query, document, document & mode)l");
		}
		
		return super.execute(list);
	}

	@Override
	public Document execute(NavigateEdit edit) {
		String bizId = edit.getBizId();
		if (bizId == null) {
			command("open", String.format(".?a=e&m=%s&d=%s", edit.getModuleName(), edit.getDocumentName()));
		}
		else {
			command("open", String.format(".?a=e&m=%s&d=%s&i=%s",
											edit.getModuleName(),
											edit.getDocumentName(),
											bizId));
		}
		
		return super.execute(edit);
	}

	@Override
	public Document execute(NavigateTree tree) {
		// TODO Auto-generated method stub
		return super.execute(tree);
	}

	@Override
	public Document execute(NavigateMap map) {
		// TODO Auto-generated method stub
		return super.execute(map);
	}

	@Override
	public Document execute(NavigateCalendar calendar) {
		// TODO Auto-generated method stub
		return super.execute(calendar);
	}

	@Override
	public void execute(NavigateLink link) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void execute(TabSelect tabSelect) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void execute(TestDataEnter testDataEnter) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void execute(DataEnter dataEnter) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void execute(Ok ok) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void execute(Save save) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void execute(Cancel cancel) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void execute(Delete delete) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void execute(ZoomOut zoomOut) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void execute(Remove remove) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void execute(Action action) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void execute(LookupDescriptionAutoComplete complete) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void execute(LookupDescriptionPick pick) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void execute(LookupDescriptionNew nu) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void execute(LookupDescriptionEdit edit) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void execute(DataGridNew nu) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void execute(DataGridZoom zoom) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void execute(DataGridEdit edit) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void execute(DataGridRemove remove) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void execute(DataGridSelect select) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void execute(ListGridZoom zoom) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void execute(ListGridSelect select) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void execute(Test test) {
		// TODO Auto-generated method stub
		
	}
}
