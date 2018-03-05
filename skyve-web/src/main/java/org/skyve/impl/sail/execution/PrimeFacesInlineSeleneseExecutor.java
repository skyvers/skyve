package org.skyve.impl.sail.execution;

import org.skyve.metadata.sail.language.step.Test;
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

import javax.faces.component.UIComponent;

import org.skyve.impl.web.UserAgent.UserAgentType;
import org.skyve.impl.web.faces.components.ListGrid;
import org.skyve.impl.web.faces.pipeline.component.ComponentBuilder;
import org.skyve.impl.web.faces.pipeline.component.ComponentBuilderChain;
import org.skyve.metadata.MetaDataException;

public class PrimeFacesInlineSeleneseExecutor extends InlineSeleneseExecutor {
	private ComponentBuilder componentBuilder;
	private UIComponent currentComponent;
	private ClientIdCollector currentCollector;
	
	public PrimeFacesInlineSeleneseExecutor(ComponentBuilder componentBuilder) {
		this.componentBuilder = componentBuilder;
	}

	@Override
	public void execute(NavigateMenu menu) {
		super.execute(menu); // determine driving document
		// TODO Auto-generated method stub
	}

	@Override
	public void execute(NavigateList list) {
		super.execute(list); // determine driving document

		String documentName = list.getDocumentName();
		String queryName = list.getQueryName();
		String modelName = list.getModelName();

		currentCollector = new ClientIdCollector(list);
		ComponentBuilderChain chain = new ComponentBuilderChain(componentBuilder, currentCollector);
		
		currentComponent = ListGrid.generate(list.getModuleName(), 
												documentName, 
												queryName, 
												modelName, 
												Boolean.TRUE,
												false,
												Boolean.TRUE,
												false,
												"skyve",
												UserAgentType.desktop,
												chain);
		
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
	}

	@Override
	public void execute(NavigateEdit edit) {
		super.execute(edit); // determine driving document

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
	}

	@Override
	public void execute(NavigateTree tree) {
		super.execute(tree); // determine driving document
		// TODO Auto-generated method stub
	}

	@Override
	public void execute(NavigateMap map) {
		super.execute(map); // determine driving document
		// TODO Auto-generated method stub
	}

	@Override
	public void execute(NavigateCalendar calendar) {
		super.execute(calendar); // determine driving document
		// TODO Auto-generated method stub
	}

	@Override
	public void execute(NavigateLink link) {
		super.execute(link); // null driving document
		// TODO Auto-generated method stub
	}

	@Override
	public void execute(TabSelect tabSelect) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void execute(TestDataEnter testDataEnter) {
System.out.println(getDrivingDocument());
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
	public void execute(ListGridNew nu) {
		command("clickAndWait", ClientIdCollector.clientId(currentCollector.getFacesComponents(nu.getIdentifier()).get(0)));
	}

	@Override
	public void execute(ListGridZoom zoom) {
		UIComponent component = currentCollector.getFacesComponents(zoom.getIdentifier()).get(0);
		command("clickAndWait", ClientIdCollector.clientId(component, zoom.getRow()));
	}

	@Override
	public void execute(ListGridSelect select) {
		UIComponent component = currentCollector.getFacesComponents(select.getIdentifier()).get(0);
		command("clickAndWait", String.format("//tr[%d]/td", select.getRow())); //ClientIdCollector.clientId(component, select.getRow()));
	}

	@Override
	public void execute(Test test) {
		// TODO Auto-generated method stub
		
	}
}
