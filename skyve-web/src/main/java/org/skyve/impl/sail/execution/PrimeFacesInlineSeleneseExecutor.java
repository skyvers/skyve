package org.skyve.impl.sail.execution;

import java.util.List;

import javax.faces.component.UIComponent;

import org.skyve.CORE;
import org.skyve.impl.web.faces.pipeline.component.ComponentBuilder;
import org.skyve.impl.web.faces.pipeline.layout.LayoutBuilder;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.DocumentQueryDefinition;
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
import org.skyve.metadata.view.View.ViewType;

public class PrimeFacesInlineSeleneseExecutor extends InlineSeleneseExecutor<PrimeFacesAutomationContext> {
	private ComponentBuilder componentBuilder;
	private LayoutBuilder layoutBuilder;
	
	public PrimeFacesInlineSeleneseExecutor(ComponentBuilder componentBuilder,
												LayoutBuilder layoutBuilder) {
		this.componentBuilder = componentBuilder;
		this.layoutBuilder = layoutBuilder;
	}

	@Override
	public void execute(PushListContext push) {
		PrimeFacesAutomationContext newContext = new PrimeFacesAutomationContext();
		String moduleName = push.getModuleName();
		Customer c = CORE.getUser().getCustomer();
		Module m = c.getModule(moduleName);
		String documentName = push.getDocumentName();
		String queryName = push.getQueryName();
		String modelName = push.getModelName();
		
		if (queryName != null) {
			DocumentQueryDefinition q = m.getDocumentQuery(queryName);
			m = q.getOwningModule();
			newContext.setModuleName(m.getName());
			newContext.setDocumentName(q.getDocumentName());
		}
		else if (documentName != null) {
			Document d = m.getDocument(c, documentName);
			if (modelName != null) {
				d = CORE.getRepository().getListModel(c, d, modelName, false).getDrivingDocument();
			}
			newContext.setModuleName(d.getOwningModuleName());
			newContext.setDocumentName(d.getName());
		}
		else {
			throw new MetaDataException("NavigateList must have module and one of (query, document, document & mode)l");
		}

		newContext.setViewType(ViewType.list);
		newContext.setUxui(push.getUxui());
		newContext.setUserAgentType(push.getUserAgentType());
		push(newContext);
		newContext.generate(push, componentBuilder);
	}

	@Override
	public void execute(PushEditContext push) {
		PrimeFacesAutomationContext newContext = new PrimeFacesAutomationContext();
		newContext.setModuleName(push.getModuleName());
		newContext.setDocumentName(push.getDocumentName());
		if (Boolean.TRUE.equals(push.getCreate())) {
			newContext.setViewType(ViewType.create);
		}
		newContext.setUxui(push.getUxui());
		newContext.setUserAgentType(push.getUserAgentType());
		push(newContext);
		newContext.generate(push, componentBuilder, layoutBuilder);
	}
	
	@Override
	public void execute(ClearContext clear) {
		clear();
	}
	
	@Override
	public void execute(PopContext pop) {
		pop();
	}
	
	@Override
	public void execute(NavigateMenu menu) {
		super.execute(menu); // determine driving document
		// TODO Auto-generated method stub
	}

	@Override
	public void execute(NavigateList list) {
		String moduleName = list.getModuleName();
		String documentName = list.getDocumentName();
		String queryName = list.getQueryName();
		String modelName = list.getModelName();

		PushListContext push = new PushListContext();
		push.setModuleName(moduleName);
		push.setDocumentName(documentName);
		push.setQueryName(queryName);
		push.setModelName(modelName);
		execute(push);

		if (queryName != null) {
			command("open", String.format("/?a=l&m=%s&q=%s", moduleName, queryName));
		}
		else if (documentName != null) {
			if (modelName != null) {
				command("open", String.format(".?a=l&m=%s&d=%s&q=%s", moduleName, documentName, modelName));
			}
			else {
				command("open", String.format(".?a=l&m=%s&q=%s", moduleName, documentName));
			}
		}
	}

	@Override
	public void execute(NavigateEdit edit) {
		PushEditContext push = new PushEditContext();
		push.setModuleName(edit.getModuleName());
		push.setDocumentName(edit.getDocumentName());
		execute(push);

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
//System.out.println(peek().getModuleName() + '.' + peek().getDocumentName());
		// TODO Auto-generated method stub
		
	}

	@Override
	public void execute(DataEnter dataEnter) {
		PrimeFacesAutomationContext context = peek();
		List<Object> widgets = context.getSkyveWidgets(dataEnter.getIdentifier(context));
		List<UIComponent> components = context.getFacesComponents(dataEnter.getIdentifier(context));
		for (UIComponent component : components) {
			// if exists and is not disabled
			
		}
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
		PrimeFacesAutomationContext context = peek();
		if (ViewType.list.equals(context.getViewType())) {
			UIComponent component = context.getFacesComponents(nu.getIdentifier(context)).get(0);
			command("clickAndWait", ComponentCollector.clientId(component));
		}
		
		PushEditContext push = new PushEditContext();
		push.setModuleName(context.getModuleName());
		push.setDocumentName(context.getDocumentName());
		push.execute(this);
	}

	@Override
	public void execute(ListGridZoom zoom) {
		PrimeFacesAutomationContext context = peek();
		if (ViewType.list.equals(context.getViewType())) {
			UIComponent component = context.getFacesComponents(zoom.getIdentifier(context)).get(0);
			command("clickAndWait", ComponentCollector.clientId(component, zoom.getRow()));
		}

		PushEditContext push = new PushEditContext();
		push.setModuleName(context.getModuleName());
		push.setDocumentName(context.getDocumentName());
		push.execute(this);
	}

	@Override
	public void execute(ListGridSelect select) {
		PrimeFacesAutomationContext context = peek();
		if (ViewType.list.equals(context.getViewType())) {
			UIComponent component = context.getFacesComponents(select.getIdentifier(context)).get(0);
			command("clickAndWait", String.format("//tr[%d]/td", select.getRow())); // ClientIdCollector.clientId(component, select.getRow()));
		}

		// TODO only if there is no select event on the skyve edit view for embedded list grid
		PushEditContext push = new PushEditContext();
		push.setModuleName(context.getModuleName());
		push.setDocumentName(context.getDocumentName());
		push.execute(this);
	}

	@Override
	public void execute(Test test) {
		// TODO Auto-generated method stub
		
	}
}
