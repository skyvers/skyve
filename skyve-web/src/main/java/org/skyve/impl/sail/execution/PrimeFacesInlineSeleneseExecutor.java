package org.skyve.impl.sail.execution;

import java.util.List;

import javax.faces.component.UIComponent;

import org.primefaces.component.inputmask.InputMask;
import org.primefaces.component.inputtext.InputText;
import org.primefaces.component.inputtextarea.InputTextarea;
import org.primefaces.component.selectbooleancheckbox.SelectBooleanCheckbox;
import org.primefaces.component.selectonemenu.SelectOneMenu;
import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.module.ModuleImpl;
import org.skyve.impl.metadata.repository.AbstractRepository;
import org.skyve.impl.metadata.view.ViewImpl;
import org.skyve.impl.web.faces.pipeline.component.ComponentBuilder;
import org.skyve.impl.web.faces.pipeline.layout.LayoutBuilder;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Collection;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.DocumentQueryDefinition;
import org.skyve.metadata.sail.language.Step;
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
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.View.ViewType;
import org.skyve.util.Binder.TargetMetaData;
import org.skyve.util.Util;

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
			if (q == null) {
				q = m.getDocumentDefaultQuery(c, documentName);
			}
			m = q.getOwningModule();
			newContext.setModuleName(m.getName());
			newContext.setDocumentName(q.getDocumentName());
		}
		else if (documentName != null) {
			Document d = m.getDocument(c, documentName);
			if (modelName != null) {
				d = CORE.getRepository().getListModel(c, d, modelName, false).getDrivingDocument();
			}
			else {
				push.setQueryName(documentName);
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
		else {
			newContext.setViewType(ViewType.edit);
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
			command("open", String.format("?a=l&m=%s&q=%s", moduleName, queryName));
		}
		else if (documentName != null) {
			if (modelName != null) {
				command("open", String.format("?a=l&m=%s&d=%s&q=%s", moduleName, documentName, modelName));
			}
			else {
				command("open", String.format("?a=l&m=%s&q=%s", moduleName, documentName));
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
		// TODO Not quite right as we are looking for the first link with the tab name/label.
		for (String tabName : tabSelect.getTabPath().split("/")) {
			command("click", String.format("link=%s", tabName));
		}
	}

	@Override
	public void execute(TestDataEnter testDataEnter) {
		PrimeFacesAutomationContext context = peek();
		String moduleName = context.getModuleName();
		String documentName = context.getDocumentName();
		User u = CORE.getUser();
		Customer c = u.getCustomer();
		Module m = c.getModule(moduleName);
		Document d = m.getDocument(c, documentName);
		AbstractRepository r = (AbstractRepository) CORE.getRepository();
		Class<?> factoryClass = null;
		try {
			factoryClass = r.getJavaClass(null, String.format("modules.%s.util.%sFactoryExtension", moduleName, documentName));
		}
		catch (MetaDataException e) {
			factoryClass = r.getJavaClass(null, String.format("modules.%s.util.%sFactory", moduleName, documentName));
		}
		Bean bean = null;
		try {
			if (factoryClass == null) {
				bean = Util.constructRandomInstance(u, m, d, 1);
			}
			else {
				// Should be the interface but its in the skyve-ee test package
				Object factory = factoryClass.newInstance();
				bean = (Bean) factoryClass.getMethod("getInstance").invoke(factory);
			}
		}
		catch (Exception e) {
			throw new MetaDataException(String.format("Could not create a random instance of %s.%s", moduleName, documentName) , e);
		}
		
        ViewImpl view = (ViewImpl) r.getView(context.getUxui(), c, d, context.getViewType().toString());
		TestDataEnterViewVisitor visitor = new TestDataEnterViewVisitor((CustomerImpl) c,
																			(ModuleImpl) m,
																			(DocumentImpl) d,
																			view,
																			bean);
		visitor.visit();
		
		for (Step steps : visitor.getScalarSteps()) {
			steps.execute(this);
		}
	}

	@Override
	public void execute(DataEnter dataEnter) {
// TODO tab clicks to get on the right tab
		PrimeFacesAutomationContext context = peek();
		String identifier = dataEnter.getIdentifier(context);
		List<UIComponent> components = context.getFacesComponents(identifier);
		if (components == null) {
			throw new MetaDataException("<DataEnter /> with binding [" + identifier + "] is not valid or is not on the view.");
		}
		for (UIComponent component : components) {
			String clientId = ComponentCollector.clientId(component);
System.out.println(component);
			boolean text = (component instanceof InputText) || (component instanceof InputTextarea);
			boolean selectOne = (component instanceof SelectOneMenu);
			boolean masked = (component instanceof InputMask);
			boolean checkbox = (component instanceof SelectBooleanCheckbox);
			
			// if exists and is not disabled
			comment(String.format("set %s (%s) if it exists and is not disabled", identifier, clientId));
			command("storeElementPresent", clientId, "present");
			command("if", "${present} == true");
			if (checkbox) {
				// dont need to check if checkbox is disabled coz we can still try to click it
				// check the value and only click if we need the other different value
				command("storeEval", String.format("window.SKYVE.getCheckboxValue('%s')", clientId), "checked");
				command("if", String.format("${checked} != %s", dataEnter.getValue()));
				command("click", String.format("%s_input", clientId));
				command("endIf");
			}
			else if (selectOne) {
				// Look for prime faces disabled style
				command("storeCssCount", String.format("css=#%s.ui-state-disabled", clientId), "disabled");
				command("if", "${disabled} == false");
			}
			else {
				// determine editable as these are <input/>
				command("storeEditable", clientId, "editable");
				command("if", "${editable} == true");
			}
			
			if (text) {
				command("type", clientId, dataEnter.getValue());
			}
			else if (masked) { // need to send key strokes to masked fields
				command("click", clientId); // the click selects the existing expression for overtype
				command("sendKeys", clientId, dataEnter.getValue());
			}
			else if (selectOne) {
				command("click", String.format("%s_label", clientId));
				// Value here should be an index in the drop down starting from 0
				command("click", String.format("%s_%s", clientId, dataEnter.getValue()));
			}
			if (! checkbox) { // endIf for disabled/editable test
				command("endIf");
			}
			command("endIf"); // endIf for present test
		}
	}

	private void button(Step button, String tagName, boolean ajax, boolean confirm) {
		PrimeFacesAutomationContext context = peek();
		String identifier = button.getIdentifier(context);
		List<UIComponent> components = context.getFacesComponents(identifier);
		if (components == null) {
			throw new MetaDataException(String.format("<%s /> is not on the view.", tagName));
		}
		for (UIComponent component : components) {
			String clientId = ComponentCollector.clientId(component);

			// if exists and is not disabled
			comment(String.format("click [%s] (%s) if it exists and is not disabled", tagName, clientId));
			command("storeElementPresent", clientId, "present");
			command("if", "${present} == true");
			// Look for prime faces disabled style
			command("storeCssCount", String.format("css=#%s.ui-state-disabled", clientId), "disabled");
			command("if", "${disabled} == false");
			if (ajax) {
				command("click", clientId);
				if (confirm) {
					command("click", "confirmOK");
				}
				command("waitForNotVisible", "ajaxStatus");
			}
			else {
				if (confirm) {
					command("click", clientId);
					command("clickAndWait", "confirmOK");
				}
				else {
					command("clickAndWait", clientId);
				}
			}
			command("endIf");
			command("endIf");
		}
		
	}
	
	@Override
	public void execute(Ok ok) {
		button(ok, "ok", false, false);
		pop();
	}

	@Override
	public void execute(Save save) {
		button(save, "save", true, false);
	}

	@Override
	public void execute(Cancel cancel) {
		button(cancel, "cancel", false, false);
		pop();
	}

	@Override
	public void execute(Delete delete) {
		button(delete, "delete", false, true);
		pop();
	}

	@Override
	public void execute(ZoomOut zoomOut) {
		button(zoomOut, "zoom out", false, false);
		pop();
	}

	@Override
	public void execute(Remove remove) {
		button(remove, "remove", false, true);
		pop();
	}

	@Override
	public void execute(Action action) {
		button(action, action.getActionName(), true, Boolean.TRUE.equals(action.getConfirm()));
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
		gridNewZoom(nu, nu.getBinding(), null);
	}
	
	@Override
	public void execute(DataGridZoom zoom) {
		gridNewZoom(zoom, zoom.getBinding(), zoom.getRow());
	}
	
	private void gridNewZoom(Step step, String binding, Integer row) {
		PrimeFacesAutomationContext context = peek();
		String buttonIdentifier = step.getIdentifier(context);
		
		List<UIComponent> dataGridComponents = context.getFacesComponents(binding);
		if (dataGridComponents == null) {
			throw new MetaDataException(String.format("<%s /> with binding [%s] is not on the view.",
														(row != null) ? "DataGridZoom" : "DataGridNew",
														binding));
		}
		for (UIComponent dataGridComponent : dataGridComponents) {
			String dataGridClientId = ComponentCollector.clientId(dataGridComponent);
			if (row != null) {
				comment(String.format("Zoom on row %d on data grid [%s] (%s)", row, binding, dataGridClientId));
			}
			else {
				comment(String.format("New row on data grid [%s] (%s)", binding, dataGridClientId));
			}
			List<UIComponent> buttonComponents = context.getFacesComponents(buttonIdentifier);
			for (UIComponent buttonComponent : buttonComponents) {
				String buttonClientId = (row != null) ?
											ComponentCollector.clientId(buttonComponent, row) :
											ComponentCollector.clientId(buttonComponent);
				if (buttonClientId.startsWith(dataGridClientId)) {
					// data grid is present
					command("storeElementPresent", dataGridClientId, "present");
					command("if", "${present} == true");
					// data grid button is present
					command("storeElementPresent", buttonClientId, "present");
					command("if", "${present} == true");
					// Look for prime faces disabled style on data grid button
					command("storeCssCount", String.format("css=#%s.ui-state-disabled", buttonClientId), "disabled");
					command("if", "${disabled} == false");
					// All good, continue with the button click
					command("clickAndWait", buttonClientId);
					command("endIf");
					command("endIf");
					command("endIf");
				}
			}
		}

		// Determine the Document of the edit view to push
		Customer c = CORE.getUser().getCustomer();
		Module m = c.getModule(context.getModuleName());
		Document d = m.getDocument(c, context.getDocumentName());
		TargetMetaData target = BindUtil.getMetaDataForBinding(c, m, d, binding);
		String newDocumentName = ((Collection) target.getAttribute()).getDocumentName();
		d = m.getDocument(c, newDocumentName);
		String newModuleName = d.getOwningModuleName();
		
		// Push it
		PushEditContext push = new PushEditContext();
		push.setModuleName(newModuleName);
		push.setDocumentName(newDocumentName);
		push.execute(this);
	}

	@Override
	public void execute(DataGridEdit edit) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void execute(DataGridRemove remove) {
		PrimeFacesAutomationContext context = peek();
		UIComponent component = context.getFacesComponents(remove.getIdentifier(context)).get(0);
// TODO loop over components and check for visible grid, visible new button and disabled new button
		command("click", ComponentCollector.clientId(component, remove.getRow()));
		command("waitForNotVisible", "ajaxStatus");
	}

	@Override
	public void execute(DataGridSelect select) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void execute(ListGridNew nu) {
		PrimeFacesAutomationContext context = peek();
		if (ViewType.list.equals(context.getViewType())) {
			String identifier = nu.getIdentifier(context);
			UIComponent component = context.getFacesComponents(identifier).get(0);
			String clientId = ComponentCollector.clientId(component);
// TODO check for visible and disabled
			comment(String.format("New row on list grid [%s] (%s)", identifier, clientId));
			command("clickAndWait", clientId);
		}
// TODO embedded list grid from an edit view, grid visible, grid new button visible and enabled
		
		PushEditContext push = new PushEditContext();
		push.setModuleName(context.getModuleName());
		push.setDocumentName(context.getDocumentName());
		push.execute(this);
	}

	@Override
	public void execute(ListGridZoom zoom) {
		PrimeFacesAutomationContext context = peek();
		if (ViewType.list.equals(context.getViewType())) {
			String identifier = zoom.getIdentifier(context);
			UIComponent component = context.getFacesComponents(identifier).get(0);
			String clientId = ComponentCollector.clientId(component, zoom.getRow());
// TODO check for visible and disabled
			comment(String.format("Zoom on row %d on data grid [%s] (%s)", zoom.getRow(), identifier, clientId));
			command("clickAndWait", clientId);
		}
// TODO embedded list grid from an edit view, grid visible, grid new button visible and enabled

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
// TODO check for visible and disabled
			command("clickAndWait", String.format("//tr[%d]/td", select.getRow())); // ClientIdCollector.clientId(component, select.getRow()));
		}
// TODO embedded list grid from an edit view, grid visible, grid new button visible and enabled

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
