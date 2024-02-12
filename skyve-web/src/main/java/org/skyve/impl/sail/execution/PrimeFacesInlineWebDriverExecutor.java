package org.skyve.impl.sail.execution;

import java.util.List;

import org.primefaces.component.colorpicker.ColorPicker;
import org.primefaces.component.datepicker.DatePicker;
import org.primefaces.component.inputmask.InputMask;
import org.primefaces.component.inputtext.InputText;
import org.primefaces.component.inputtextarea.InputTextarea;
import org.primefaces.component.password.Password;
import org.primefaces.component.selectbooleancheckbox.SelectBooleanCheckbox;
import org.primefaces.component.selectonemenu.SelectOneMenu;
import org.primefaces.component.selectoneradio.SelectOneRadio;
import org.primefaces.component.spinner.Spinner;
import org.primefaces.component.tristatecheckbox.TriStateCheckbox;
import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.view.container.TabPane;
import org.skyve.impl.web.faces.pipeline.component.ComponentBuilder;
import org.skyve.impl.web.faces.pipeline.layout.LayoutBuilder;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.Relation;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.Module.DocumentRef;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;
import org.skyve.metadata.sail.language.Automation.TestStrategy;
import org.skyve.metadata.sail.language.Step;
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
import org.skyve.metadata.view.View.ViewType;
import org.skyve.metadata.view.model.list.ListModel;
import org.skyve.util.Binder.TargetMetaData;

import jakarta.faces.component.UIComponent;

public class PrimeFacesInlineWebDriverExecutor extends InlineWebDriverExecutor<PrimeFacesAutomationContext> {
	private ComponentBuilder componentBuilder;
	private LayoutBuilder layoutBuilder;
	
	public PrimeFacesInlineWebDriverExecutor(ComponentBuilder componentBuilder,
												LayoutBuilder layoutBuilder) {
		this.componentBuilder = componentBuilder;
		this.layoutBuilder = layoutBuilder;
	}

	@Override
	public void executePushListContext(PushListContext push) {
		PrimeFacesAutomationContext newContext = ExecutionDelegate.newContext(push);

		push(newContext);
		newContext.generate(push, componentBuilder);
	}

	@Override
	public void executePushEditContext(PushEditContext push) {
		PrimeFacesAutomationContext newContext = ExecutionDelegate.newContext(push);

		push(newContext);
		newContext.generate(push, componentBuilder, layoutBuilder);
	}
	
	@Override
	public void executeClearContext(ClearContext clear) {
		clear();
	}
	
	@Override
	public void executePopContext(PopContext pop) {
		pop();
	}
	
	@Override
	public void executeLogin(Login login) {
		String customer = login.getCustomer();
		String user = login.getUser();
		
		if (customer == null) {
			comment("Login as " + user);
		}
		else {
			comment("Login as " + customer + "/" + user);
		}
		indent().append("login(");
		if (customer == null) {
			append("null, ");
		}
		else {
			append("\"").append(customer).append("\", ");
		}
		append("\"").append(user).append("\", ");
		append("\"").append(login.getPassword()).append("\");").newline();
	}
	
	@Override
	public void executeLogout(Logout logout) {
		comment("Logout");
		indent().append("logout();").newline();
	}
	
	@Override
	public void executeNavigateList(NavigateList list) {
		String moduleName = list.getModuleName();
		String documentName = list.getDocumentName();
		String queryName = list.getQueryName();
		String modelName = list.getModelName();

		PushListContext push = new PushListContext();
		push.setModuleName(moduleName);
		push.setDocumentName(documentName);
		push.setQueryName(queryName);
		push.setModelName(modelName);
		executePushListContext(push);

		if (queryName != null) {
			comment(String.format("List for query [%s.%s]", moduleName, queryName));
			get(String.format("?a=l&m=%s&q=%s", moduleName, queryName));
		}
		else if (documentName != null) {
			if (modelName != null) {
				comment(String.format("List for model [%s.%s.%s]", moduleName, documentName, modelName));
				get(String.format("?a=l&m=%s&d=%s&q=%s", moduleName, documentName, modelName));
			}
			else {
				comment(String.format("List for default query of [%s.%s]", moduleName, documentName));
				get(String.format("?a=l&m=%s&q=%s", moduleName, documentName));
			}
		}
	}

	@Override
	public void executeNavigateEdit(NavigateEdit edit) {
		String moduleName = edit.getModuleName();
		String documentName = edit.getDocumentName();
		
		PushEditContext push = new PushEditContext();
		push.setModuleName(moduleName);
		push.setDocumentName(documentName);
		executePushEditContext(push);

		String bizId = edit.getBizId();
		if (bizId == null) {
			comment(String.format("Edit new document [%s.%s] instance", moduleName, documentName));
			get(String.format("?a=e&m=%s&d=%s", moduleName, documentName));
		}
		else {
			comment(String.format("Edit document [%s.%s] instance with bizId %s", moduleName, documentName, bizId));
			get(String.format("?a=e&m=%s&d=%s&i=%s", moduleName, documentName, bizId));
		}
	}

	@Override
	public void executeNavigateTree(NavigateTree tree) {
		super.executeNavigateTree(tree); // determine driving document
		// TODO Auto-generated method stub
	}

	@Override
	public void executeNavigateMap(NavigateMap map) {
		super.executeNavigateMap(map); // determine driving document
		// TODO Auto-generated method stub
	}

	@Override
	public void executeNavigateCalendar(NavigateCalendar calendar) {
		super.executeNavigateCalendar(calendar); // determine driving document
		// TODO Auto-generated method stub
	}

	@Override
	public void executeNavigateLink(NavigateLink link) {
		super.executeNavigateLink(link); // null driving document
		// TODO Auto-generated method stub
	}

	@Override
	public void executeTabSelect(TabSelect tabSelect) {
		PrimeFacesAutomationContext context = peek();
		String identifier = tabSelect.getIdentifier(context);
		List<UIComponent> components = context.getFacesComponents(identifier);
		if (components == null) {
			throw new MetaDataException("<tabSelect /> with path [" + tabSelect.getTabPath() + "] is not valid or is not on the view.");
		}
		boolean wizard = false;
		List<Object> widgets = context.getSkyveWidgets(identifier);
		for (Object widget : widgets) {
			if (widget instanceof TabPane) {
				wizard = ((TabPane) widget).getProperties().containsKey("wizard");
			}
		}
		for (UIComponent component : components) {
			String clientId = PrimeFacesAutomationContext.clientId(component);
			if (wizard) {
				comment(String.format("click step [%s]", tabSelect.getTabPath()));
				indent().append("step(\"").append(clientId).append("\");").newline();
			}
			else {
				comment(String.format("click tab [%s]", tabSelect.getTabPath()));
				indent().append("tab(\"").append(clientId).append("\");").newline();
			}
		}
	}

	@Override
	public void executeTestDataEnter(TestDataEnter testDataEnter) {
		PrimeFacesAutomationContext context = peek();
		ExecutionDelegate.executeTestDataEnter(testDataEnter, context, this);
	}

	@Override
	public void executeDataEnter(DataEnter dataEnter) {
		PrimeFacesAutomationContext context = peek();
		String identifier = dataEnter.getIdentifier(context);
		List<UIComponent> components = context.getFacesComponents(identifier);
		if (components == null) {
			throw new MetaDataException("<DataEnter /> with binding [" + identifier + "] is not valid or is not on the view.");
		}
		for (UIComponent component : components) {
			String clientId = PrimeFacesAutomationContext.clientId(component);
			boolean text = (component instanceof InputText) || 
								(component instanceof InputTextarea) || 
								(component instanceof Password) ||
								(component instanceof InputMask);
			boolean selectOne = (component instanceof SelectOneMenu);
			boolean radio = (component instanceof SelectOneRadio);
			boolean checkbox = (component instanceof SelectBooleanCheckbox) || (component instanceof TriStateCheckbox);
			boolean _input = (component instanceof Spinner) || (component instanceof DatePicker);
			
			// TODO implement colour picker testing
			if (component instanceof ColorPicker) {
				comment(String.format("Ignore colour picker %s (%s) for now", identifier, clientId));
				return;
			}
			
			// if exists and is not disabled
			comment(String.format("set %s (%s) if it exists and is not disabled", identifier, clientId));
			
			//replace newlines
			String value = dataEnter.getValue();
			if (value != null) {
				// never replace with a new line as chrome under test will trip the default button on the form 
				// from the enter key as represented by \n
				value = value.replace("\n", " ");
			}
			
			if (checkbox) {
				if (Boolean.TRUE.toString().equals(value)) {
					value = "Boolean.TRUE";
				}
				else if (Boolean.FALSE.toString().equals(value)) {
					value = "Boolean.FALSE";
				}
				indent().append("checkbox(\"").append(clientId).append("\", ").append(value).append(");").newline();
			}
			else if (text) {
				if (value != null) {
					value = value.replace("\\", "\\\\").replace("\"", "\\\"");
				}
				indent().append("text(\"").append(clientId).append("\", \"").append(value).append("\");").newline();
			}
			else if (_input) {
				if (value != null) {
					value = value.replace("\\", "\\\\").replace("\"", "\\\"");
				}
				indent().append("_input(\"").append(clientId).append("\", \"").append(value).append("\");").newline();
			}
			else if (selectOne) {
				indent().append("selectOne(\"").append(clientId).append("\", ").append(value).append(");").newline();
			}
			else if (radio) {
				indent().append("radio(\"").append(clientId).append("\", ").append(value).append(");").newline();
			}
		}
	}

	private void button(Step button, String tagName, boolean ajax, boolean confirm, Boolean testSuccess) {
		PrimeFacesAutomationContext context = peek();
		String identifier = button.getIdentifier(context);
		List<UIComponent> components = context.getFacesComponents(identifier);
		if (components == null) {
			throw new MetaDataException(String.format("<%s /> is not on the view.", tagName));
		}
		for (UIComponent component : components) {
			String clientId = PrimeFacesAutomationContext.clientId(component);

			comment(String.format("click [%s] (%s) if it exists and is not disabled", tagName, clientId));
			indent().append("button(\"").append(clientId).append("\", ").append(String.valueOf(ajax));
			append(", ").append(String.valueOf(confirm)).append(");").newline();

			if (! Boolean.FALSE.equals(testSuccess)) { // true or null (defaults on)
				executeTestSuccess(new TestSuccess());
			}
		}
	}

	private void redirectButton(Step button, String tagName, boolean confirm, Boolean testSuccess) {
		PrimeFacesAutomationContext context = peek();
		String identifier = button.getIdentifier(context);
		List<UIComponent> components = context.getFacesComponents(identifier);
		if (components == null) {
			throw new MetaDataException(String.format("<%s /> is not on the view.", tagName));
		}
		for (UIComponent component : components) {
			String clientId = PrimeFacesAutomationContext.clientId(component);

			comment(String.format("click [%s] (%s) if it exists and is not disabled", tagName, clientId));
			indent().append("redirectButton(\"").append(clientId).append("\", ");
			append(String.valueOf(confirm)).append(");").newline();

			if (! Boolean.FALSE.equals(testSuccess)) { // true or null (defaults on)
				executeTestSuccess(new TestSuccess());
			}
		}
	}

	@Override
	public void executeOk(Ok ok) {
		redirectButton(ok, "ok", false, ok.getTestSuccess());
		pop();
	}

	@Override
	public void executeSave(Save save) {
		button(save, "save", true, false, save.getTestSuccess());
		Boolean createView = save.getCreateView(); // NB could be null
		if (Boolean.TRUE.equals(createView)) {
			PrimeFacesAutomationContext context = peek();
			context.setViewType(ViewType.create);
		}
		else if (Boolean.FALSE.equals(createView)) {
			PrimeFacesAutomationContext context = peek();
			context.setViewType(ViewType.edit);
		}
	}

	@Override
	public void executeCancel(Cancel cancel) {
		button(cancel, "cancel", false, false, cancel.getTestSuccess());
		pop();
	}

	@Override
	public void executeDelete(Delete delete) {
		redirectButton(delete, "delete", true, delete.getTestSuccess());
		pop();
	}

	@Override
	public void executeZoomOut(ZoomOut zoomOut) {
		button(zoomOut, "zoom out", false, false, zoomOut.getTestSuccess());
		pop();
	}

	@Override
	public void executeRemove(Remove remove) {
		button(remove, "remove", false, true, remove.getTestSuccess());
		pop();
	}

	@Override
	public void executeAction(Action action) {
		button(action, 
				action.getActionName(),
				true,
				Boolean.TRUE.equals(action.getConfirm()),
				action.getTestSuccess());
	}

	@Override
	public void executeLookupDescriptionAutoComplete(LookupDescriptionAutoComplete complete) {
		lookupDescription(complete, complete.getBinding(), null, complete.getSearch());
	}

	@Override
	public void executeLookupDescriptionPick(LookupDescriptionPick pick) {
		lookupDescription(pick, pick.getBinding(), pick.getRow(), null);
	}

	private void lookupDescription(Step step, String binding, Integer row, String search) {
		PrimeFacesAutomationContext context = peek();
		
		List<UIComponent> lookupComponents = context.getFacesComponents(binding);
		if (lookupComponents == null) {
			throw new MetaDataException(String.format("<%s /> with binding [%s] is not on the view.",
														step.getClass().getSimpleName(),
														binding));
		}
		for (UIComponent lookupComponent : lookupComponents) {
			String clientId = PrimeFacesAutomationContext.clientId(lookupComponent);
			if (row != null) {
				comment(String.format("Pick on row %d on lookup description [%s] (%s)", row, binding, clientId));
				indent().append("lookupDescription(\"").append(clientId).append("\", ").append(String.valueOf(row)).append(");").newline();
			}
			else {
				comment(String.format("Auto complete with search '%s' on lookup description [%s] (%s)", search, binding, clientId));
				indent().append("lookupDescription(\"").append(clientId).append("\", \"").append(search).append("\");").newline();
			}
		}
	}

	@Override
	public void executeLookupDescriptionNew(LookupDescriptionNew nu) {
		// Nothing to do here as PF doesn't allow new off of lookup descriptions
	}

	@Override
	public void executeLookupDescriptionEdit(LookupDescriptionEdit edit) {
		// Nothing to do here as PF doesn't allow edit off of lookup descriptions
	}

	@Override
	public void executeZoomIn(ZoomIn zoom) {
		button(zoom, "zoomIn", false, Boolean.TRUE.equals(zoom.getConfirm()), zoom.getTestSuccess());
		
		// Determine the Document of the edit view to push
		PrimeFacesAutomationContext context = peek();
		String binding = zoom.getBinding();
		Customer c = CORE.getUser().getCustomer();
		Module m = c.getModule(context.getModuleName());
		Document d = m.getDocument(c, context.getDocumentName());
		TargetMetaData target = BindUtil.getMetaDataForBinding(c, m, d, binding);
		Relation relation = (Relation) target.getAttribute();
		if (relation != null) { // should always be
			String newDocumentName = relation.getDocumentName();
			d = m.getDocument(c, newDocumentName);
			String newModuleName = d.getOwningModuleName();
			
			// Push it
			PushEditContext push = new PushEditContext();
			push.setModuleName(newModuleName);
			push.setDocumentName(newDocumentName);
			push.execute(this);
		}
	}
	
	@Override
	public void executeDataGridNew(DataGridNew nu) {
		dataGridGesture(nu, nu.getBinding(), null);
	}
	
	@Override
	public void executeDataGridZoom(DataGridZoom zoom) {
		dataGridGesture(zoom, zoom.getBinding(), zoom.getRow());
	}
	
	@Override
	public void executeDataGridRemove(DataGridRemove remove) {
		dataGridGesture(remove, remove.getBinding(), remove.getRow());
	}

	@Override
	public void executeDataGridSelect(DataGridSelect select) {
		dataGridGesture(select, select.getBinding(), select.getRow());
	}

	@Override
	public void executeDataGridEdit(DataGridEdit edit) {
		// cannot edit a grid row in PF
	}

	private void dataGridGesture(Step step, String binding, Integer row) {
		PrimeFacesAutomationContext context = peek();
		String buttonIdentifier = step.getIdentifier(context);
		
		List<UIComponent> dataGridComponents = context.getFacesComponents(binding);
		if (dataGridComponents == null) {
			throw new MetaDataException(String.format("<%s /> with binding [%s] is not on the view.",
														(row != null) ? 
															((step instanceof DataGridZoom) ? "DataGridZoom" : "DataGridRemove") : 
															"DataGridNew",
														binding));
		}
		for (UIComponent dataGridComponent : dataGridComponents) {
			String dataGridClientId = PrimeFacesAutomationContext.clientId(dataGridComponent);
			boolean ajax = false;
			if (row != null) {
				if (step instanceof DataGridZoom) {
					comment(String.format("Zoom on row %d on data grid [%s] (%s)", row, binding, dataGridClientId));
				}
				else if (step instanceof DataGridSelect) {
					comment(String.format("Select on row %d on list grid [%s] (%s)", row, binding, dataGridClientId));
				}
				else {
					ajax = true;
					comment(String.format("Remove on row %d on data grid [%s] (%s)", row, binding, dataGridClientId));
				}
			}
			else {
				comment(String.format("New row on data grid [%s] (%s)", binding, dataGridClientId));
			}

			List<UIComponent> buttonComponents = context.getFacesComponents(buttonIdentifier);
			if (buttonComponents != null) { // button may not be shown
				for (UIComponent buttonComponent : buttonComponents) {
					String buttonClientId = (row != null) ?
												PrimeFacesAutomationContext.clientId(buttonComponent, row) :
												PrimeFacesAutomationContext.clientId(buttonComponent);
					if (buttonClientId.startsWith(dataGridClientId)) {
						if (step instanceof DataGridSelect) {
							indent().append("dataGridSelect(\"").append(dataGridClientId).append("\", ").append(String.valueOf(row)).append(");").newline();
						}
						else {
							indent().append("dataGridButton(\"").append(dataGridClientId).append("\", \"");
							append(buttonClientId).append("\", ").append(String.valueOf(ajax)).append(");").newline();
						}
					}
				}
			}
		}

		// Determine the Document of the edit view to push
		// NB Don't push a new context for DataGridSelect - let them use DataGridZoom if they want that
		if ((step instanceof DataGridNew) || (step instanceof DataGridZoom)) {
			Customer c = CORE.getUser().getCustomer();
			Module m = c.getModule(context.getModuleName());
			Document d = m.getDocument(c, context.getDocumentName());
			TargetMetaData target = BindUtil.getMetaDataForBinding(c, m, d, binding);
			Relation relation = (Relation) target.getAttribute();
			if (relation != null) { // should always be
				String newDocumentName = relation.getDocumentName();
				d = m.getDocument(c, newDocumentName);
				String newModuleName = d.getOwningModuleName();
				
				// Push it
				PushEditContext push = new PushEditContext();
				push.setModuleName(newModuleName);
				push.setDocumentName(newDocumentName);
				push.execute(this);
			}
		}
	}

	@Override
	public void executeListGridNew(ListGridNew nu) {
		listGridGesture(nu, null);
		
		PushEditContext push = listGridContext(nu.getQueryName(), nu.getDocumentName(), nu.getModelName(), nu);
		push.setCreateView(nu.getCreateView());
		push.execute(this);
	}

	@Override
	public void executeListGridZoom(ListGridZoom zoom) {
		listGridGesture(zoom, zoom.getRow());
		
		PushEditContext push = listGridContext(zoom.getQueryName(), zoom.getDocumentName(), zoom.getModelName(), zoom);
		push.execute(this);
	}

	@Override
	public void executeListGridSelect(ListGridSelect select) {
		listGridGesture(select, select.getRow());
	}

	private void listGridGesture(Step step, Integer row) {
		PrimeFacesAutomationContext context = peek();
		String buttonIdentifier = step.getIdentifier(context);
		String listGridIdentifier = buttonIdentifier.substring(0, buttonIdentifier.lastIndexOf('.'));
		
		List<UIComponent> listGridComponents = context.getFacesComponents(listGridIdentifier);
		if (listGridComponents == null) {
			throw new MetaDataException(String.format("<%s /> with identifier [%s] is not defined.",
															step.getClass().getSimpleName(),
															listGridIdentifier));
		}
		for (UIComponent listGridComponent : listGridComponents) {
			List<UIComponent> buttonComponents = context.getFacesComponents(buttonIdentifier);
			if (buttonComponents != null) { // button may not be shown
				String listGridClientId = PrimeFacesAutomationContext.clientId(listGridComponent);
				if (row != null) {
					if (step instanceof ListGridZoom) {
						comment(String.format("Zoom on row %d on list grid [%s] (%s)", row, listGridIdentifier, listGridClientId));
					}
					else if (step instanceof ListGridSelect) {
						comment(String.format("Select on row %d on list grid [%s] (%s)", row, listGridIdentifier, listGridClientId));
					}
					else {
						comment(String.format("Delete on row %d on list grid [%s] (%s)", row, listGridIdentifier, listGridClientId));
					}
				}
				else {
					comment(String.format("New row on list grid [%s] (%s)", listGridIdentifier, listGridClientId));
				}

				for (UIComponent buttonComponent : buttonComponents) {
					String buttonClientId = (row != null) ?
												PrimeFacesAutomationContext.clientId(buttonComponent, row) :
												PrimeFacesAutomationContext.clientId(buttonComponent);
					if (buttonClientId.startsWith(listGridClientId)) {
						if (step instanceof ListGridSelect) {
							indent().append("listGridSelect(\"").append(listGridClientId).append("\", ").append(String.valueOf(row)).append(");").newline();
						}
						else {
							indent().append("listGridButton(\"").append(listGridClientId).append("\", \"");
							append(buttonClientId).append("\", false);").newline();
						}
					}
				}
			}
		}
	}
	
	private PushEditContext listGridContext(String queryName, String documentName, String modelName, Step step) {
		PushEditContext result = new PushEditContext();

		PrimeFacesAutomationContext context = peek();
		if (ViewType.list.equals(context.getViewType())) {
			result.setModuleName(context.getModuleName());
			result.setDocumentName(context.getDocumentName());
		}
		else {
			String key = queryName;
			if (key == null) {
				key = documentName;
			}
			if (key == null) {
				key = modelName;
			}
			Customer c = CORE.getUser().getCustomer();
			Module m = c.getModule(context.getModuleName());
			MetaDataQueryDefinition q = m.getMetaDataQuery(key);
			if (q != null) {
				result.setModuleName(q.getDocumentModule(c).getName());
				result.setDocumentName(q.getDocumentName());
			}
			else {
				DocumentRef r = m.getDocumentRefs().get(key);
				if (r != null) {
					result.setModuleName(r.getReferencedModuleName());
					result.setDocumentName(key);
				}
				else {
					Document d = m.getDocument(c, context.getDocumentName());
					ListModel<Bean> lm = d.getListModel(c, key, true);
					if (lm != null) {
						d = lm.getDrivingDocument();
						result.setModuleName(d.getOwningModuleName());
						result.setDocumentName(d.getName());
					}
					else {
						throw new MetaDataException(String.format("<%s /> with identifier [%s] requires queryName, documentName or modelName defined.",
																	step.getClass().getSimpleName(),
																	step.getIdentifier(context)));
					}
				}
			}
		}
		
		return result;
	}

	@Override
	public void executeTestValue(TestValue test) {
		// TODO Auto-generated method stub
	}
	
	@Override
	public void executeTestSuccess(TestSuccess test) {
		TestStrategy strategy = getTestStrategy();
		if (TestStrategy.Verify.equals(strategy)) {
			comment("Test Success");
			indent().append("verifySuccess();").newline();
		}
		else if (TestStrategy.None.equals(strategy)) {
			// nothing to do
		}
		else { // null or Assert
			comment("Test Success");
			indent().append("assertSuccess();").newline();
		}
	}
	
	@Override
	public void executeTestFailure(TestFailure test) {
		TestStrategy strategy = getTestStrategy();
		if (! TestStrategy.None.equals(strategy)) {
			String message = test.getMessage();
			if (message == null) {
				comment("Test Failure");
				if (TestStrategy.Verify.equals(strategy)) {
					indent().append("verifyFailure();").newline();
				}
				else {
					indent().append("assertFailure();").newline();
				}
			}
			else {
				comment(String.format("Test Failure with message '%s'", message));
				if (TestStrategy.Verify.equals(strategy)) {
					indent().append("verifyFailure(\"").append(message).append("\"").newline();
				}
				else {
					indent().append("assertFailure(\"").append(message).append("\"").newline();
				}
			}
		}
	}
	
	private void get(String url) {
		indent().append("get(\"").append(url).append("\");").newline();
	}
}
