package org.skyve.impl.sail.execution.pf;

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
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.module.ModuleImpl;
import org.skyve.impl.metadata.view.ViewImpl;
import org.skyve.impl.sail.execution.TestDataEnterViewVisitor;
import org.skyve.impl.sail.execution.WebDriverExecutor;
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
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.View.ViewType;
import org.skyve.metadata.view.model.list.ListModel;
import org.skyve.util.Binder.TargetMetaData;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import jakarta.faces.component.UIComponent;

/**
 * Executes SAIL interaction steps against the web UI for test scenarios.
 */
@SuppressWarnings("java:S1192") // Repeated literals are deliberate inline WebDriver script fragments.
public class PrimeFacesInlineWebDriverExecutor extends WebDriverExecutor<PrimeFacesAutomationContext> {

	private ComponentBuilder componentBuilder;
	private LayoutBuilder layoutBuilder;
	
	/**
	 * Creates an inline WebDriver executor with explicit component and layout builders.
	 *
	 * @param componentBuilder the component builder used to generate PrimeFaces components
	 * @param layoutBuilder the layout builder used to assemble PrimeFaces layouts
	 */
	public PrimeFacesInlineWebDriverExecutor(ComponentBuilder componentBuilder,
												LayoutBuilder layoutBuilder) {
		this.componentBuilder = componentBuilder;
		this.layoutBuilder = layoutBuilder;
	}

	/**
	 * Pushes a list-view context and generates component mappings for the list scope.
	 *
	 * @param push the list-context metadata to push
	 */
	@Override
	public void executePushListContext(PushListContext push) {
		PrimeFacesAutomationContext newContext = new PrimeFacesAutomationContext();
		newContext(push, newContext);

		push(newContext);
		newContext.generate(new PrimeFacesGenerateListContext(push, componentBuilder));
	}

	/**
	 * Pushes an edit-view context and generates component mappings for the edit scope.
	 *
	 * @param push the edit-context metadata to push
	 */
	@Override
	public void executePushEditContext(PushEditContext push) {
		PrimeFacesAutomationContext newContext = new PrimeFacesAutomationContext();
		newContext(push, newContext);

		push(newContext);
		newContext.generate(new PrimeFacesGenerateEditContext(push, componentBuilder, layoutBuilder));
	}
	
	/**
	 * Clears the current execution context stack.
	 *
	 * @param clear the clear-context step being executed
	 */
	@Override
	public void executeClearContext(ClearContext clear) {
		clear();
	}
	
	/**
	 * Pops the active execution context from the stack.
	 *
	 * @param pop the pop-context step being executed
	 */
	@Override
	public void executePopContext(PopContext pop) {
		pop();
	}
	
	/**
	 * Emits login script statements for the supplied credentials.
	 *
	 * @param login the login step containing authentication credentials
	 */
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
	
	/**
	 * Emits logout script statements.
	 *
	 * @param logout the logout step being executed
	 */
	@Override
	public void executeLogout(Logout logout) {
		comment("Logout");
		indent().append("logout();").newline();
	}
	
	/**
	 * Navigates to a list view and pushes corresponding list context metadata.
	 *
	 * @param list the list-navigation step to execute
	 */
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

	/**
	 * Navigates to an edit view and pushes corresponding edit context metadata.
	 *
	 * @param edit the edit-navigation step to execute
	 */
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

	/**
	 * Resolves tree-navigation metadata but does not currently emit a WebDriver script step.
	 *
	 * <p>Current behavior: delegates to the superclass so the driving-document context is updated consistently,
	 * then performs no additional action because PrimeFaces inline tree navigation has not yet been implemented.
	 *
	 * @param tree the tree-navigation step to resolve
	 */
	@Override
	public void executeNavigateTree(NavigateTree tree) {
		super.executeNavigateTree(tree); // determine driving document
		// TODO Auto-generated method stub
	}

	/**
	 * Resolves map-navigation metadata but does not currently emit a WebDriver script step.
	 *
	 * <p>Current behavior: delegates to the superclass so the driving-document context is updated consistently,
	 * then performs no additional action because PrimeFaces inline map navigation has not yet been implemented.
	 *
	 * @param map the map-navigation step to resolve
	 */
	@Override
	public void executeNavigateMap(NavigateMap map) {
		super.executeNavigateMap(map); // determine driving document
		// TODO Auto-generated method stub
	}

	/**
	 * Resolves calendar-navigation metadata but does not currently emit a WebDriver script step.
	 *
	 * <p>Current behavior: delegates to the superclass so the driving-document context is updated consistently,
	 * then performs no additional action because PrimeFaces inline calendar navigation has not yet been implemented.
	 *
	 * @param calendar the calendar-navigation step to resolve
	 */
	@Override
	public void executeNavigateCalendar(NavigateCalendar calendar) {
		super.executeNavigateCalendar(calendar); // determine driving document
		// TODO Auto-generated method stub
	}

	/**
	 * Resolves link-navigation metadata but does not currently emit a WebDriver script step.
	 *
	 * <p>Current behavior: delegates to the superclass so the executor records the null driving-document state,
	 * then performs no additional action because PrimeFaces inline link navigation has not yet been implemented.
	 *
	 * @param link the link-navigation step to resolve
	 */
	@Override
	public void executeNavigateLink(NavigateLink link) {
		super.executeNavigateLink(link); // null driving document
		// TODO Auto-generated method stub
	}

	/**
	 * Selects one or more rendered tab components resolved from the tab path identifier.
	 *
	 * @param tabSelect the tab-selection step to execute
	 */
	@Override
	public void executeTabSelect(TabSelect tabSelect) {
		PrimeFacesAutomationContext context = peek();
		String identifier = tabSelect.getIdentifier(context);
		List<UIComponent> components = context.getFacesComponents(identifier);
		if (components == null) {
			throw new MetaDataException("<tabSelect /> with path [" + tabSelect.getTabPath() + "] is not valid or is not on the view.");
		}
		for (UIComponent component : components) {
			String clientId = PrimeFacesAutomationContext.clientId(component);
			comment(String.format("click tab [%s]", tabSelect.getTabPath()));
			indent().append("tab(\"").append(clientId).append("\");").newline();
		}
	}

	/**
	 * Builds fixture data and emits generated data-entry steps for the current edit context.
	 *
	 * @param testDataEnter the test-data-enter step describing fixture selection
	 */
	@Override
	public void executeTestDataEnter(TestDataEnter testDataEnter) {
		PrimeFacesAutomationContext context = peek();

		User u = CORE.getUser();
		Customer c = u.getCustomer();
		Module m = c.getModule(context.getModuleName());
		Document d = m.getDocument(c, context.getDocumentName());

		Bean bean = null;

		String fixture = testDataEnter.getFixture();
		if (fixture == null) {
			bean = new DataBuilder().fixture(FixtureType.sail)
					.build(d);
		} else {
			bean = new DataBuilder().fixture(fixture)
					.build(d);
		}

		final String uxui = context.getUxui();
		ViewImpl view = (ViewImpl) d.getView(uxui, c, context.getViewType()
				.toString());

		TestDataEnterViewVisitor visitor = new TestDataEnterViewVisitor(
				(CustomerImpl) c,
				(ModuleImpl) m,
				(DocumentImpl) d,
				view,
				uxui,
				bean);

		visitor.visit();

		for (Step steps : visitor.getScalarSteps()) {
			steps.execute(this);
		}
	}

	/**
	 * Emits data-entry script for resolved component types matching the supplied binding.
	 *
	 * @param dataEnter the data-entry step to execute
	 */
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

	/**
	 * Emits script for clicking resolved button-like components and optional success assertions.
	 *
	 * @param button the button-oriented step being executed
	 * @param tagName the logical SAIL tag name used in comments and error reporting
	 * @param ajax whether the click is expected to execute via AJAX
	 * @param confirm whether a confirmation dialog is expected
	 * @param testSuccess optional success-assertion override
	 */
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

	/**
	 * Emits script for redirecting button actions and optional success assertions.
	 *
	 * @param button the button-oriented step being executed
	 * @param tagName the logical SAIL tag name used in comments and error reporting
	 * @param confirm whether a confirmation dialog is expected
	 * @param testSuccess optional success-assertion override
	 */
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

	/**
	 * Executes an OK action and pops the active edit context.
	 *
	 * @param ok the OK action step to execute
	 */
	@Override
	public void executeOk(Ok ok) {
		redirectButton(ok, "ok", false, ok.getTestSuccess());
		pop();
	}

	/**
	 * Executes a save action and updates inferred view type when create-view intent is provided.
	 *
	 * @param save the save action step to execute
	 */
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

	/**
	 * Executes a cancel action and pops the active edit context.
	 *
	 * @param cancel the cancel action step to execute
	 */
	@Override
	public void executeCancel(Cancel cancel) {
		button(cancel, "cancel", false, false, cancel.getTestSuccess());
		pop();
	}

	/**
	 * Executes a delete action and pops the active edit context.
	 *
	 * @param delete the delete action step to execute
	 */
	@Override
	public void executeDelete(Delete delete) {
		redirectButton(delete, "delete", true, delete.getTestSuccess());
		pop();
	}

	/**
	 * Executes a zoom-out action and pops the active edit context.
	 *
	 * @param zoomOut the zoom-out action step to execute
	 */
	@Override
	public void executeZoomOut(ZoomOut zoomOut) {
		button(zoomOut, "zoom out", false, false, zoomOut.getTestSuccess());
		pop();
	}

	/**
	 * Executes a remove action and pops the active edit context.
	 *
	 * @param remove the remove action step to execute
	 */
	@Override
	public void executeRemove(Remove remove) {
		button(remove, "remove", false, true, remove.getTestSuccess());
		pop();
	}

	/**
	 * Executes a custom action button resolved from the current context.
	 *
	 * @param action the custom action step to execute
	 */
	@Override
	public void executeAction(Action action) {
		button(action, 
				action.getActionName(),
				true,
				Boolean.TRUE.equals(action.getConfirm()),
				action.getTestSuccess());
	}

	/**
	 * Executes lookup auto-complete selection by search text.
	 *
	 * @param complete the lookup auto-complete step to execute
	 */
	@Override
	public void executeLookupDescriptionAutoComplete(LookupDescriptionAutoComplete complete) {
		lookupDescription(complete, complete.getBinding(), null, complete.getSearch());
	}

	/**
	 * Executes lookup pick-list selection by row index.
	 *
	 * @param pick the lookup pick step to execute
	 */
	@Override
	public void executeLookupDescriptionPick(LookupDescriptionPick pick) {
		lookupDescription(pick, pick.getBinding(), pick.getRow(), null);
	}

	/**
	 * Emits lookup-description interaction script for either row picking or search auto-complete.
	 *
	 * @param step the originating lookup step
	 * @param binding the lookup binding identifier
	 * @param row optional row index for pick-list selection
	 * @param search optional search string for auto-complete selection
	 */
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

	/**
	 * No-op because PrimeFaces lookup descriptions do not expose inline new behavior.
	 *
	 * @param nu the lookup-description-new step being ignored
	 */
	@Override
	public void executeLookupDescriptionNew(LookupDescriptionNew nu) {
		// Nothing to do here as PF doesn't allow new off of lookup descriptions
	}

	/**
	 * No-op because PrimeFaces lookup descriptions do not expose inline edit behavior.
	 *
	 * @param edit the lookup-description-edit step being ignored
	 */
	@Override
	public void executeLookupDescriptionEdit(LookupDescriptionEdit edit) {
		// Nothing to do here as PF doesn't allow edit off of lookup descriptions
	}

	/**
	 * Executes zoom-in and pushes a nested edit context for the resolved relation target.
	 *
	 * @param zoom the zoom-in action step to execute
	 */
	@Override
	public void executeZoomIn(ZoomIn zoom) {
		button(zoom, "zoomIn", false, false, zoom.getTestSuccess());
		
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
	
	/**
	 * Executes DataGrid new-row interaction.
	 *
	 * @param nu the data-grid-new step to execute
	 */
	@Override
	public void executeDataGridNew(DataGridNew nu) {
		dataGridGesture(nu, nu.getBinding(), null);
	}
	
	/**
	 * Executes DataGrid zoom interaction on a specific row.
	 *
	 * @param zoom the data-grid-zoom step to execute
	 */
	@Override
	public void executeDataGridZoom(DataGridZoom zoom) {
		dataGridGesture(zoom, zoom.getBinding(), zoom.getRow());
	}
	
	/**
	 * Executes DataGrid remove interaction on a specific row.
	 *
	 * @param remove the data-grid-remove step to execute
	 */
	@Override
	public void executeDataGridRemove(DataGridRemove remove) {
		dataGridGesture(remove, remove.getBinding(), remove.getRow());
	}

	/**
	 * Executes DataGrid row-select interaction.
	 *
	 * @param select the data-grid-select step to execute
	 */
	@Override
	public void executeDataGridSelect(DataGridSelect select) {
		dataGridGesture(select, select.getBinding(), select.getRow());
	}

	/**
	 * No-op because inline editing of DataGrid rows is not supported by this executor.
	 *
	 * @param edit the data-grid-edit step being ignored
	 */
	@Override
	public void executeDataGridEdit(DataGridEdit edit) {
		// cannot edit a grid row in PF
	}

	/**
	 * Emits DataGrid gesture script and pushes nested edit context when navigation enters a related row.
	 *
	 * @param step the originating data-grid step
	 * @param binding the data-grid binding identifier
	 * @param row optional row index for row-scoped interactions
	 */
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

	/**
	 * Executes ListGrid new-row interaction and pushes an edit context for the row document.
	 *
	 * @param nu the list-grid-new step to execute
	 */
	@Override
	public void executeListGridNew(ListGridNew nu) {
		listGridGesture(nu, null);
		
		PushEditContext push = listGridContext(nu.getQueryName(), nu.getDocumentName(), nu.getModelName(), nu);
		push.setCreateView(nu.getCreateView());
		push.execute(this);
	}

	/**
	 * Executes ListGrid zoom interaction and pushes an edit context for the row document.
	 *
	 * @param zoom the list-grid-zoom step to execute
	 */
	@Override
	public void executeListGridZoom(ListGridZoom zoom) {
		listGridGesture(zoom, zoom.getRow());
		
		PushEditContext push = listGridContext(zoom.getQueryName(), zoom.getDocumentName(), zoom.getModelName(), zoom);
		push.execute(this);
	}

	/**
	 * Executes ListGrid row-select interaction.
	 *
	 * @param select the list-grid-select step to execute
	 */
	@Override
	public void executeListGridSelect(ListGridSelect select) {
		listGridGesture(select, select.getRow());
	}

	/**
	 * Emits ListGrid gesture script for new/zoom/select interactions.
	 *
	 * @param step the originating list-grid step
	 * @param row optional row index for row-scoped interactions
	 */
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
	
	/**
	 * Resolves and builds a push-edit context target for list-grid interactions.
	 *
	 * @param queryName optional query name driving the list-grid
	 * @param documentName optional document name driving the list-grid
	 * @param modelName optional model name driving the list-grid
	 * @param step the originating list-grid step
	 * @return a populated push-edit context for the target document
	 */
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
					try {
						ListModel<Bean> lm = d.getListModel(c, key, false);
						d = lm.getDrivingDocument();
						result.setModuleName(d.getOwningModuleName());
						result.setDocumentName(d.getName());
					}
					catch (MetaDataException e) {
						throw new MetaDataException(String.format("<%s /> with identifier [%s] requires queryName, documentName or modelName defined.",
																	step.getClass().getSimpleName(),
																	step.getIdentifier(context)),
													e);
						
					}
				}
			}
		}
		
		return result;
	}

	/**
	 * Placeholder for value assertion support in inline WebDriver execution.
	 *
	 * @param test the value-assertion step being ignored until implemented
	 */
	@Override
	public void executeTestValue(TestValue test) {
		// TODO Auto-generated method stub
	}
	
	/**
	 * Emits success assertions or verifications based on the configured test strategy.
	 *
	 * @param test the success-assertion step to execute
	 */
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
	
	/**
	 * Emits failure assertions or verifications based on the configured test strategy.
	 *
	 * @param test the failure-assertion step to execute
	 */
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
	
	/**
	 * Emits a script-level GET navigation call for the supplied URL fragment.
	 *
	 * @param url the URL fragment to navigate to
	 */
	private void get(String url) {
		indent().append("get(\"").append(url).append("\");").newline();
	}
}
