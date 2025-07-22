package util.sail;

import java.util.List;

import org.openqa.selenium.JavascriptExecutor;
import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.messages.DomainException;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.sail.execution.SmartClientAutomationContext;
import org.skyve.impl.sail.execution.WebDriverExecutor;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.Relation;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.Module.DocumentRef;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;
import org.skyve.metadata.sail.language.Automation.TestStrategy;
import org.skyve.metadata.sail.language.Step;
import org.skyve.metadata.sail.language.step.Comment;
import org.skyve.metadata.sail.language.step.Execute;
import org.skyve.metadata.sail.language.step.Pause;
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

/**
 * A SAIL executor that interprets the SAIL commands and makes the appropriate calls to the decorated test implementation.
 */
public class SmartClientInterpretedWebDriverExecutor extends WebDriverExecutor<SmartClientAutomationContext> {
	private SmartClientSelenide test;
	
	public SmartClientInterpretedWebDriverExecutor(SmartClientSelenide test) {
		this.test = test;
	}

	@Override
	public void executePushListContext(PushListContext push) {
		SmartClientAutomationContext newContext = new SmartClientAutomationContext();
		newContext(push, newContext);

		push(newContext);
		newContext.generate(push);
	}

	@Override
	public void executePushEditContext(PushEditContext push) {
		SmartClientAutomationContext newContext = new SmartClientAutomationContext();
		newContext(push, newContext);

		push(newContext);
		newContext.generate(push);
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
			test.trace("Login as " + user);
		}
		else {
			test.trace("Login as " + customer + "/" + user);
		}
		test.login(customer, user, login.getPassword());
	}
	
	@Override
	public void executeLogout(Logout logout) {
		test.trace("Logout");
		test.logout();
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
			test.trace(String.format("List for query [%s.%s]", moduleName, queryName));
			test.get(String.format("?a=l&m=%s&q=%s", moduleName, queryName));
		}
		else if (documentName != null) {
			if (modelName != null) {
				test.trace(String.format("List for model [%s.%s.%s]", moduleName, documentName, modelName));
				test.get(String.format("?a=l&m=%s&d=%s&q=%s", moduleName, documentName, modelName));
			}
			else {
				test.trace(String.format("List for default query of [%s.%s]", moduleName, documentName));
				test.get(String.format("?a=l&m=%s&q=%s", moduleName, documentName));
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
			test.trace(String.format("Edit new document [%s.%s] instance", moduleName, documentName));
			test.get(String.format("?a=e&m=%s&d=%s", moduleName, documentName));
		}
		else {
			test.trace(String.format("Edit document [%s.%s] instance with bizId %s", moduleName, documentName, bizId));
			test.get(String.format("?a=e&m=%s&d=%s&i=%s", moduleName, documentName, bizId));
		}
	}

	@Override
	public void executeNavigateTree(NavigateTree tree) {
		super.executeNavigateTree(tree); // determine driving document
	}

	@Override
	public void executeNavigateMap(NavigateMap map) {
		super.executeNavigateMap(map); // determine driving document
	}

	@Override
	public void executeNavigateCalendar(NavigateCalendar calendar) {
		super.executeNavigateCalendar(calendar); // determine driving document
	}

	@Override
	public void executeNavigateLink(NavigateLink link) {
		super.executeNavigateLink(link); // null driving document
	}

	@Override
	public void executeComment(Comment comment) {
		test.trace(comment.getComment());
	}
	
	@Override
	public void executeExecute(Execute execute) {
		JavascriptExecutor js = (JavascriptExecutor) test.driver;
		js.executeScript("javascript:" + execute.getScript());
	}
	
	@Override
	public void executePause(Pause pause) {
		test.pause(pause.getMillis());
	}
	
	@Override
	public void executeTabSelect(TabSelect tabSelect) {
		SmartClientAutomationContext context = peek();
		String identifier = tabSelect.getIdentifier(context);
		List<String> locators = context.getLocators(identifier);
		if (locators == null) {
			throw new MetaDataException("<tabSelect /> with path [" + tabSelect.getTabPath() + "] is not valid or is not on the view.");
		}

		boolean success = false;
		for (String locator : locators) {
			test.trace(String.format("click tab [%s] (%s)", tabSelect.getTabPath(), locator));
			success = success || test.tab(locator);
		}
		if (! success) {
			throw new DomainException("Tab not selected");
		}
	}

	@Override
	public void executeTestDataEnter(TestDataEnter testDataEnter) {
		SmartClientAutomationContext context = peek();
//		ExecutionDelegate.executeTestDataEnter(testDataEnter, context, this);
	}

	@Override
	public void executeDataEnter(DataEnter dataEnter) {
		SmartClientAutomationContext context = peek();
		String identifier = dataEnter.getIdentifier(context);
		List<String> locators = context.getLocators(identifier);
		if (locators == null) {
			throw new MetaDataException("<DataEnter /> with binding [" + identifier + "] is not valid or is not on the view.");
		}
		boolean success = false;
		for (String locator : locators) {
/*
			String clientId = PrimeFacesAutomationContext.clientId(component);
			boolean text = (component instanceof InputText) || 
								(component instanceof InputTextarea) || 
								(component instanceof Password) ||
								(component instanceof InputMask) ||
								(component instanceof ColorPicker);
			boolean selectOne = (component instanceof SelectOneMenu);
			boolean radio = (component instanceof SelectOneRadio);
			boolean checkbox = (component instanceof SelectBooleanCheckbox) || (component instanceof TriStateCheckbox);
			boolean _input = (component instanceof Spinner) || (component instanceof DatePicker);
			
			// replace newlines
			String value = dataEnter.getValue();
			if (value != null) {
				// never replace with a new line as chrome under test will trip the default button on the form 
				// from the enter key as represented by \n
				value = value.replace("\n", " ");
			}
			
			// if exists and is not disabled
			test.trace(String.format("set %s (%s) to %s", identifier, clientId, value));

			if (checkbox) {
				Boolean bool = Boolean.valueOf(value);
				success = success || test.checkbox(clientId, bool);
			}
			else if (text) {
				success = success || test.text(clientId, value, false);
			}
			else if (_input) {
				success = success || test._input(clientId, value, false);
			}
			else if (selectOne) {
				success = success || test.selectOne(clientId, Integer.parseInt(value));
			}
			else if (radio) {
				success = success || test.radio(clientId, Integer.parseInt(value));
			}
*/
		}
		if (! success) {
			throw new DomainException("Data entry failed");
		}
	}

	private void button(Step button, String tagName, boolean ajax, boolean confirm, Boolean testSuccess) {
		SmartClientAutomationContext context = peek();
		String identifier = button.getIdentifier(context);
		List<String> locators = context.getLocators(identifier);
		if (locators == null) {
			throw new MetaDataException(String.format("<%s /> is not on the view.", tagName));
		}
		boolean success = false;
		for (String locator : locators) {
/*
			test.trace(String.format("click [%s] (%s)", tagName, clientId));
			boolean successful = test.button(clientId, ajax, confirm);
			if (successful && (! Boolean.FALSE.equals(testSuccess))) { // true or null (defaults on)
				executeTestSuccess(new TestSuccess());
			}
			success = success || successful;
*/
		}
		if (! success) {
			throw new DomainException("Could not click button");
		}
	}

	private void redirectButton(Step button, String tagName, boolean confirm, Boolean testSuccess) {
		SmartClientAutomationContext context = peek();
		String identifier = button.getIdentifier(context);
		List<String> locators = context.getLocators(identifier);
		if (locators == null) {
			throw new MetaDataException(String.format("<%s /> is not on the view.", tagName));
		}
		boolean success = false;
		for (String locator : locators) {
/*
			test.trace(String.format("click [%s] (%s)", tagName, clientId));
			boolean successful = test.redirectButton(clientId, confirm);
			if (successful && (! Boolean.FALSE.equals(testSuccess))) { // true or null (defaults on)
				executeTestSuccess(new TestSuccess());
			}
			success = success || successful;
*/
		}
		if (! success) {
			throw new DomainException("Could not click button");
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
			SmartClientAutomationContext context = peek();
			context.setViewType(ViewType.create);
		}
		else if (Boolean.FALSE.equals(createView)) {
			SmartClientAutomationContext context = peek();
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
		SmartClientAutomationContext context = peek();
		
		List<String> locators = context.getLocators(binding);
		if (locators == null) {
			throw new MetaDataException(String.format("<%s /> with binding [%s] is not on the view.",
														step.getClass().getSimpleName(),
														binding));
		}
		boolean success = false;
		for (String locator : locators) {
/*
			if (row != null) {
				test.trace(String.format("Pick on row %d on lookup description [%s] (%s)", row, binding, clientId));
				success = success || test.lookupDescription(clientId, row.intValue());
			}
			else {
				test.trace(String.format("Auto complete with search '%s' on lookup description [%s] (%s)", search, binding, clientId));
				success = success || test.lookupDescription(clientId, search);
			}
*/
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
		SmartClientAutomationContext context = peek();
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
		SmartClientAutomationContext context = peek();
		String buttonIdentifier = step.getIdentifier(context);
		
		List<String> locators = context.getLocators(binding);
		if (locators == null) {
			throw new MetaDataException(String.format("<%s /> with binding [%s] is not on the view.",
														(row != null) ? 
															((step instanceof DataGridZoom) ? "DataGridZoom" : "DataGridRemove") : 
															"DataGridNew",
														binding));
		}
		for (String locator : locators) {
/*
			boolean ajax = false;
			if (row != null) {
				if (step instanceof DataGridZoom) {
					test.trace(String.format("Zoom on row %d on data grid [%s] (%s)", row, binding, dataGridClientId));
				}
				else if (step instanceof DataGridSelect) {
					test.trace(String.format("Select on row %d on list grid [%s] (%s)", row, binding, dataGridClientId));
				}
				else {
					ajax = true;
					test.trace(String.format("Remove on row %d on data grid [%s] (%s)", row, binding, dataGridClientId));
				}
			}
			else {
				test.trace(String.format("New row on data grid [%s] (%s)", binding, dataGridClientId));
			}

			List<UIComponent> buttonComponents = context.getFacesComponents(buttonIdentifier);
			if (buttonComponents != null) { // button may not be shown
				for (UIComponent buttonComponent : buttonComponents) {
					String buttonClientId = (row != null) ?
												PrimeFacesAutomationContext.clientId(buttonComponent, row) :
												PrimeFacesAutomationContext.clientId(buttonComponent);
					if (buttonClientId.startsWith(dataGridClientId)) {
						if (step instanceof DataGridSelect) {
							if (row == null) {
								throw new MetaDataException("No row defined in DataGridSelect");
							}
							test.dataGridSelect(dataGridClientId, row.intValue());
						}
						else {
							test.dataGridButton(dataGridClientId, buttonClientId, ajax);
						}
					}
				}
			}
*/
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
		SmartClientAutomationContext context = peek();
		String gestureIdentifier = step.getIdentifier(context);
		List<String> locators = context.getLocators(gestureIdentifier);
		if (locators == null) {
			throw new MetaDataException(String.format("<%s /> with identifier [%s] is not defined.",
															step.getClass().getSimpleName(),
															gestureIdentifier));
		}
		for (String locator : locators) {
			test.click(test.locate(locator));
/*
			List<UIComponent> buttonComponents = context.getFacesComponents(buttonIdentifier);
			if (buttonComponents != null) { // button may not be shown
				String listGridClientId = PrimeFacesAutomationContext.clientId(listGridComponent);
				if (row != null) {
					if (step instanceof ListGridZoom) {
						test.trace(String.format("Zoom on row %d on list grid [%s] (%s)", row, listGridIdentifier, listGridClientId));
					}
					else if (step instanceof ListGridSelect) {
						test.trace(String.format("Select on row %d on list grid [%s] (%s)", row, listGridIdentifier, listGridClientId));
					}
					else {
						test.trace(String.format("Delete on row %d on list grid [%s] (%s)", row, listGridIdentifier, listGridClientId));
					}
				}
				else {
					test.trace(String.format("New row on list grid [%s] (%s)", listGridIdentifier, listGridClientId));
				}

				for (UIComponent buttonComponent : buttonComponents) {
					String buttonClientId = (row != null) ?
												PrimeFacesAutomationContext.clientId(buttonComponent, row) :
												PrimeFacesAutomationContext.clientId(buttonComponent);
					if (buttonClientId.startsWith(listGridClientId)) {
						if (step instanceof ListGridSelect) {
							if (row == null) {
								throw new MetaDataException("No row defined in ListGridSelect");
							}
							test.listGridSelect(listGridClientId, row.intValue());
						}
						else {
							test.listGridButton(listGridClientId, buttonClientId, false);
						}
					}
				}
			}
*/
		}
	}
	
	private PushEditContext listGridContext(String queryName, String documentName, String modelName, Step step) {
		PushEditContext result = new PushEditContext();

		SmartClientAutomationContext context = peek();
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
	public void executeTestSuccess(TestSuccess success) {
		TestStrategy strategy = getTestStrategy();
		if (TestStrategy.Verify.equals(strategy)) {
			test.trace("Test Success");
			test.verifySuccess();
		}
		else if (TestStrategy.None.equals(strategy)) {
			// nothing to do
		}
		else { // null or Assert
			test.trace("Test Success");
			test.assertSuccess();
		}
	}
	
	@Override
	public void executeTestFailure(TestFailure failure) {
		TestStrategy strategy = getTestStrategy();
		if (! TestStrategy.None.equals(strategy)) {
			String message = failure.getMessage();
			if (message == null) {
				comment("Test Failure");
				if (TestStrategy.Verify.equals(strategy)) {
					test.verifyFailure();
				}
				else {
					test.assertFailure();
				}
			}
			else {
				test.trace(String.format("Test Failure with message '%s'", message));
				if (TestStrategy.Verify.equals(strategy)) {
					test.verifyFailure(message);
				}
				else {
					test.assertFailure(message);
				}
			}
		}
	}
}
