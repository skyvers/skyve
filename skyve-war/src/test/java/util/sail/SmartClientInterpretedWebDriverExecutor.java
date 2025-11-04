package util.sail;

import java.util.List;

import org.apache.commons.lang3.BooleanUtils;
import org.openqa.selenium.JavascriptExecutor;
import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.messages.DomainException;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.module.ModuleImpl;
import org.skyve.impl.metadata.view.ViewImpl;
import org.skyve.impl.sail.execution.Locator;
import org.skyve.impl.sail.execution.Locator.InputType;
import org.skyve.impl.sail.execution.SmartClientAutomationContext;
import org.skyve.impl.sail.execution.TestDataEnterViewVisitor;
import org.skyve.impl.sail.execution.WebDriverExecutor;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.Relation;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.Module.DocumentRef;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;
import org.skyve.metadata.sail.execution.ExecutionOptions;
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
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.View;
import org.skyve.metadata.view.View.ViewType;
import org.skyve.metadata.view.model.list.ListModel;
import org.skyve.util.Binder.TargetMetaData;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

/**
 * Executes SAIL commands and delegates actions to the underlying test implementation.
 * 
 * @author mike
 */
public class SmartClientInterpretedWebDriverExecutor extends WebDriverExecutor<SmartClientAutomationContext> {

	private static List<ImplicitActionName> EXCLUDED_IMPLICIT_ACTIONS = List.of(ImplicitActionName.Upload);

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

		test.waitForFullPageResponse();
	}

	@Override
	public void executePushEditContext(PushEditContext push, ExecutionOptions options) {
		SmartClientAutomationContext newContext = new SmartClientAutomationContext();
		newContext(push, newContext);

		push(newContext);
		newContext.generate(push, options.isWindowed());

		test.waitForFullPageResponse();
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
		} else {
			test.trace("Login as " + customer + "/" + user);
		}

		test.login(customer, user, login.getPassword());

		test.waitForFullPageResponse();
	}
	
	@Override
	public void executeLogout(Logout logout) {
		test.trace("Logout");
		test.logout();

		test.waitForFullPageResponse();
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
		} else if (documentName != null) {
			if (modelName != null) {
				test.trace(String.format("List for model [%s.%s.%s]", moduleName, documentName, modelName));
				test.get(String.format("?a=l&m=%s&d=%s&q=%s", moduleName, documentName, modelName));
			} else {
				test.trace(String.format("List for default query of [%s.%s]", moduleName, documentName));
				test.get(String.format("?a=l&m=%s&q=%s", moduleName, documentName));
			}
		}

		test.waitForFullPageResponse();
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
		} else {
			test.trace(String.format("Edit document [%s.%s] instance with bizId %s", moduleName, documentName, bizId));
			test.get(String.format("?a=e&m=%s&d=%s&i=%s", moduleName, documentName, bizId));
		}

		test.waitForFullPageResponse();
	}

	@Override
	public void executeNavigateTree(NavigateTree tree) {
		super.executeNavigateTree(tree); // Determine driving document
	}

	@Override
	public void executeNavigateMap(NavigateMap map) {
		super.executeNavigateMap(map); // Determine driving document
	}

	@Override
	public void executeNavigateCalendar(NavigateCalendar calendar) {
		super.executeNavigateCalendar(calendar); // Determine driving document
	}

	@Override
	public void executeNavigateLink(NavigateLink link) {
		super.executeNavigateLink(link); // Null driving document
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

		List<Locator> locators = context.getLocators(identifier);
		if (locators == null) {
			throw new MetaDataException(
					String.format("<tabSelect /> with path [%s] is not valid or is not on the view", tabSelect.getTabPath()));
		}
		
		if (tabSelect.getTabPath().contains("/")) {
			// TODO: Address this edge case properly
			throw new MetaDataException(
			        String.format("<tabSelect /> with path [%s] is invalid because '/' cannot be used with SmartClient locators", tabSelect.getTabPath()));
		}

		boolean success = false;

		for (Locator locator : locators) {
			if (success) {
				continue;
			}

			test.trace(String.format("click tab [%s] (%s)", tabSelect.getTabPath(), locator));
			success = test.tab(locator.getLocator());
		}

		if (!success) {
			throw new DomainException("Tab not selected");
		}

		test.waitForFullPageResponse();
	}

	@Override
	public void executeTestDataEnter(TestDataEnter testDataEnter) {
		SmartClientAutomationContext context = peek();

		User u = CORE.getUser();
		Customer c = u.getCustomer();
		Module m = c.getModule(context.getModuleName());
		Document d = m.getDocument(c, context.getDocumentName());
		
		Bean bean = null;

		String fixture = testDataEnter.getFixture();
		if (fixture == null) {
			bean = new DataBuilder().fixture(FixtureType.sail).build(d);
		} else {
			bean = new DataBuilder().fixture(fixture).build(d);
		}

		final String uxui = context.getUxui();
		ViewImpl view = (ViewImpl) d.getView(uxui, c, context.getViewType().toString());

		TestDataEnterViewVisitor visitor = new TestDataEnterViewVisitor(
				(CustomerImpl) c,
				(ModuleImpl) m,
				(DocumentImpl) d,
				view,
				uxui,
				bean);

		visitor.visit();

		for (Step steps : visitor.getScalarSteps()) {
			steps.execute(this, ExecutionOptions.defaultOptions());
		}
	}

	@Override
	public void executeDataEnter(DataEnter dataEnter) {
		SmartClientAutomationContext context = peek();

		String identifier = dataEnter.getIdentifier(context);
		List<Locator> locators = context.getLocators(identifier);
		if (locators == null) {
			throw new MetaDataException(
					String.format("<dataEnter /> with binding [%s] is not supported or is not on the view", identifier));
		}

		boolean success = false;

		for (Locator locator : locators) {
			InputType inputType = locator.getInputType();

			// Never replace with a new line as chrome under test will trip the default button on the form
			String value = dataEnter.getValue();
			if (value != null) {
				value = value.replace("\n", " ");
			}

			if (InputType.CHECKBOX == inputType) {
				success = success || test.checkbox(locator.getLocator(), Boolean.valueOf(value));
			} else if (InputType.COMBO == inputType) {
				success = success || test.selectOne(locator.getLocator(), Integer.parseInt(value));
			} else if (InputType.TEXT == inputType) {
				success = success || test.text(locator.getLocator(), value, false);
			} else if (InputType.RADIO == inputType) {
				success = success || test.radio(locator.getLocator(), Integer.parseInt(value));
			}
		}

		if (!success) {
			throw new DomainException("Data entry failed");
		}

		test.waitForFullPageResponse();
	}

	private void button(Step button, String tagName, boolean confirm, Boolean testSuccess) {
		SmartClientAutomationContext context = peek();

		User u = CORE.getUser();
		Customer c = u.getCustomer();
		Module m = c.getModule(context.getModuleName());
		Document d = m.getDocument(c, context.getDocumentName());
		View v = d.getView(context.getUxui(), c, context.getViewType().toString());
		org.skyve.metadata.view.Action a = v.getAction(tagName);

		// Guard against unsupported actions
		if (a != null && a.getImplicitName() != null && EXCLUDED_IMPLICIT_ACTIONS.contains(a.getImplicitName())) {
			throw new MetaDataException(String.format("<%s /> is not supported", tagName));
		}

		String identifier = button.getIdentifier(context);

		List<Locator> locators = context.getLocators(identifier);
		if (locators == null) {
			throw new MetaDataException(String.format("<%s /> is not on the view", tagName));
		}

		boolean success = false;

		for (Locator locator : locators) {
			if (success) {
				continue;
			}

			test.trace(String.format("click [%s]", tagName));
			success = test.button(locator.getLocator(), confirm);

			if (success) {
				if (BooleanUtils.isNotFalse(testSuccess)) {
					executeTestSuccess(new TestSuccess());
				} else {
					test.okIfPresent();
				}
			}
		}

		if (!success) {
			throw new DomainException("Could not click button");
		}
	}

	@Override
	public void executeOk(Ok ok) {
		button(ok, "ok", false, ok.getTestSuccess());
		pop();

		test.waitForFullPageResponse();
	}

	@Override
	public void executeSave(Save save) {
		button(save, "save", false, save.getTestSuccess());

		Boolean createView = save.getCreateView();
		if (Boolean.TRUE.equals(createView)) {
			SmartClientAutomationContext context = peek();
			context.setViewType(ViewType.create);
		} else if (Boolean.FALSE.equals(createView)) {
			SmartClientAutomationContext context = peek();
			context.setViewType(ViewType.edit);
		}

		test.waitForFullPageResponse();
	}

	@Override
	public void executeCancel(Cancel cancel) {
		button(cancel, "cancel", false, cancel.getTestSuccess());
		pop();

		test.waitForFullPageResponse();
	}

	@Override
	public void executeDelete(Delete delete) {
		button(delete, "delete", true, delete.getTestSuccess());
		pop();

		test.waitForFullPageResponse();
	}

	@Override
	public void executeZoomOut(ZoomOut zoomOut) {
		button(zoomOut, "zoom out", false, zoomOut.getTestSuccess());
		pop();

		SmartClientAutomationContext.decrementWindowNumber();

		test.waitForFullPageResponse();
	}

	@Override
	public void executeRemove(Remove remove) {
		button(remove, "remove", true, remove.getTestSuccess());
		pop();

		test.waitForFullPageResponse();
	}

	@Override
	public void executeAction(Action action) {
		button(action, action.getActionName(), Boolean.TRUE.equals(action.getConfirm()), action.getTestSuccess());

		test.waitForFullPageResponse();
	}

	@Override
	public void executeLookupDescriptionAutoComplete(LookupDescriptionAutoComplete complete) {
		lookupDescription(complete, complete.getBinding(), null, complete.getSearch());

		test.waitForFullPageResponse();
	}

	@Override
	public void executeLookupDescriptionPick(LookupDescriptionPick pick) {
		lookupDescription(pick, pick.getBinding(), pick.getRow(), null);

		test.waitForFullPageResponse();
	}

	private void lookupDescription(Step step, String binding, Integer row, String search) {
		SmartClientAutomationContext context = peek();
		
		List<Locator> locators = context.getLocators(binding);
		if (locators == null) {
			throw new MetaDataException(String.format("<%s /> with binding [%s] is not on the view", step.getClass()
					.getSimpleName(), binding));
		}

		boolean success = false;

		for (Locator locator : locators) {
			if (row != null) {
				test.trace(String.format("Pick on row %d on lookup description [%s]", row, binding));
				success = success || test.lookupDescription(locator.getLocator(), row.intValue());
			} else {
				test.trace(String.format("Auto complete with search '%s' on lookup description [%s]", search, binding));
				success = success || test.lookupDescription(locator.getLocator(), search);
			}
		}

		if (!success) {
			throw new DomainException("Data entry failed");
		}
	}

	@Override
	public void executeLookupDescriptionNew(LookupDescriptionNew nu) {
		lookupDescriptionNew(nu, nu.getBinding());

		// Determine the document of the edit view to push
		SmartClientAutomationContext context = peek();
		String binding = nu.getBinding();

		User u = CORE.getUser();
		Customer c = u.getCustomer();
		Module m = c.getModule(context.getModuleName());
		Document d = m.getDocument(c, context.getDocumentName());
		TargetMetaData target = BindUtil.getMetaDataForBinding(c, m, d, binding);
		Relation relation = (Relation) target.getAttribute();

		if (relation != null) {
			String newDocumentName = relation.getDocumentName();
			d = m.getDocument(c, newDocumentName);
			String newModuleName = d.getOwningModuleName();

			// Push it
			PushEditContext push = new PushEditContext();
			push.setModuleName(newModuleName);
			push.setDocumentName(newDocumentName);
			push.execute(this, ExecutionOptions.windowed());
		}

		test.waitForFullPageResponse();
	}
	
	private void lookupDescriptionNew(Step step, String binding) {
		SmartClientAutomationContext context = peek();

		List<Locator> locators = context.getLocators(binding);
		if (locators == null) {
			throw new MetaDataException(String.format("<%s /> with binding [%s] is not on the view",
					step.getClass().getSimpleName(), binding));
		}

		boolean success = false;

		for (Locator locator : locators) {
			if (success) {
				continue;
			}

			test.trace(String.format("New on lookup description [%s]", binding));
			success = test.lookupDescriptionNew(locator.getLocator());
		}

		if (!success) {
			throw new DomainException("New on lookup description failed");
		}
	}

	@Override
	public void executeLookupDescriptionEdit(LookupDescriptionEdit edit) {
		lookupDescriptionEdit(edit, edit.getBinding());

		// Determine the document of the edit view to push
		SmartClientAutomationContext context = peek();
		String binding = edit.getBinding();

		User u = CORE.getUser();
		Customer c = u.getCustomer();
		Module m = c.getModule(context.getModuleName());
		Document d = m.getDocument(c, context.getDocumentName());
		TargetMetaData target = BindUtil.getMetaDataForBinding(c, m, d, binding);
		Relation relation = (Relation) target.getAttribute();

		if (relation != null) {
			String newDocumentName = relation.getDocumentName();
			d = m.getDocument(c, newDocumentName);
			String newModuleName = d.getOwningModuleName();

			// Push it
			PushEditContext push = new PushEditContext();
			push.setModuleName(newModuleName);
			push.setDocumentName(newDocumentName);
			push.execute(this, ExecutionOptions.windowed());
		}

		test.waitForFullPageResponse();
	}

	private void lookupDescriptionEdit(Step step, String binding) {
		SmartClientAutomationContext context = peek();

		List<Locator> locators = context.getLocators(binding);
		if (locators == null) {
			throw new MetaDataException(String.format("<%s /> with binding [%s] is not on the view",
					step.getClass().getSimpleName(), binding));
		}

		boolean success = false;

		for (Locator locator : locators) {
			if (success) {
				continue;
			}

			test.trace(String.format("Edit on lookup description [%s]", binding));
			success = test.lookupDescriptionEdit(locator.getLocator());
		}

		if (!success) {
			throw new DomainException("Edit on lookup description failed");
		}
	}

	@Override
	public void executeZoomIn(ZoomIn zoom) {
		button(zoom, "zoomIn", false, zoom.getTestSuccess());

		// Determine the document of the edit view to push
		SmartClientAutomationContext context = peek();

		String binding = zoom.getBinding();
		Customer c = CORE.getUser().getCustomer();
		Module m = c.getModule(context.getModuleName());
		Document d = m.getDocument(c, context.getDocumentName());
		TargetMetaData target = BindUtil.getMetaDataForBinding(c, m, d, binding);
		Relation relation = (Relation) target.getAttribute();

		if (relation != null) {
			String newDocumentName = relation.getDocumentName();
			d = m.getDocument(c, newDocumentName);
			String newModuleName = d.getOwningModuleName();
			
			// Push it
			PushEditContext push = new PushEditContext();
			push.setModuleName(newModuleName);
			push.setDocumentName(newDocumentName);
			push.execute(this, ExecutionOptions.windowed());
		}

		test.waitForFullPageResponse();
	}
	
	@Override
	public void executeDataGridNew(DataGridNew nu) {
		dataGridGesture(nu, nu.getBinding(), null);

		test.waitForFullPageResponse();
	}
	
	@Override
	public void executeDataGridZoom(DataGridZoom zoom) {
		dataGridGesture(zoom, zoom.getBinding(), zoom.getRow());

		test.waitForFullPageResponse();
	}
	
	@Override
	public void executeDataGridRemove(DataGridRemove remove) {
		dataGridGesture(remove, remove.getBinding(), remove.getRow());

		test.waitForFullPageResponse();
	}

	@Override
	public void executeDataGridSelect(DataGridSelect select) {
		dataGridGesture(select, select.getBinding(), select.getRow());

		test.waitForFullPageResponse();
	}

	@Override
	public void executeDataGridEdit(DataGridEdit edit) {
		// TODO Auto-generated method stub
	}

	private void dataGridGesture(Step step, String binding, Integer row) {
		SmartClientAutomationContext context = peek();
		String buttonIdentifier = step.getIdentifier(context);
		
		List<Locator> locators = context.getLocators(buttonIdentifier);
		if (locators == null) {
			throw new MetaDataException(String.format("<%s /> with binding [%s] is not on the view", step.getClass()
					.getSimpleName(), binding));
		}

		boolean success = false;

		for (Locator locator : locators) {
			if (success) {
				continue;
			}

			if (row != null) {
				if (step instanceof DataGridZoom) {
					test.trace(String.format("Zoom on row %d on data grid [%s]", row, binding));
				} else if (step instanceof DataGridSelect) {
					test.trace(String.format("Select on row %d on list grid [%s]", row, binding));
				} else {
					test.trace(String.format("Remove on row %d on data grid [%s]", row, binding));
				}
			} else {
				test.trace(String.format("New row on data grid [%s]", binding));
			}

			if (step instanceof DataGridNew) {
				success = test.dataGridButton(locator.getLocator(), false);
			} else {
				if (row == null) {
					throw new MetaDataException("No row defined in DataGridSelect");
				}

				if (step instanceof DataGridSelect) {
					success = test.dataGridSelect(locator.getLocator(), row.intValue());
				} else {
					// Must select row before completing zoom or remove actions
					DataGridSelect select = new DataGridSelect();
					select.setBinding(binding);
					select.setRow(row);

					executeDataGridSelect(select);

					if (step instanceof DataGridRemove) {
						success = test.dataGridButton(locator.getLocator(), true);
					} else {
						success = test.dataGridButton(locator.getLocator(), false);
					}
				}
			}
		}

		if (!success) {
			throw new DomainException("Data grid gesture failed");
		}

		// Determine the document of the edit view to push
		if (step instanceof DataGridNew || step instanceof DataGridZoom) {
			User u = CORE.getUser();
			Customer c = u.getCustomer();
			Module m = c.getModule(context.getModuleName());
			Document d = m.getDocument(c, context.getDocumentName());
			TargetMetaData target = BindUtil.getMetaDataForBinding(c, m, d, binding);
			Relation relation = (Relation) target.getAttribute();

			if (relation != null) {
				String newDocumentName = relation.getDocumentName();
				d = m.getDocument(c, newDocumentName);
				String newModuleName = d.getOwningModuleName();
				
				// Push it
				PushEditContext push = new PushEditContext();
				push.setModuleName(newModuleName);
				push.setDocumentName(newDocumentName);
				push.execute(this, ExecutionOptions.windowed());
			}
		}
	}

	@Override
	public void executeListGridNew(ListGridNew nu) {
		listGridGesture(nu, null);
		
		PushEditContext push = listGridContext(nu.getQueryName(), nu.getDocumentName(), nu.getModelName(), nu);
		push.setCreateView(nu.getCreateView());
		push.execute(this, ExecutionOptions.windowed());

		test.waitForFullPageResponse();
	}

	@Override
	public void executeListGridZoom(ListGridZoom zoom) {
		listGridGesture(zoom, zoom.getRow());
		
		PushEditContext push = listGridContext(zoom.getQueryName(), zoom.getDocumentName(), zoom.getModelName(), zoom);
		push.execute(this, ExecutionOptions.windowed());

		test.waitForFullPageResponse();
	}

	@Override
	public void executeListGridSelect(ListGridSelect select) {
		listGridGesture(select, select.getRow());

		test.waitForFullPageResponse();
	}

	private void listGridGesture(Step step, Integer row) {
		SmartClientAutomationContext context = peek();
		String buttonIdentifier = step.getIdentifier(context);
		String listGridIdentifier = buttonIdentifier.substring(0, buttonIdentifier.lastIndexOf('.'));
		
		List<Locator> locators = context.getLocators(buttonIdentifier);
		if (locators == null) {
			throw new MetaDataException(String.format("<%s /> with identifier [%s] is not defined", step.getClass()
					.getSimpleName(), listGridIdentifier));
		}

		boolean success = false;

		for (Locator locator : locators) {
			if (success) {
				continue;
			}

			if (step instanceof ListGridNew) {
				test.trace(String.format("New on list grid [%s]", listGridIdentifier));
				success = test.listGridButton(locator.getLocator(), false);
			} else if (step instanceof ListGridSelect) {
				test.trace(String.format("Select on row %d on list grid [%s]", row, listGridIdentifier));
				success = test.listGridSelect(locator.getLocator(), row.intValue());
			} else if (step instanceof ListGridZoom zoom) {
				// Must select row before completing zoom action
				ListGridSelect select = new ListGridSelect();
				select.setModuleName(zoom.getModuleName());
				select.setDocumentName(zoom.getDocumentName());
				select.setModelName(zoom.getModelName());
				select.setQueryName(zoom.getQueryName());
				select.setRow(zoom.getRow());

				executeListGridSelect(select);

				test.trace(String.format("Zoom on row %d on list grid [%s]", row, listGridIdentifier));
				success = test.listGridButton(locator.getLocator(), false);
			}
		}

		if (!success) {
			throw new DomainException("List grid gesture failed");
		}
	}
	
	private PushEditContext listGridContext(String queryName, String documentName, String modelName, Step step) {
		PushEditContext result = new PushEditContext();

		SmartClientAutomationContext context = peek();

		if (ViewType.list.equals(context.getViewType())) {
			result.setModuleName(context.getModuleName());
			result.setDocumentName(context.getDocumentName());
		} else {
			String key = null;
			if (queryName != null) {
				key = queryName;
			} else if (documentName != null) {
				key = documentName;
			} else if (modelName != null) {
				key = modelName;
			}
			
			Customer c = CORE.getUser().getCustomer();
			Module m = c.getModule(context.getModuleName());
			MetaDataQueryDefinition q = m.getMetaDataQuery(key);

			if (q != null) {
				result.setModuleName(q.getDocumentModule(c).getName());
				result.setDocumentName(q.getDocumentName());
			} else {
				DocumentRef r = m.getDocumentRefs().get(key);
				if (r != null) {
					result.setModuleName(r.getReferencedModuleName());
					result.setDocumentName(key);
				} else {
					Document d = m.getDocument(c, context.getDocumentName());
					ListModel<Bean> lm = d.getListModel(c, key, true);
					if (lm != null) {
						d = lm.getDrivingDocument();
						result.setModuleName(d.getOwningModuleName());
						result.setDocumentName(d.getName());
					} else {
						throw new MetaDataException(
								String.format("<%s /> with identifier [%s] requires queryName, documentName or modelName defined", 
										step.getClass().getSimpleName(), step.getIdentifier(context)));
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

		if (TestStrategy.Assert == strategy) {
			test.trace("Asserting Success");
			test.assertSuccess();
		} else if (TestStrategy.Verify == strategy) {
			test.trace("Verifying Success");
			test.verifySuccess();

			test.okIfPresent();
		} else {
			test.okIfPresent();
		}
	}
	
	@Override
	public void executeTestFailure(TestFailure failure) {
		TestStrategy strategy = getTestStrategy();

		// Only handle if strategy is not None
		if (TestStrategy.None != strategy) {
			String message = failure.getMessage();

			if (message == null) {
				comment("Test Failure");
			} else {
				test.trace(String.format("Test Failure with message '%s'", message));
			}

			if (TestStrategy.Verify == strategy) {
				// Verify failure, optionally with message
				if (message == null) {
					test.verifyFailure();
				} else {
					test.verifyFailure(message);
				}

				// Close potential error dialog to continue
				test.okIfPresent();
			} else {
				// Assert failure, optionally with message
				if (message == null) {
					test.assertFailure();
				} else {
					test.assertFailure(message);
				}
			}
		} else {
			// Close potential error dialog to continue
			test.okIfPresent();
		}
	}
}
