package util.sail;

import java.util.List;

import org.apache.commons.lang3.BooleanUtils;
import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.messages.DomainException;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.module.ModuleImpl;
import org.skyve.impl.metadata.view.ViewImpl;
import org.skyve.impl.sail.execution.TestDataEnterViewVisitor;
import org.skyve.impl.sail.execution.sc.Locator;
import org.skyve.impl.sail.execution.sc.SmartClientAutomationContext;
import org.skyve.impl.sail.execution.sc.SmartClientGenerateEditContext;
import org.skyve.impl.sail.execution.sc.SmartClientGenerateListContext;
import org.skyve.impl.sail.execution.sc.Locator.InputType;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.Relation;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.Module.DocumentRef;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;
import org.skyve.metadata.sail.execution.ExecutionOptions;
import org.skyve.metadata.sail.language.Step;
import org.skyve.metadata.sail.language.step.TestFailure;
import org.skyve.metadata.sail.language.step.TestSuccess;
import org.skyve.metadata.sail.language.step.TestValue;
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
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.View;
import org.skyve.metadata.view.View.ViewType;
import org.skyve.metadata.view.model.list.ListModel;
import org.skyve.util.Binder.TargetMetaData;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import util.sail.commands.impl.SmartClientButtonCommand;
import util.sail.commands.impl.SmartClientCheckboxCommand;
import util.sail.commands.impl.SmartClientDataGridButtonCommand;
import util.sail.commands.impl.SmartClientDataGridSelectCommand;
import util.sail.commands.impl.SmartClientListGridButtonCommand;
import util.sail.commands.impl.SmartClientListGridSelectCommand;
import util.sail.commands.impl.SmartClientLookupDescriptionByRowCommand;
import util.sail.commands.impl.SmartClientLookupDescriptionBySearchCommand;
import util.sail.commands.impl.SmartClientRadioCommand;
import util.sail.commands.impl.SmartClientSelectOneCommand;
import util.sail.commands.impl.SmartClientTabCommand;
import util.sail.commands.impl.SmartClientTextCommand;

/**
 * Executor for SmartClient-based UI automation, interpreting web steps
 * using a {@link SmartClientSelenide} instance and {@link SmartClientAutomationContext}.
 * 
 * @author simeonsolomou
 */
public class SmartClientInterpretedWebDriverExecutor extends InterpretedWebDriverExecutor<SmartClientAutomationContext, SmartClientSelenide> {

	private static List<ImplicitActionName> EXCLUDED_IMPLICIT_ACTIONS = List.of(ImplicitActionName.Upload);

	public SmartClientInterpretedWebDriverExecutor(SmartClientSelenide selenide) {
		this.selenide = selenide;
	}

	@Override
	public void executePushListContext(PushListContext push) {
		SmartClientAutomationContext newContext = new SmartClientAutomationContext();
		newContext(push, newContext);

		push(newContext);
		newContext.generate(new SmartClientGenerateListContext(push));

		selenide.waitForFullPageResponse();
	}

	@Override
	public void executePushEditContext(PushEditContext push, ExecutionOptions options) {
		SmartClientAutomationContext newContext = new SmartClientAutomationContext();
		newContext(push, newContext);

		push(newContext);
		newContext.generate(new SmartClientGenerateEditContext(push, options.isWindowed()));

		selenide.waitForFullPageResponse();
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

			selenide.trace(String.format("click tab [%s] (%s)", tabSelect.getTabPath(), locator));
			success = selenide.tab(new SmartClientTabCommand(locator.getLocator()));
		}

		if (!success) {
			throw new DomainException("Tab not selected");
		}

		selenide.waitForFullPageResponse();
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

			selenide.trace(String.format("set %s to %s", identifier, value));

			if (InputType.CHECKBOX == inputType) {
				success = success || selenide.checkbox(new SmartClientCheckboxCommand(locator.getLocator(), Boolean.valueOf(value)));
			} else if (InputType.COMBO == inputType) {
				success = success || selenide.selectOne(new SmartClientSelectOneCommand(locator.getLocator(), Integer.parseInt(value)));
			} else if (InputType.TEXT == inputType) {
				success = success || selenide.text(new SmartClientTextCommand(locator.getLocator(), value, false));
			} else if (InputType.RADIO == inputType) {
				success = success || selenide.radio(new SmartClientRadioCommand(locator.getLocator(), Integer.parseInt(value)));
			}
		}

		if (!success) {
			throw new DomainException("Data entry failed");
		}

		selenide.waitForFullPageResponse();
	}

	/**
	 * Attempts to click a button identified by the given {@link Step} and tag name within the current UI context.
	 * 
	 * @param button the Step object representing the button to click
	 * @param tagName the XML tag name of the button action
	 * @param confirm whether to confirm the button click action
	 * @param testSuccess optional flag to control triggering test success actions; if null or true, test success is executed
	 */
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

			selenide.trace(String.format("click [%s]", tagName));
			success = selenide.button(new SmartClientButtonCommand(locator.getLocator(), confirm));

			if (success) {
				if (BooleanUtils.isNotFalse(testSuccess)) {
					executeTestSuccess(new TestSuccess());
				} else {
					selenide.ok();
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

		selenide.waitForFullPageResponse();
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

		selenide.waitForFullPageResponse();
	}

	@Override
	public void executeCancel(Cancel cancel) {
		button(cancel, "cancel", false, cancel.getTestSuccess());
		pop();

		selenide.waitForFullPageResponse();
	}

	@Override
	public void executeDelete(Delete delete) {
		button(delete, "delete", true, delete.getTestSuccess());
		pop();

		selenide.waitForFullPageResponse();
	}

	@Override
	public void executeZoomOut(ZoomOut zoomOut) {
		button(zoomOut, "zoom out", false, zoomOut.getTestSuccess());
		pop();

		SmartClientAutomationContext.decrementWindowNumber();

		selenide.waitForFullPageResponse();
	}

	@Override
	public void executeRemove(Remove remove) {
		button(remove, "remove", true, remove.getTestSuccess());
		pop();

		selenide.waitForFullPageResponse();
	}

	@Override
	public void executeAction(Action action) {
		button(action, action.getActionName(), Boolean.TRUE.equals(action.getConfirm()), action.getTestSuccess());

		selenide.waitForFullPageResponse();
	}

	@Override
	public void executeLookupDescriptionAutoComplete(LookupDescriptionAutoComplete complete) {
		lookupDescription(complete, complete.getBinding(), null, complete.getSearch());

		selenide.waitForFullPageResponse();
	}

	@Override
	public void executeLookupDescriptionPick(LookupDescriptionPick pick) {
		lookupDescription(pick, pick.getBinding(), pick.getRow(), null);

		selenide.waitForFullPageResponse();
	}

	/**
	 * Performs a lookup description action on UI elements identified by the given binding.
	 *
	 * @param step the Step object representing the current automation step
	 * @param binding the binding string used to identify UI elements
	 * @param row optional row index for row-based lookup; if null, search parameter is used
	 * @param search optional search text for autocomplete lookup; used if row is null
	 */
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
				selenide.trace(String.format("Pick on row %d on lookup description [%s]", row, binding));
				success = success || selenide.lookupDescriptionByRow(new SmartClientLookupDescriptionByRowCommand(locator.getLocator(), row.intValue()));
			} else {
				selenide.trace(String.format("Auto complete with search '%s' on lookup description [%s]", search, binding));
				success = success || selenide.lookupDescriptionBySearch(new SmartClientLookupDescriptionBySearchCommand(locator.getLocator(), search));
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

		selenide.waitForFullPageResponse();
	}
	
	/**
	 * Executes a new action on lookup descriptions for UI elements identified by the given binding.
	 * 
	 * @param step the Step object representing the current automation step
	 * @param binding the binding string used to identify UI elements
	 */
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

			selenide.trace(String.format("New on lookup description [%s]", binding));
			success = selenide.lookupDescriptionNew(locator.getLocator());
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

		selenide.waitForFullPageResponse();
	}

	/**
	 * Performs an edit action on lookup descriptions for UI elements identified by the given binding.
	 * 
	 * @param step the Step object representing the current automation step
	 * @param binding the binding string used to identify UI elements
	 */
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

			selenide.trace(String.format("Edit on lookup description [%s]", binding));
			success = selenide.lookupDescriptionEdit(locator.getLocator());
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

		selenide.waitForFullPageResponse();
	}
	
	@Override
	public void executeDataGridNew(DataGridNew nu) {
		dataGridGesture(nu, nu.getBinding(), null);

		selenide.waitForFullPageResponse();
	}
	
	@Override
	public void executeDataGridZoom(DataGridZoom zoom) {
		dataGridGesture(zoom, zoom.getBinding(), zoom.getRow());

		selenide.waitForFullPageResponse();
	}
	
	@Override
	public void executeDataGridRemove(DataGridRemove remove) {
		dataGridGesture(remove, remove.getBinding(), remove.getRow());

		selenide.waitForFullPageResponse();
		selenide.sleep(250, "Waiting for asynchronous DataGrid update");
	}

	@Override
	public void executeDataGridSelect(DataGridSelect select) {
		dataGridGesture(select, select.getBinding(), select.getRow());

		selenide.waitForFullPageResponse();
	}

	@Override
	public void executeDataGridEdit(DataGridEdit edit) {
		// TODO Auto-generated method stub
	}

	/**
	 * Executes a data grid gesture (new, select, zoom, or remove) on the UI element identified by the binding.
	 * 
	 * @param step the Step object representing the data grid action to perform
	 * @param binding the binding string used to identify the data grid UI elements
	 * @param row optional row index for row-specific gestures; required for select, zoom, and remove actions
	 */
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
					selenide.trace(String.format("Zoom on row %d on data grid [%s]", row, binding));
				} else if (step instanceof DataGridSelect) {
					selenide.trace(String.format("Select on row %d on list grid [%s]", row, binding));
				} else {
					selenide.trace(String.format("Remove on row %d on data grid [%s]", row, binding));
				}
			} else {
				selenide.trace(String.format("New row on data grid [%s]", binding));
			}

			if (step instanceof DataGridNew) {
				success = selenide.dataGridButton(new SmartClientDataGridButtonCommand(locator.getLocator(), false));
			} else {
				if (row == null) {
					throw new MetaDataException("No row defined in DataGridSelect");
				}

				if (step instanceof DataGridSelect) {
					success = selenide.dataGridSelect(new SmartClientDataGridSelectCommand(locator.getLocator(), row.intValue()));
				} else {
					// Must select row before completing zoom or remove actions
					DataGridSelect select = new DataGridSelect();
					select.setBinding(binding);
					select.setRow(row);

					executeDataGridSelect(select);

					if (step instanceof DataGridRemove) {
						success = selenide.dataGridButton(new SmartClientDataGridButtonCommand(locator.getLocator(), true));
					} else {
						success = selenide.dataGridButton(new SmartClientDataGridButtonCommand(locator.getLocator(), false));
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

		selenide.waitForFullPageResponse();
	}

	@Override
	public void executeListGridZoom(ListGridZoom zoom) {
		listGridGesture(zoom, zoom.getRow());
		
		PushEditContext push = listGridContext(zoom.getQueryName(), zoom.getDocumentName(), zoom.getModelName(), zoom);
		push.execute(this, ExecutionOptions.windowed());

		selenide.waitForFullPageResponse();
	}

	@Override
	public void executeListGridSelect(ListGridSelect select) {
		listGridGesture(select, select.getRow());

		selenide.waitForFullPageResponse();
	}

	/**
	 * Performs a list grid gesture (new, select, or zoom) on UI elements identified by the step's binding.
	 *
	 * @param step the Step object representing the list grid action to perform
	 * @param row the row index for row-specific actions; may be required depending on the step type
	 */
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
				selenide.trace(String.format("New on list grid [%s]", listGridIdentifier));
				success = selenide.listGridButton(new SmartClientListGridButtonCommand(locator.getLocator(), false));
			} else if (step instanceof ListGridSelect) {
				selenide.trace(String.format("Select on row %d on list grid [%s]", row, listGridIdentifier));
				success = selenide.listGridSelect(new SmartClientListGridSelectCommand(locator.getLocator(), row.intValue()));
			} else if (step instanceof ListGridZoom zoom) {
				// Must select row before completing zoom action
				ListGridSelect select = new ListGridSelect();
				select.setModuleName(zoom.getModuleName());
				select.setDocumentName(zoom.getDocumentName());
				select.setModelName(zoom.getModelName());
				select.setQueryName(zoom.getQueryName());
				select.setRow(zoom.getRow());

				executeListGridSelect(select);

				selenide.trace(String.format("Zoom on row %d on list grid [%s]", row, listGridIdentifier));
				success = selenide.listGridButton(new SmartClientListGridButtonCommand(locator.getLocator(), false));
			}
		}

		if (!success) {
			throw new DomainException("List grid gesture failed");
		}
	}
	
	/**
	 * Constructs a PushEditContext based on the current view type and provided identifiers.
	 * 
	 * @param queryName optional query name to resolve metadata
	 * @param documentName optional document name to resolve metadata
	 * @param modelName optional model name to resolve metadata
	 * @param step the Step object providing context for error reporting
	 * @return a PushEditContext with module and document names set appropriately
	 */
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
		super.executeTestSuccess(success);

		selenide.ok();
	}
	
	@Override
	public void executeTestFailure(TestFailure failure) {
		super.executeTestFailure(failure);

		selenide.ok();
	}
}
