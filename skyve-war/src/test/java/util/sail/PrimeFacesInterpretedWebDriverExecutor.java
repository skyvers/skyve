package util.sail;

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
import org.skyve.domain.messages.DomainException;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.module.ModuleImpl;
import org.skyve.impl.metadata.view.ViewImpl;
import org.skyve.impl.sail.execution.PrimeFacesAutomationContext;
import org.skyve.impl.sail.execution.PrimeFacesGenerateEditContext;
import org.skyve.impl.sail.execution.PrimeFacesGenerateListContext;
import org.skyve.impl.sail.execution.TestDataEnterViewVisitor;
import org.skyve.impl.web.faces.pipeline.component.ComponentBuilder;
import org.skyve.impl.web.faces.pipeline.component.SkyveComponentBuilderChain;
import org.skyve.impl.web.faces.pipeline.layout.LayoutBuilder;
import org.skyve.impl.web.faces.pipeline.layout.ResponsiveLayoutBuilder;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.Relation;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.Module.DocumentRef;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;
import org.skyve.metadata.sail.execution.ExecutionOptions;
import org.skyve.metadata.sail.language.Step;
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
import org.skyve.metadata.view.View.ViewType;
import org.skyve.metadata.view.model.list.ListModel;
import org.skyve.util.Binder.TargetMetaData;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import jakarta.faces.component.UIComponent;
import util.sail.commands.impl.PrimeFacesButtonCommand;
import util.sail.commands.impl.PrimeFacesCheckboxCommand;
import util.sail.commands.impl.PrimeFacesDataGridButtonCommand;
import util.sail.commands.impl.PrimeFacesDataGridSelectCommand;
import util.sail.commands.impl.PrimeFacesListGridButtonCommand;
import util.sail.commands.impl.PrimeFacesListGridSelectCommand;
import util.sail.commands.impl.PrimeFacesLookupDescriptionByRowCommand;
import util.sail.commands.impl.PrimeFacesLookupDescriptionBySearchCommand;
import util.sail.commands.impl.PrimeFacesRadioCommand;
import util.sail.commands.impl.PrimeFacesSelectOneCommand;
import util.sail.commands.impl.PrimeFacesTabCommand;
import util.sail.commands.impl.PrimeFacesTextCommand;

/**
 * Executor for PrimeFaces-based UI automation, interpreting web steps
 * using a {@link PrimeFacesSelenide} instance and {@link PrimeFacesAutomationContext}.
 * 
 * @author mike
 */
public class PrimeFacesInterpretedWebDriverExecutor extends InterpretedWebDriverExecutor<PrimeFacesAutomationContext, PrimeFacesSelenide> {
	
	private ComponentBuilder componentBuilder;
	private LayoutBuilder layoutBuilder;
	
	public PrimeFacesInterpretedWebDriverExecutor(PrimeFacesSelenide selenide) {
		this(selenide, new SkyveComponentBuilderChain(), new ResponsiveLayoutBuilder());
	}
	
	public PrimeFacesInterpretedWebDriverExecutor(
			PrimeFacesSelenide selenide,
			ComponentBuilder componentBuilder,
			LayoutBuilder layoutBuilder) {
		this.selenide = selenide;
		this.componentBuilder = componentBuilder;
		this.layoutBuilder = layoutBuilder;
	}

	@Override
	public void executePushListContext(PushListContext push) {
		PrimeFacesAutomationContext newContext = new PrimeFacesAutomationContext();
		newContext(push, newContext);

		push(newContext);
		newContext.generate(new PrimeFacesGenerateListContext(push, componentBuilder));
	}

	@Override
	public void executePushEditContext(PushEditContext push, ExecutionOptions options) {
		PrimeFacesAutomationContext newContext = new PrimeFacesAutomationContext();
		newContext(push, newContext);

		push(newContext);
		newContext.generate(new PrimeFacesGenerateEditContext(push, componentBuilder, layoutBuilder));
	}
	
	@Override
	public void executeTabSelect(TabSelect tabSelect) {
		PrimeFacesAutomationContext context = peek();
		String identifier = tabSelect.getIdentifier(context);

		List<UIComponent> components = context.getFacesComponents(identifier);
		if (components == null) {
			throw new MetaDataException(
					String.format("<tabSelect /> with path [%s] is not valid or is not on the view", tabSelect.getTabPath()));
		}

		boolean success = false;
		for (UIComponent component : components) {
			String clientId = PrimeFacesAutomationContext.clientId(component);

			selenide.trace(String.format("click tab [%s] (%s)", tabSelect.getTabPath(), clientId));
			success = success || selenide.tab(new PrimeFacesTabCommand(clientId));
		}

		if (!success) {
			throw new DomainException("Tab not selected");
		}
	}

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
			steps.execute(this);
		}
	}

	@Override
	public void executeDataEnter(DataEnter dataEnter) {
		PrimeFacesAutomationContext context = peek();
		String identifier = dataEnter.getIdentifier(context);

		List<UIComponent> components = context.getFacesComponents(identifier);
		if (components == null) {
			throw new MetaDataException(
					String.format("<dataEnter /> with binding [%s] is not supported or is not on the view", identifier));
		}

		boolean success = false;
		for (UIComponent component : components) {
			String clientId = PrimeFacesAutomationContext.clientId(component);
			boolean text = component instanceof InputText
					|| component instanceof InputTextarea
					|| component instanceof Password
					|| component instanceof InputMask;
			boolean selectOne = component instanceof SelectOneMenu;
			boolean radio = component instanceof SelectOneRadio;
			boolean checkbox = component instanceof SelectBooleanCheckbox || component instanceof TriStateCheckbox;
			boolean _input = component instanceof Spinner || component instanceof DatePicker;
			
			// TODO implement colour picker testing
			if (component instanceof ColorPicker) {
				selenide.trace(String.format("Ignore colour picker %s (%s) for now", identifier, clientId));
				return;
			}
			
			// Never replace with a new line as chrome under test will trip the default button on the form
			String value = dataEnter.getValue();
			if (value != null) {
				value = value.replace("\n", " ");
			}
			
			// if exists and is not disabled
			selenide.trace(String.format("set %s (%s) to %s", identifier, clientId, value));

			if (checkbox) {
				success = success || selenide.checkbox(new PrimeFacesCheckboxCommand(clientId, Boolean.valueOf(value)));
			} else if (text) {
				success = success || selenide.text(new PrimeFacesTextCommand(clientId, value, false));
			} else if (_input) {
				success = success || selenide._input(clientId, value, false);
			} else if (selectOne) {
				success = success || selenide.selectOne(new PrimeFacesSelectOneCommand(clientId, Integer.parseInt(value)));
			} else if (radio) {
				success = success || selenide.radio(new PrimeFacesRadioCommand(clientId, Integer.parseInt(value)));
			}
		}

		if (!success) {
			throw new DomainException("Data entry failed");
		}
	}

	private void button(Step button, String tagName, boolean ajax, boolean confirm, Boolean testSuccess) {
		PrimeFacesAutomationContext context = peek();
		String identifier = button.getIdentifier(context);

		List<UIComponent> components = context.getFacesComponents(identifier);
		if (components == null) {
			throw new MetaDataException(String.format("<%s /> is not on the view", tagName));
		}

		boolean success = false;
		for (UIComponent component : components) {
			String clientId = PrimeFacesAutomationContext.clientId(component);

			selenide.trace(String.format("click [%s] (%s)", tagName, clientId));
			boolean successful = selenide.button(new PrimeFacesButtonCommand(clientId, ajax, confirm));

			if (successful && !Boolean.FALSE.equals(testSuccess)) { // true or null (defaults on)
				executeTestSuccess(new TestSuccess());
			}

			success = success || successful;
		}

		if (!success) {
			throw new DomainException("Could not click button");
		}
	}

	private void redirectButton(Step button, String tagName, boolean confirm, Boolean testSuccess) {
		PrimeFacesAutomationContext context = peek();
		String identifier = button.getIdentifier(context);

		List<UIComponent> components = context.getFacesComponents(identifier);
		if (components == null) {
			throw new MetaDataException(String.format("<%s /> is not on the view", tagName));
		}

		boolean success = false;
		for (UIComponent component : components) {
			String clientId = PrimeFacesAutomationContext.clientId(component);

			selenide.trace(String.format("click [%s] (%s)", tagName, clientId));
			boolean successful = selenide.redirectButton(clientId, confirm);

			if (successful && !Boolean.FALSE.equals(testSuccess)) { // true or null (defaults on)
				executeTestSuccess(new TestSuccess());
			}

			success = success || successful;
		}

		if (!success) {
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

		Boolean createView = save.getCreateView();
		if (Boolean.TRUE.equals(createView)) {
			PrimeFacesAutomationContext context = peek();
			context.setViewType(ViewType.create);
		} else if (Boolean.FALSE.equals(createView)) {
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
		button(action, action.getActionName(), true, Boolean.TRUE.equals(action.getConfirm()), action.getTestSuccess());
	}

	@Override
	public void executeLookupDescriptionAutoComplete(LookupDescriptionAutoComplete complete) {
		lookupDescription(complete, complete.getBinding(), null, complete.getSearch());
	}

	@Override
	public void executeLookupDescriptionPick(LookupDescriptionPick pick) {
		lookupDescription(pick, pick.getBinding(), pick.getRow(), null);
	}

	@Override
	public void executeLookupDescriptionNew(LookupDescriptionNew nu) {
		throw new IllegalStateException("This interaction is unavailable in PrimeFaces");
	}

	@Override
	public void executeLookupDescriptionEdit(LookupDescriptionEdit edit) {
		throw new IllegalStateException("This interaction is unavailable in PrimeFaces");
	}

	private void lookupDescription(Step step, String binding, Integer row, String search) {
		PrimeFacesAutomationContext context = peek();
		
		List<UIComponent> lookupComponents = context.getFacesComponents(binding);
		if (lookupComponents == null) {
			throw new MetaDataException(String.format("<%s /> with binding [%s] is not on the view.",
					step.getClass().getSimpleName(),
					binding));
		}
		
		boolean success = false;
		for (UIComponent lookupComponent : lookupComponents) {
			String clientId = PrimeFacesAutomationContext.clientId(lookupComponent);
			if (row != null) {
				selenide.trace(String.format("Pick on row %d on lookup description [%s] (%s)", row, binding, clientId));
				success = success || selenide.lookupDescriptionByRow(new PrimeFacesLookupDescriptionByRowCommand(clientId, row.intValue()));
			} else {
				selenide.trace(String.format("Auto complete with search '%s' on lookup description [%s] (%s)", search, binding, clientId));
				success = success || selenide.lookupDescriptionBySearch(new PrimeFacesLookupDescriptionBySearchCommand(clientId, search));
			}
		}
	}

	@Override
	public void executeZoomIn(ZoomIn zoom) {
		button(zoom, "zoomIn", false, false, zoom.getTestSuccess());
		
		// Determine the document of the edit view to push
		PrimeFacesAutomationContext context = peek();

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
		throw new IllegalStateException("This interaction is unavailable in PrimeFaces");
	}

	private void dataGridGesture(Step step, String binding, Integer row) {
		PrimeFacesAutomationContext context = peek();
		String buttonIdentifier = step.getIdentifier(context);
		
		List<UIComponent> dataGridComponents = context.getFacesComponents(binding);
		if (dataGridComponents == null) {
			throw new MetaDataException(String.format("<%s /> with binding [%s] is not on the view", step.getClass()
					.getSimpleName(), binding));
		}

		for (UIComponent dataGridComponent : dataGridComponents) {
			String dataGridClientId = PrimeFacesAutomationContext.clientId(dataGridComponent);
			boolean ajax = false;

			if (row != null) {
				if (step instanceof DataGridZoom) {
					selenide.trace(String.format("Zoom on row %d on data grid [%s] (%s)", row, binding, dataGridClientId));
				} else if (step instanceof DataGridSelect) {
					selenide.trace(String.format("Select on row %d on list grid [%s] (%s)", row, binding, dataGridClientId));
				} else {
					ajax = true;
					selenide.trace(String.format("Remove on row %d on data grid [%s] (%s)", row, binding, dataGridClientId));
				}
			} else {
				selenide.trace(String.format("New row on data grid [%s] (%s)", binding, dataGridClientId));
			}

			List<UIComponent> buttonComponents = context.getFacesComponents(buttonIdentifier);
			if (buttonComponents != null) {
				for (UIComponent buttonComponent : buttonComponents) {
					String buttonClientId = row != null
							? PrimeFacesAutomationContext.clientId(buttonComponent, row)
							: PrimeFacesAutomationContext.clientId(buttonComponent);
					if (buttonClientId.startsWith(dataGridClientId)) {
						if (step instanceof DataGridSelect) {
							if (row == null) {
								throw new MetaDataException("No row defined in DataGridSelect");
							}
							selenide.dataGridSelect(new PrimeFacesDataGridSelectCommand(dataGridClientId, row.intValue()));
						} else {
							selenide.dataGridButton(new PrimeFacesDataGridButtonCommand(dataGridClientId, buttonClientId, ajax));
						}
					}
				}
			}
		}

		// Determine the document of the edit view to push
		if (step instanceof DataGridNew || step instanceof DataGridZoom) {
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
			throw new MetaDataException(String.format("<%s /> with identifier [%s] is not defined", step.getClass()
					.getSimpleName(), listGridIdentifier));
		}

		for (UIComponent listGridComponent : listGridComponents) {
			List<UIComponent> buttonComponents = context.getFacesComponents(buttonIdentifier);
			if (buttonComponents != null) {
				String listGridClientId = PrimeFacesAutomationContext.clientId(listGridComponent);

				if (row != null) {
					if (step instanceof ListGridZoom) {
						selenide.trace(
								String.format("Zoom on row %d on list grid [%s] (%s)", row, listGridIdentifier, listGridClientId));
					} else if (step instanceof ListGridSelect) {
						selenide.trace(String.format("Select on row %d on list grid [%s] (%s)", row, listGridIdentifier, listGridClientId));
					} else {
						selenide.trace(String.format("Delete on row %d on list grid [%s] (%s)", row, listGridIdentifier, listGridClientId));
					}
				} else {
					selenide.trace(String.format("New row on list grid [%s] (%s)", listGridIdentifier, listGridClientId));
				}

				for (UIComponent buttonComponent : buttonComponents) {
					String buttonClientId = row != null
							? PrimeFacesAutomationContext.clientId(buttonComponent, row)
							: PrimeFacesAutomationContext.clientId(buttonComponent);
					if (buttonClientId.startsWith(listGridClientId)) {
						if (step instanceof ListGridSelect) {
							if (row == null) {
								throw new MetaDataException("No row defined in ListGridSelect");
							}

							selenide.listGridSelect(new PrimeFacesListGridSelectCommand(listGridClientId, row.intValue()));
						} else {
							selenide.listGridButton(new PrimeFacesListGridButtonCommand(listGridClientId, buttonClientId, false));
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
		} else {
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
}
