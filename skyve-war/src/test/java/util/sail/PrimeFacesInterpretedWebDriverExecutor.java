package util.sail;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

import org.primefaces.component.autocomplete.AutoComplete;
import org.primefaces.component.colorpicker.ColorPicker;
import org.primefaces.component.datepicker.DatePicker;
import org.primefaces.component.inputmask.InputMask;
import org.primefaces.component.inputtext.InputText;
import org.primefaces.component.inputtextarea.InputTextarea;
import org.primefaces.component.password.Password;
import org.primefaces.component.selectbooleancheckbox.SelectBooleanCheckbox;
import org.primefaces.component.selectonemenu.SelectOneMenu;
import org.primefaces.component.selectoneradio.SelectOneRadio;
import org.primefaces.component.slider.Slider;
import org.primefaces.component.spinner.Spinner;
import org.primefaces.component.texteditor.TextEditor;
import org.primefaces.component.tristatecheckbox.TriStateCheckbox;
import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.messages.DomainException;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.module.ModuleImpl;
import org.skyve.impl.metadata.view.ViewImpl;
import org.skyve.impl.sail.execution.TestDataEnterViewVisitor;
import org.skyve.impl.sail.execution.pf.PrimeFacesAutomationContext;
import org.skyve.impl.sail.execution.pf.PrimeFacesGenerateEditContext;
import org.skyve.impl.sail.execution.pf.PrimeFacesGenerateListContext;
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
import org.skyve.metadata.sail.language.Step;
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
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateCalendar;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateEdit;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateLink;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateList;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateMap;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateTree;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.View.ViewType;
import org.skyve.metadata.view.model.list.ListModel;
import org.skyve.util.Binder.TargetMetaData;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.faces.component.UICommand;
import jakarta.faces.component.UIComponent;
import jakarta.faces.component.html.HtmlInputHidden;
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
	public void executePushEditContext(PushEditContext push) {
		PrimeFacesAutomationContext newContext = new PrimeFacesAutomationContext();
		newContext(push, newContext);

		push(newContext);
		newContext.generate(new PrimeFacesGenerateEditContext(push, componentBuilder, layoutBuilder));
	}
	
	/**
	 * Opens the list and waits for it to load.
	 *
	 * @param list	the step
	 */
	@Override
	public void executeNavigateList(NavigateList list) {
		super.executeNavigateList(list);
		selenide.waitForAjaxResponse();
	}

	/**
	 * Opens the edit view and waits for it to load.
	 *
	 * @param edit	the step
	 */
	@Override
	public void executeNavigateEdit(NavigateEdit edit) {
		super.executeNavigateEdit(edit);
		selenide.waitForAjaxResponse();
	}

	/**
	 * Opens the map ({@code external/map.xhtml}) and waits for it to load.
	 *
	 * @param map	the step
	 */
	@Override
	public void executeNavigateMap(NavigateMap map) {
		super.executeNavigateMap(map);
		selenide.waitForAjaxResponse();
	}

	/**
	 * Opens the link and waits for it to load.
	 *
	 * @param link	the step
	 */
	@Override
	public void executeNavigateLink(NavigateLink link) {
		super.executeNavigateLink(link);
		selenide.waitForAjaxResponse();
	}

	/**
	 * Fails: the PrimeFaces UX/UIs have no tree page (the router has no route for {@code a=t}, so it shows the home page).
	 *
	 * @param tree	the step
	 */
	@Override
	public void executeNavigateTree(NavigateTree tree) {
		throw new UnsupportedOperationException("<navigateTree /> is not available in PrimeFaces: " +
													"the router has no tree page (a=t) for the PrimeFaces UX/UIs");
	}

	/**
	 * Fails: the PrimeFaces UX/UIs have no calendar page (the router has no route for {@code a=c}, so it shows the home page).
	 *
	 * @param calendar	the step
	 */
	@Override
	public void executeNavigateCalendar(NavigateCalendar calendar) {
		throw new UnsupportedOperationException("<navigateCalendar /> is not available in PrimeFaces: " +
													"the router has no calendar page (a=c) for the PrimeFaces UX/UIs");
	}

	@Override
	public void executeTabSelect(TabSelect tabSelect) {
		PrimeFacesAutomationContext context = peek();
		@SuppressWarnings("null") // uses a nullable result where the API expects non-null
		String identifier = tabSelect.getIdentifier(context);

		@SuppressWarnings("null") // a null here fails the test
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
		@SuppressWarnings("null") // a null here fails the test; uses a nullable result where the API expects non-null
		Module m = c.getModule(context.getModuleName());
		@SuppressWarnings("null") // uses a nullable result where the API expects non-null
		Document d = m.getDocument(c, context.getDocumentName());
		
		Bean bean = null;

		String fixture = testDataEnter.getFixture();
		if (fixture == null) {
			bean = new DataBuilder().fixture(FixtureType.sail).build(d);
		} else {
			bean = new DataBuilder().fixture(fixture).build(d);
		}

		final String uxui = context.getUxui();
		@SuppressWarnings("null") // a null here fails the test
		ViewImpl view = (ViewImpl) d.getView(uxui, c, context.getViewType().toString());

		@SuppressWarnings("null") // uses a nullable result where the API expects non-null
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
	 * Enters a value into the first enabled, displayed widget for the binding. Combos and radios take a zero-based index,
	 * check boxes take true or false, sliders take a number, rich text and HTML editors take plain text,
	 * and other widgets take the text a user would type.
	 *
	 * @param dataEnter	the step
	 */
	@SuppressWarnings("null") // checks values the annotations say are non-null
	@Override
	public void executeDataEnter(DataEnter dataEnter) {
		PrimeFacesAutomationContext context = peek();
		String identifier = dataEnter.getIdentifier(context);

		List<UIComponent> components = context.getFacesComponents(identifier);
		if (components == null) {
			throw new MetaDataException(
					String.format("<dataEnter /> with binding [%s] is not supported or is not on the view", identifier));
		}

		// Never replace with a new line as chrome under test will trip the default button on the form
		String value = dataEnter.getValue();
		if (value != null) {
			value = value.replace("\n", " ");
		}

		boolean success = false;
		for (UIComponent component : components) {
			String clientId = PrimeFacesAutomationContext.clientId(component);
			selenide.trace(String.format("set %s (%s) to %s", identifier, clientId, value));

			if (component instanceof SelectBooleanCheckbox || component instanceof TriStateCheckbox) {
				success = selenide.checkbox(new PrimeFacesCheckboxCommand(clientId, Boolean.valueOf(value)));
			} else if (component instanceof ColorPicker) {
				success = selenide.colourPicker(clientId, value);
			} else if (component instanceof InputText
					|| component instanceof InputTextarea
					|| component instanceof Password
					|| component instanceof InputMask) {
				success = selenide.text(new PrimeFacesTextCommand(clientId, value, false));
			} else if (component instanceof Spinner || component instanceof DatePicker) {
				success = selenide._input(clientId, value, false);
			} else if (component instanceof SelectOneMenu) {
				success = selenide.selectOne(new PrimeFacesSelectOneCommand(clientId, index(identifier, value)));
			} else if (component instanceof SelectOneRadio) {
				success = selenide.radio(new PrimeFacesRadioCommand(clientId, index(identifier, value)));
			} else if (component instanceof TextEditor) {
				success = selenide.textEditor(clientId, value);
			} else if (component instanceof AutoComplete) {
				throw new MetaDataException(String.format("<dataEnter /> with binding [%s] is a lookup description: " +
															"use <lookupDescriptionAutoComplete /> or <lookupDescriptionPick />",
															identifier));
			} else {
				Slider slider = findChild(component, Slider.class);
				if (slider == null) {
					throw new MetaDataException(String.format("<dataEnter /> with binding [%s] is a %s, which PrimeFaces SAIL cannot enter data into",
																identifier,
																component.getClass().getSimpleName()));
				}
				success = selenide.slider(PrimeFacesAutomationContext.clientId(slider), value);
			}

			if (success) {
				break;
			}
		}

		if (!success) {
			throw new DomainException(String.format("Data entry failed: binding [%s] has no displayed, enabled widget to enter '%s' into",
														identifier,
														value));
		}
	}

	/**
	 * Parses a combo or radio index.
	 */
	private static int index(@Nonnull String binding, @Nullable String value) {
		try {
			return Integer.parseInt(Objects.requireNonNull(value));
		}
		catch (@SuppressWarnings("unused") NumberFormatException | NullPointerException e) {
			throw new MetaDataException(String.format("<dataEnter /> with binding [%s] needs a zero-based index for a combo or radio, not '%s'",
														binding,
														value));
		}
	}

	/**
	 * Finds the first descendant (or the component itself) of the given type.
	 */
	private static @Nullable <T extends UIComponent> T findChild(@Nonnull UIComponent component, @Nonnull Class<T> type) {
		if (type.isInstance(component)) {
			return type.cast(component);
		}
		for (UIComponent child : component.getChildren()) {
			T result = findChild(child, type);
			if (result != null) {
				return result;
			}
		}
		return null;
	}

	/**
	 * Compares the value shown by each widget for the binding that is on the page with the expected value,
	 * honouring the test strategy. Text widgets compare their text; combos and radios match the selected
	 * option's zero-based index (as {@code <dataEnter />} takes), its value or its label; check boxes compare
	 * true, false or an empty value (unset); rich text and HTML editors match their plain text or their HTML;
	 * colours compare ignoring case.
	 *
	 * @param test	the step
	 */
	@SuppressWarnings("null") // a null here fails the test
	@Override
	public void executeTestValue(TestValue test) {
		if (isNoneStrategy()) {
			return;
		}
		PrimeFacesAutomationContext context = peek();
		String identifier = test.getIdentifier(context);

		List<UIComponent> components = context.getFacesComponents(identifier);
		if (components == null) {
			throw new MetaDataException(String.format("<testValue /> with binding [%s] is not supported or is not on the view", identifier));
		}

		String expected = test.getValue();
		expected = (expected == null) ? "" : expected.replace("\n", " ").trim();

		boolean found = false;
		for (UIComponent component : components) {
			String clientId = PrimeFacesAutomationContext.clientId(component);
			boolean ignoreCase = (component instanceof ColorPicker);
			List<String> actuals = actualValues(component, clientId);
			if (actuals == null) { // not on the page
				continue;
			}
			found = true;
			selenide.trace(String.format("test %s (%s) is %s", identifier, clientId, expected));

			boolean matched = false;
			for (String actual : actuals) {
				String trimmed = (actual == null) ? "" : actual.trim();
				if (ignoreCase ? trimmed.equalsIgnoreCase(expected) : trimmed.equals(expected)) {
					matched = true;
					break;
				}
			}
			final String expectedValue = expected;
			check(matched, () -> String.format("expected [%s] to be '%s' but it was %s", identifier, expectedValue, describeActuals(actuals)));
		}

		if (! found) {
			throw new DomainException(String.format("<testValue /> with binding [%s] has no widget on the page", identifier));
		}
	}

	private static @Nonnull String describeActuals(@Nonnull List<String> actuals) {
		if (actuals.size() == 1) {
			return "'" + actuals.get(0) + "'";
		}
		StringBuilder result = new StringBuilder(64);
		for (String actual : actuals) {
			result.append(result.isEmpty() ? "'" : " / '").append(actual).append('\'');
		}
		return result.toString();
	}

	/**
	 * Reads the representations of a widget's value that a {@code <testValue />} can match.
	 *
	 * @return the values, or null if the widget is not on the page
	 */
	private @Nullable List<String> actualValues(@Nonnull UIComponent component, @Nonnull String clientId) {
		List<String> result = new ArrayList<>(3);
		if (component instanceof SelectBooleanCheckbox || component instanceof TriStateCheckbox) {
			if (selenide.inputValue(clientId + "_input") == null) {
				return null;
			}
			Boolean value = selenide.checkboxValue(clientId);
			result.add((value == null) ? "" : value.toString());
		} else if (component instanceof SelectOneMenu) {
			List<String> values = selenide.selectOneValues(clientId);
			if (values == null) {
				return null;
			}
			result.addAll(values);
		} else if (component instanceof SelectOneRadio) {
			List<String> values = selenide.radioValues(clientId);
			if (values == null) {
				return null;
			}
			result.addAll(values);
		} else if (component instanceof TextEditor) {
			List<String> values = selenide.textEditorValues(clientId);
			if (values == null) {
				return null;
			}
			result.addAll(values);
		} else if (component instanceof Spinner || component instanceof DatePicker || component instanceof AutoComplete) {
			String value = selenide.inputValue(clientId + "_input");
			if (value == null) {
				return null;
			}
			result.add(value);
		} else {
			Slider slider = findChild(component, Slider.class);
			HtmlInputHidden hidden = (slider == null) ? null : findChild(component, HtmlInputHidden.class);
			String value = selenide.inputValue((hidden == null) ? clientId : PrimeFacesAutomationContext.clientId(hidden));
			if (value == null) {
				return null;
			}
			result.add(value);
		}
		return result;
	}

	/**
	 * Checks that the element with the client id shows the label of the component that this JVM built, so a click cannot
	 * land on another button. The ids are generated in order, so they shift when the page and this JVM's view differ
	 * (for example when the browser's user cannot see an action).
	 *
	 * @throws IllegalStateException	if the labels differ
	 */
	private void verifyLabel(@Nonnull UIComponent component, @Nonnull String clientId, @Nonnull String tagName) {
		if ((! (component instanceof UICommand)) || (component.getValueExpression("value") != null)) {
			return;
		}
		Object value = ((UICommand) component).getValue();
		if (! (value instanceof String expected)) {
			return;
		}
		expected = expected.replaceAll("<[^>]*>", "").replaceAll("\\s+", " ").trim();
		String actual = selenide.label(clientId);
		if (expected.isEmpty() || (actual == null) || actual.isEmpty()) {
			return;
		}
		if (! expected.equalsIgnoreCase(actual)) {
			throw new IllegalStateException(String.format("<%s /> expected the button %s to be '%s' but the page shows '%s': " +
															"the page's view differs from the view SAIL built (user permissions, customer or UX/UI)",
															tagName,
															clientId,
															expected,
															actual));
		}
	}

	/**
	 * Clicks the first clickable component for the step, waits, then checks for success unless the step's
	 * {@code testSuccess} is false.
	 */
	private void button(Step button, String tagName, boolean ajax, boolean confirm, Boolean testSuccess) {
		PrimeFacesAutomationContext context = peek();
		@SuppressWarnings("null") // uses a nullable result where the API expects non-null
		String identifier = button.getIdentifier(context);

		@SuppressWarnings("null") // a null here fails the test; uses a nullable result where the API expects non-null
		List<UIComponent> components = context.getFacesComponents(identifier);
		if (components == null) {
			throw new MetaDataException(String.format("<%s /> is not on the view", tagName));
		}

		boolean success = false;
		for (UIComponent component : components) {
			String clientId = PrimeFacesAutomationContext.clientId(component);

			selenide.trace(String.format("click [%s] (%s)", tagName, clientId));
			verifyLabel(component, clientId, tagName);
			success = selenide.button(new PrimeFacesButtonCommand(clientId, ajax, confirm));
			if (success) {
				break;
			}
		}

		if (!success) {
			throw new DomainException(String.format("Could not click button [%s]: it is not on the page or is disabled", tagName));
		}
		if (!Boolean.FALSE.equals(testSuccess)) { // true or null (defaults on)
			checkSuccess(String.format("expected no error messages after [%s]", tagName));
		}
	}

	/**
	 * Clicks the first clickable component for the step, waits for the next page, then checks for success unless
	 * the step's {@code testSuccess} is false.
	 */
	private void redirectButton(Step button, String tagName, boolean confirm, Boolean testSuccess) {
		PrimeFacesAutomationContext context = peek();
		@SuppressWarnings("null") // uses a nullable result where the API expects non-null
		String identifier = button.getIdentifier(context);

		@SuppressWarnings("null") // a null here fails the test; uses a nullable result where the API expects non-null
		List<UIComponent> components = context.getFacesComponents(identifier);
		if (components == null) {
			throw new MetaDataException(String.format("<%s /> is not on the view", tagName));
		}

		boolean success = false;
		for (UIComponent component : components) {
			String clientId = PrimeFacesAutomationContext.clientId(component);

			selenide.trace(String.format("click [%s] (%s)", tagName, clientId));
			verifyLabel(component, clientId, tagName);
			success = selenide.redirectButton(clientId, confirm);
			if (success) {
				break;
			}
		}

		if (!success) {
			throw new DomainException(String.format("Could not click button [%s]: it is not on the page or is disabled", tagName));
		}
		if (!Boolean.FALSE.equals(testSuccess)) { // true or null (defaults on)
			checkSuccess(String.format("expected no error messages after [%s]", tagName));
		}
	}

	@Override
	public void executeOk(Ok ok) {
		redirectButton(ok, "ok", false, ok.getTestSuccess());
		pop();
	}

	@SuppressWarnings("null") // a null here fails the test
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

	/**
	 * Fails: a PrimeFaces lookup description is an auto complete with a drop down, and has no New gesture.
	 *
	 * @param nu	the step
	 */
	@Override
	public void executeLookupDescriptionNew(LookupDescriptionNew nu) {
		throw new UnsupportedOperationException(String.format("<lookupDescriptionNew /> on [%s] is not available in PrimeFaces: " +
																"its lookup description is an auto complete with no New button",
																nu.getBinding()));
	}

	/**
	 * Fails: a PrimeFaces lookup description is an auto complete with a drop down, and has no Edit gesture.
	 *
	 * @param edit	the step
	 */
	@Override
	public void executeLookupDescriptionEdit(LookupDescriptionEdit edit) {
		throw new UnsupportedOperationException(String.format("<lookupDescriptionEdit /> on [%s] is not available in PrimeFaces: " +
																"its lookup description is an auto complete with no Edit button",
																edit.getBinding()));
	}

	private void lookupDescription(Step step, String binding, Integer row, String search) {
		PrimeFacesAutomationContext context = peek();
		
		@SuppressWarnings("null") // a null here fails the test
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

		if (! success) {
			throw new DomainException(String.format("<%s /> with binding [%s] has no lookup on the page",
														step.getClass().getSimpleName(),
														binding));
		}
	}

	@Override
	public void executeZoomIn(ZoomIn zoom) {
		button(zoom, "zoomIn", false, false, zoom.getTestSuccess());
		
		// Determine the document of the edit view to push
		PrimeFacesAutomationContext context = peek();

		String binding = zoom.getBinding();
		Customer c = CORE.getUser().getCustomer();
		@SuppressWarnings("null") // a null here fails the test; uses a nullable result where the API expects non-null
		Module m = c.getModule(context.getModuleName());
		@SuppressWarnings("null") // uses a nullable result where the API expects non-null
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

	/**
	 * Fails: PrimeFaces data grids have no row edit gesture. An inline grid shows its cells as inputs,
	 * and other grids zoom into a row ({@code <dataGridZoom />}).
	 *
	 * @param edit	the step
	 */
	@Override
	public void executeDataGridEdit(DataGridEdit edit) {
		throw new UnsupportedOperationException(String.format("<dataGridEdit /> on [%s] is not available in PrimeFaces: " +
																"its data grids have no row edit gesture; use <dataGridZoom />",
																edit.getBinding()));
	}

	private void dataGridGesture(Step step, String binding, Integer row) {
		PrimeFacesAutomationContext context = peek();
		@SuppressWarnings("null") // uses a nullable result where the API expects non-null
		String buttonIdentifier = step.getIdentifier(context);
		
		@SuppressWarnings("null") // a null here fails the test
		List<UIComponent> dataGridComponents = context.getFacesComponents(binding);
		if (dataGridComponents == null) {
			throw new MetaDataException(String.format("<%s /> with binding [%s] is not on the view", step.getClass()
					.getSimpleName(), binding));
		}

		boolean success = false;
		for (UIComponent dataGridComponent : dataGridComponents) {
			if (success) {
				break;
			}
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

			@SuppressWarnings("null") // uses a nullable result where the API expects non-null
			List<UIComponent> buttonComponents = context.getFacesComponents(buttonIdentifier);
			if (buttonComponents != null) {
				for (UIComponent buttonComponent : buttonComponents) {
					String buttonClientId = row != null
							? PrimeFacesAutomationContext.clientId(buttonComponent, row)
							: PrimeFacesAutomationContext.clientId(buttonComponent);
					if ((! success) && buttonClientId.startsWith(dataGridClientId)) {
						if (step instanceof DataGridSelect) {
							if (row == null) {
								throw new MetaDataException("No row defined in DataGridSelect");
							}
							success = selenide.dataGridSelect(new PrimeFacesDataGridSelectCommand(dataGridClientId, row.intValue()));
						} else {
							success = selenide.dataGridButton(new PrimeFacesDataGridButtonCommand(dataGridClientId, buttonClientId, ajax));
						}
					}
				}
			}
		}

		if (! success) {
			throw new DomainException(String.format("<%s /> with binding [%s] could not be done: the grid, %s or its button is not on the page or is disabled",
														step.getClass().getSimpleName(),
														binding,
														(row == null) ? "it" : "row " + row));
		}
		checkSuccess(String.format("expected no error messages after <%s />", step.getClass().getSimpleName()));

		// Determine the document of the edit view to push
		if (step instanceof DataGridNew || step instanceof DataGridZoom) {
			Customer c = CORE.getUser().getCustomer();
			@SuppressWarnings("null") // uses a nullable result where the API expects non-null
			Module m = c.getModule(context.getModuleName());
			@SuppressWarnings("null") // uses a nullable result where the API expects non-null
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
		@SuppressWarnings("null") // uses a nullable result where the API expects non-null
		String buttonIdentifier = step.getIdentifier(context);
		@SuppressWarnings("null") // a null here fails the test
		String listGridIdentifier = buttonIdentifier.substring(0, buttonIdentifier.lastIndexOf('.'));
		
		@SuppressWarnings("null") // a null here fails the test
		List<UIComponent> listGridComponents = context.getFacesComponents(listGridIdentifier);
		if (listGridComponents == null) {
			throw new MetaDataException(String.format("<%s /> with identifier [%s] is not defined", step.getClass()
					.getSimpleName(), listGridIdentifier));
		}

		boolean success = false;
		for (UIComponent listGridComponent : listGridComponents) {
			if (success) {
				break;
			}
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
					if ((! success) && buttonClientId.startsWith(listGridClientId)) {
						if (step instanceof ListGridSelect) {
							if (row == null) {
								throw new MetaDataException("No row defined in ListGridSelect");
							}

							success = selenide.listGridSelect(new PrimeFacesListGridSelectCommand(listGridClientId, row.intValue()));
						} else {
							success = selenide.listGridButton(new PrimeFacesListGridButtonCommand(listGridClientId, buttonClientId, false));
						}
					}
				}
			}
		}

		if (! success) {
			throw new DomainException(String.format("<%s /> on [%s] could not be done: the list, %s or its button is not on the page or is disabled",
														step.getClass().getSimpleName(),
														listGridIdentifier,
														(row == null) ? "it" : "row " + row));
		}
		checkSuccess(String.format("expected no error messages after <%s />", step.getClass().getSimpleName()));
	}
	
	@SuppressWarnings("null") // a null here fails the test; uses a nullable result where the API expects non-null; checks values the annotations say are non-null
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

}
