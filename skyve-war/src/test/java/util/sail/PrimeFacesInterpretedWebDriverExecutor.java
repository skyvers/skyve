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
import org.primefaces.component.picklist.PickList;
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
import org.skyve.impl.metadata.module.menu.AbstractDocumentOrQueryOrModelMenuItem;
import org.skyve.impl.metadata.module.menu.CalendarItem;
import org.skyve.impl.metadata.module.menu.EditItem;
import org.skyve.impl.metadata.module.menu.ListItem;
import org.skyve.impl.metadata.module.menu.MapItem;
import org.skyve.impl.metadata.module.menu.TreeItem;
import org.skyve.metadata.module.menu.MenuGroup;
import org.skyve.metadata.module.menu.MenuItem;
import org.skyve.metadata.sail.language.step.TestGridRows;
import org.skyve.metadata.sail.language.step.TestMessage;
import org.skyve.metadata.sail.language.step.TestWidget;
import org.skyve.metadata.sail.language.step.interaction.Upload;
import org.skyve.metadata.sail.language.step.interaction.session.SwitchMode;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateMenu;
import org.skyve.metadata.sail.language.step.interaction.navigation.TestMenu;
import org.skyve.impl.metadata.view.widget.bound.input.TextField;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGrid;
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
import org.skyve.metadata.sail.language.step.interaction.grids.GridRowStep;
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
			value = substituteRunToken(value);
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
			} else if (component instanceof PickList) {
				List<String> members = membershipItems(value);
				List<String> missing = selenide.pickList(clientId, members);
				if ((missing != null) && (! missing.isEmpty())) {
					throw new DomainException(String.format("<dataEnter /> list membership [%s] has no items %s", identifier, missing));
				}
				success = (missing != null);
			} else if ((component instanceof AutoComplete) && (widgetAt(context, identifier, components.indexOf(component)) instanceof TextField)) {
				// a text field with suggestions (complete="suggest") takes typed text
				success = selenide._input(clientId, value, false);
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
	 * Splits a membership value ({@code A|B}) into its item labels.
	 */
	private static @Nonnull List<String> membershipItems(@Nullable String value) {
		List<String> result = new ArrayList<>();
		if (value != null) {
			for (String item : value.split("\\|")) {
				String trimmed = item.trim();
				if (! trimmed.isEmpty()) {
					result.add(trimmed);
				}
			}
		}
		return result;
	}

	/**
	 * Returns the widget metadata collected with the component at the index, or null.
	 */
	private static @Nullable Object widgetAt(@Nonnull PrimeFacesAutomationContext context, @Nonnull String identifier, int index) {
		List<Object> widgets = context.getSkyveWidgets(identifier);
		return ((widgets == null) || (index < 0) || (index >= widgets.size())) ? null : widgets.get(index);
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
		expected = (expected == null) ? "" : substituteRunToken(expected).replace("\n", " ").trim();

		boolean found = false;
		for (UIComponent component : components) {
			String clientId = PrimeFacesAutomationContext.clientId(component);
			if (component instanceof PickList) {
				List<String> members = selenide.pickListMembers(clientId);
				if (members == null) {
					continue;
				}
				found = true;
				List<String> expectedMembers = membershipItems(expected);
				selenide.trace(String.format("test %s (%s) members are %s", identifier, clientId, expectedMembers));
				check(new java.util.HashSet<>(members).equals(new java.util.HashSet<>(expectedMembers)),
						() -> String.format("expected [%s] members to be %s but they are %s", identifier, expectedMembers, members));
				continue;
			}
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
	private boolean button(Step button, String tagName, boolean ajax, Boolean answer, Boolean testSuccess) {
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
			success = selenide.button(clientId, ajax, answer);
			if (success) {
				break;
			}
		}

		if (!success) {
			throw new DomainException(String.format("Could not click button [%s]: it is not on the page or is disabled", tagName));
		}
		if (selenide.wasDeclined()) {
			selenide.trace(String.format("[%s] was declined", tagName));
			return true;
		}
		if (!Boolean.FALSE.equals(testSuccess)) { // true or null (defaults on)
			checkSuccess(String.format("expected no error messages after [%s]", tagName));
		}
		return false;
	}

	/**
	 * Clicks the first clickable component for the step, waits for the next page, then checks for success unless
	 * the step's {@code testSuccess} is false.
	 */
	private boolean redirectButton(Step button, String tagName, Boolean answer, Boolean testSuccess) {
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
			success = selenide.redirectButton(clientId, answer);
			if (success) {
				break;
			}
		}

		if (!success) {
			throw new DomainException(String.format("Could not click button [%s]: it is not on the page or is disabled", tagName));
		}
		if (selenide.wasDeclined()) {
			selenide.trace(String.format("[%s] was declined", tagName));
			return true;
		}
		if (!Boolean.FALSE.equals(testSuccess)) { // true or null (defaults on)
			checkSuccess(String.format("expected no error messages after [%s]", tagName));
		}
		return false;
	}

	@Override
	public void executeOk(Ok ok) {
		if (! redirectButton(ok, "ok", ok.getConfirm(), ok.getTestSuccess())) {
			pop();
		}
	}

	@SuppressWarnings("null") // a null here fails the test
	@Override
	public void executeSave(Save save) {
		button(save, "save", true, save.getConfirm(), save.getTestSuccess());

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
		if (! button(cancel, "cancel", false, null, cancel.getTestSuccess())) {
			pop();
		}
	}

	@Override
	public void executeDelete(Delete delete) {
		if (! redirectButton(delete, "delete", delete.getConfirm(), delete.getTestSuccess())) {
			pop();
		}
	}

	@Override
	public void executeZoomOut(ZoomOut zoomOut) {
		if (! button(zoomOut, "zoom out", false, zoomOut.getConfirm(), zoomOut.getTestSuccess())) {
			pop();
		}
	}

	@Override
	public void executeRemove(Remove remove) {
		if (! button(remove, "remove", false, remove.getConfirm(), remove.getTestSuccess())) {
			pop();
		}
	}

	@Override
	public void executeAction(Action action) {
		button(action, action.getActionName(), true, action.getConfirm(), action.getTestSuccess());
	}

	@Override
	public void executeLookupDescriptionAutoComplete(LookupDescriptionAutoComplete complete) {
		lookupDescription(complete, complete.getBinding(), null, substituteRunToken(complete.getSearch()));
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
		if (button(zoom, "zoomIn", false, null, zoom.getTestSuccess())) {
			return;
		}
		// A refused zoom (e.g. validation errors, expected with testSuccess="false") stays in this view
		if (! selenide.verifySuccess()) {
			selenide.trace("zoom in was refused; staying in this view");
			return;
		}
		
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
		dataGridGesture(zoom, zoom.getBinding(), zoom);
	}
	
	@Override
	public void executeDataGridRemove(DataGridRemove remove) {
		dataGridGesture(remove, remove.getBinding(), remove);
	}

	/**
	 * Selects a row of a data grid that has a {@code selectedIdBinding}.
	 * Fails for other grids: in PrimeFaces, selecting a row of them zooms into it, so use {@code <dataGridZoom />}.
	 *
	 * @param select	the step
	 */
	@Override
	public void executeDataGridSelect(DataGridSelect select) {
		String binding = select.getBinding();
		PrimeFacesAutomationContext context = peek();
		@SuppressWarnings("null") // a null here fails the test
		List<Object> widgets = context.getSkyveWidgets(binding);
		if ((widgets != null) && (! widgets.isEmpty()) &&
				(widgets.get(0) instanceof DataGrid grid) && (grid.getSelectedIdBinding() == null)) {
			throw new UnsupportedOperationException(String.format("<dataGridSelect /> on [%s] is not available in PrimeFaces: " +
																	"the grid has no selectedIdBinding, so selecting a row zooms into it; use <dataGridZoom />",
																	binding));
		}
		dataGridGesture(select, binding, select);
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

	private void dataGridGesture(Step step, String binding, GridRowStep rowStep) {
		PrimeFacesAutomationContext context = peek();
		@SuppressWarnings("null") // uses a nullable result where the API expects non-null
		String buttonIdentifier = step.getIdentifier(context);
		
		@SuppressWarnings("null") // a null here fails the test
		List<UIComponent> dataGridComponents = context.getFacesComponents(binding);
		if (dataGridComponents == null) {
			throw new MetaDataException(String.format("<%s /> with binding [%s] is not on the view", step.getClass()
					.getSimpleName(), binding));
		}

		// An inline grid adds its row in place with ajax; other grids zoom into a new view
		List<Object> gridWidgets = context.getSkyveWidgets(binding);
		boolean inline = (gridWidgets != null) && (! gridWidgets.isEmpty()) &&
							(gridWidgets.get(0) instanceof DataGrid grid) && Boolean.TRUE.equals(grid.getInline());

		boolean success = false;
		for (UIComponent dataGridComponent : dataGridComponents) {
			if (success) {
				break;
			}
			String dataGridClientId = PrimeFacesAutomationContext.clientId(dataGridComponent);
			boolean ajax = inline;
			Integer row = null;
			if (rowStep != null) {
				row = resolveRow(rowStep, dataGridClientId);
				if (row == null) { // not on the page
					continue;
				}
			}

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
														describeRow(rowStep)));
		}
		checkSuccess(String.format("expected no error messages after <%s />", step.getClass().getSimpleName()));

		// Determine the document of the edit view to push
		if ((! inline) && (step instanceof DataGridNew || step instanceof DataGridZoom)) {
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

	/**
	 * Resolves a grid step's row: its {@code row}, or the first row on the page with a cell containing its
	 * {@code rowContaining} text (after {@code {run}} substitution).
	 *
	 * @return the zero-based row, or null if the grid is not on the page
	 * @throws DomainException	if no row contains the text
	 */
	private @Nullable Integer resolveRow(@Nonnull GridRowStep step, @Nonnull String gridClientId) {
		Integer row = step.getRow();
		if (row != null) {
			return row;
		}
		String text = substituteRunToken(Objects.requireNonNull(step.getRowContaining(), "rowContaining"));
		int result = selenide.rowContaining(gridClientId, text);
		if (result == -2) {
			return null;
		}
		if (result < 0) {
			throw new DomainException(String.format("Grid %s has no row containing '%s'", gridClientId, text));
		}
		selenide.trace(String.format("    row %d contains '%s'", Integer.valueOf(result), text));
		return Integer.valueOf(result);
	}

	private String describeRow(@Nullable GridRowStep step) {
		if (step == null) {
			return "it";
		}
		Integer row = step.getRow();
		return (row != null) ? "row " + row : "the row containing '" + substituteRunToken(String.valueOf(step.getRowContaining())) + "'";
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
		listGridGesture(zoom, zoom);
		
		PushEditContext push = listGridContext(zoom.getQueryName(), zoom.getDocumentName(), zoom.getModelName(), zoom);
		push.execute(this);
	}

	/**
	 * Selects a row of a list grid that has a {@code selectedIdBinding}.
	 * Fails for other lists: in PrimeFaces, selecting a row of them zooms into it, so use {@code <listGridZoom />}.
	 *
	 * @param select	the step
	 */
	@Override
	public void executeListGridSelect(ListGridSelect select) {
		PrimeFacesAutomationContext context = peek();
		@SuppressWarnings("null") // a null here fails the test
		String identifier = select.getIdentifier(context);
		@SuppressWarnings("null") // a null here fails the test
		List<Object> widgets = context.getSkyveWidgets(identifier);
		if ((widgets != null) && (! widgets.isEmpty()) &&
				(widgets.get(0) instanceof org.skyve.impl.metadata.view.widget.bound.tabular.ListGrid grid) &&
				(grid.getSelectedIdBinding() == null)) {
			throw new UnsupportedOperationException(String.format("<listGridSelect /> on [%s] is not available in PrimeFaces: " +
																	"the list has no selectedIdBinding, so selecting a row zooms into it; use <listGridZoom />",
																	identifier.substring(0, identifier.lastIndexOf('.'))));
		}
		listGridGesture(select, select);
	}

	private void listGridGesture(Step step, GridRowStep rowStep) {
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
			if ((buttonComponents == null) && (step instanceof ListGridZoom) && (rowStep != null)) {
				// No zoom button: the list zooms when a row is selected
				String listGridClientId = PrimeFacesAutomationContext.clientId(listGridComponent);
				Integer row = resolveRow(rowStep, listGridClientId);
				if (row != null) {
					selenide.trace(String.format("Zoom by selecting row %d on list grid [%s] (%s)", row, listGridIdentifier, listGridClientId));
					success = selenide.zoomRowBySelection(listGridClientId, row.intValue());
				}
				continue;
			}
			if (buttonComponents != null) {
				String listGridClientId = PrimeFacesAutomationContext.clientId(listGridComponent);
				Integer row = null;
				if (rowStep != null) {
					row = resolveRow(rowStep, listGridClientId);
					if (row == null) { // not on the page
						continue;
					}
				}

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
														describeRow(rowStep)));
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


	/**
	 * Uploads a test classpath file: into a content widget (through its upload page), or through an upload action's
	 * upload page. For a content widget, the widget must then hold content.
	 *
	 * @param upload	the step
	 */
	@SuppressWarnings("null") // a null here fails the test
	@Override
	public void executeUpload(Upload upload) {
		PrimeFacesAutomationContext context = peek();
		String identifier = upload.getIdentifier(context);
		List<UIComponent> components = context.getFacesComponents(identifier);
		if (components == null) {
			throw new MetaDataException(String.format("<upload /> with %s [%s] is not on the view",
														(upload.getBinding() != null) ? "binding" : "action", identifier));
		}
		java.io.File file = resourceFile(upload.getFile());

		boolean success = false;
		for (UIComponent component : components) {
			String clientId = PrimeFacesAutomationContext.clientId(component);
			selenide.trace(String.format("upload %s to [%s] (%s)", upload.getFile(), identifier, clientId));
			if (upload.getBinding() != null) {
				org.openqa.selenium.WebElement trigger = selenide.contentUploadTrigger(clientId);
				if ((trigger != null) && selenide.upload(trigger, file)) {
					String value = selenide.contentValue(clientId);
					if ((value == null) || value.isEmpty()) {
						throw new DomainException(String.format("<upload /> to [%s] finished but the widget holds no content", identifier));
					}
					success = true;
				}
			}
			else {
				success = selenide.upload(clientId, file);
			}
			if (success) {
				break;
			}
		}
		if (! success) {
			throw new DomainException(String.format("<upload /> to [%s]: no displayed, enabled widget to upload with", identifier));
		}
		checkSuccess(String.format("expected no error messages after uploading to [%s]", identifier));
	}

	/**
	 * Resolves a test classpath resource to a file the browser can be given.
	 */
	private static @Nonnull java.io.File resourceFile(@Nonnull String resource) {
		java.net.URL url = Thread.currentThread().getContextClassLoader().getResource(resource);
		if (url == null) {
			throw new MetaDataException("<upload /> file " + resource + " is not on the test classpath");
		}
		try {
			return new java.io.File(url.toURI());
		}
		catch (java.net.URISyntaxException | IllegalArgumentException e) {
			throw new MetaDataException("<upload /> file " + resource + " is not a file on disk: " + url, e);
		}
	}

	/**
	 * Asserts that a global message, growl or field message (of the type, if given) contains the text.
	 *
	 * @param test	the step
	 */
	@Override
	public void executeTestMessage(TestMessage test) {
		if (isNoneStrategy()) {
			return;
		}
		String contains = substituteRunToken(test.getContains());
		TestMessage.MessageType type = test.getType();
		String severity = (type == null) ? null : type.name();
		selenide.trace(String.format("test %s message contains '%s'", (severity == null) ? "a" : "an " + severity, contains));
		// No wait beyond the response: the messages of the last response are on the page once it has been applied
		selenide.waitForAjaxResponse();
		boolean found = selenide.messagesOfType(severity).stream().anyMatch(m -> m.contains(contains));
		check(found, () -> String.format("expected %s message containing '%s' but found: %s",
											(severity == null) ? "a" : "an " + severity, contains, selenide.describeMessages()));
	}

	/**
	 * Counts the rows of a list or data grid (only those containing the text, if given) and checks the bounds.
	 * With a timeout, a list grid is reloaded and the rows recounted until the bounds hold or the time is up.
	 *
	 * @param test	the step
	 */
	@SuppressWarnings("null") // a null here fails the test
	@Override
	public void executeTestGridRows(TestGridRows test) {
		if (isNoneStrategy()) {
			return;
		}
		PrimeFacesAutomationContext context = peek();
		String identifier = test.getIdentifier(context);
		List<UIComponent> components = context.getFacesComponents(identifier);
		if (components == null) {
			throw new MetaDataException(String.format("<testGridRows /> grid [%s] is not on the view", identifier));
		}
		String text = test.getRowContaining();
		final String contains = (text == null) ? null : substituteRunToken(text);
		int min = test.getEffectiveMinRows();
		Integer max = test.getMaxRows();
		Integer timeout = test.getTimeoutMillis();

		String gridId = null;
		for (UIComponent component : components) {
			String clientId = PrimeFacesAutomationContext.clientId(component);
			if (selenide.countRows(clientId, null) >= 0) {
				gridId = clientId;
				break;
			}
		}
		if (gridId == null) {
			throw new DomainException(String.format("<testGridRows /> grid [%s] is not on the page", identifier));
		}
		selenide.trace(String.format("test grid [%s] (%s) has %d..%s rows%s", identifier, gridId, Integer.valueOf(min),
										(max == null) ? "" : max.toString(), (contains == null) ? "" : " containing '" + contains + "'"));
		int count = selenide.countRows(gridId, contains);
		if ((timeout != null) && (! ((count >= min) && ((max == null) || (count <= max.intValue()))))) {
			// Author-requested (timeoutMillis): poll a server-side job's effect by reloading the list each second
			final String grid = gridId;
			try {
				selenide.waitFor(java.time.Duration.ofMillis(timeout.longValue()), java.time.Duration.ofSeconds(1), d -> {
					selenide.refreshGrid(grid);
					int n = selenide.countRows(grid, contains);
					return ((n >= min) && ((max == null) || (n <= max.intValue()))) ? Boolean.TRUE : null;
				});
			}
			catch (@SuppressWarnings("unused") org.openqa.selenium.TimeoutException e) {
				// reported below
			}
			count = selenide.countRows(gridId, contains);
		}
		final int rows = count;
		check((rows >= min) && ((max == null) || (rows <= max.intValue())),
				() -> String.format("expected grid [%s] to have %s%s rows%s but it has %d",
										identifier,
										Integer.valueOf(min),
										(max == null) ? " or more" : " to " + max,
										(contains == null) ? "" : " containing '" + contains + "'",
										Integer.valueOf(rows)));
	}

	/**
	 * Asserts that a widget, action or tab is (or is not) shown, and (if given) is (or is not) enabled.
	 * Not shown includes not rendered.
	 *
	 * @param test	the step
	 */
	@SuppressWarnings("null") // a null here fails the test
	@Override
	public void executeTestWidget(TestWidget test) {
		if (isNoneStrategy()) {
			return;
		}
		PrimeFacesAutomationContext context = peek();
		String identifier = test.getIdentifier(context);
		List<UIComponent> components = context.getFacesComponents(identifier);
		if (components == null) {
			throw new MetaDataException(String.format("<testWidget /> [%s] is not in the view", identifier));
		}
		boolean tab = (test.getTabPath() != null);
		boolean shown = false;
		boolean enabled = false;
		for (UIComponent component : components) {
			String clientId = PrimeFacesAutomationContext.clientId(component);
			List<Boolean> state = tab ? selenide.tabState(clientId) : selenide.widgetState(clientId);
			if (Boolean.TRUE.equals(state.get(0))) {
				shown = true;
				enabled = enabled || Boolean.TRUE.equals(state.get(1));
			}
		}
		Boolean expectedEnabled = test.getEnabled();
		final boolean isShown = shown;
		final boolean isEnabled = enabled;
		if (expectedEnabled != null) {
			selenide.trace(String.format("test [%s] is shown and %s", identifier, expectedEnabled.booleanValue() ? "enabled" : "disabled"));
			check(isShown && (isEnabled == expectedEnabled.booleanValue()),
					() -> String.format("expected [%s] to be shown and %s but it is %s",
											identifier,
											expectedEnabled.booleanValue() ? "enabled" : "disabled",
											(! isShown) ? "not shown" : (isEnabled ? "enabled" : "disabled")));
		}
		else {
			boolean expectedVisible = test.isExpectedVisible();
			selenide.trace(String.format("test [%s] is %s", identifier, expectedVisible ? "shown" : "not shown"));
			check(isShown == expectedVisible,
					() -> String.format("expected [%s] to be %s but it is %s",
											identifier,
											expectedVisible ? "shown" : "not shown",
											isShown ? "shown" : "not shown"));
		}
	}

	/**
	 * Clicks through the menu (module, groups, item) and sets the context as the equivalent navigate step would.
	 *
	 * @param menu	the step
	 */
	@Override
	public void executeNavigateMenu(NavigateMenu menu) {
		MenuPath path = menuPath(menu.getModuleName(), menu.getItemName());
		MenuItem item = path.item;
		if (item == null) {
			throw new MetaDataException(String.format("<navigateMenu /> module %s has no menu item %s", menu.getModuleName(), menu.getItemName()));
		}
		Customer c = CORE.getUser().getCustomer();
		Module m = c.getModule(menu.getModuleName());
		selenide.trace(String.format("Menu %s / %s", path.moduleTitle, String.join(" / ", path.labels())));
		if (item instanceof TreeItem || item instanceof CalendarItem) {
			throw new UnsupportedOperationException(String.format("<navigateMenu /> item %s is a %s, which PrimeFaces cannot show",
																	menu.getItemName(),
																	(item instanceof TreeItem) ? "tree" : "calendar"));
		}
		if (item instanceof ListItem || item instanceof MapItem) {
			AbstractDocumentOrQueryOrModelMenuItem listItem = (AbstractDocumentOrQueryOrModelMenuItem) item;
			PushListContext push = new PushListContext();
			push.setModuleName(m.getName());
			push.setDocumentName(listItem.getDocumentName());
			push.setQueryName(listItem.getQueryName());
			push.setModelName(listItem.getModelName());
			executePushListContext(push);
		}
		else if (item instanceof EditItem editItem) {
			Document d = m.getDocument(c, Objects.requireNonNull(editItem.getDocumentName()));
			PushEditContext push = new PushEditContext();
			push.setModuleName(d.getOwningModuleName());
			push.setDocumentName(d.getName());
			executePushEditContext(push);
		}
		else {
			PrimeFacesAutomationContext context = peek();
			if (context != null) {
				context.setDocumentName(null);
				context.setViewType(null);
			}
		}
		if (! selenide.menu(path.moduleTitle, path.groupLabels, path.itemLabel, true)) {
			throw new DomainException(String.format("<navigateMenu /> %s / %s is not in the menu", path.moduleTitle, String.join(" / ", path.labels())));
		}
	}

	/**
	 * Asserts that a module, or a menu item of it, is (or is not) in the menu.
	 *
	 * @param test	the step
	 */
	@Override
	public void executeTestMenu(TestMenu test) {
		if (isNoneStrategy()) {
			return;
		}
		String itemName = test.getItemName();
		MenuPath path = menuPath(test.getModuleName(), itemName);
		if ((itemName != null) && (path.item == null)) {
			throw new MetaDataException(String.format("<testMenu /> module %s has no menu item %s", test.getModuleName(), itemName));
		}
		boolean expected = ! Boolean.FALSE.equals(test.getPresent());
		boolean present = selenide.menu(path.moduleTitle, path.groupLabels, path.itemLabel, false);
		String what = (itemName == null) ? path.moduleTitle : path.moduleTitle + " / " + String.join(" / ", path.labels());
		selenide.trace(String.format("test menu %s is %s", what, expected ? "present" : "absent"));
		check(present == expected, () -> String.format("expected the menu %s %s but it %s", what,
														expected ? "to show" : "not to show", present ? "does" : "does not"));
	}

	/** A menu item and the labels shown for it. */
	private static final class MenuPath {
		String moduleTitle;
		List<String> groupLabels = new ArrayList<>();
		String itemLabel;
		MenuItem item;

		List<String> labels() {
			List<String> result = new ArrayList<>(groupLabels);
			if (itemLabel != null) {
				result.add(itemLabel);
			}
			return result;
		}
	}

	/**
	 * Resolves a {@code /}-separated menu path (group names then the item name; each the metadata name or the shown name).
	 */
	private static @Nonnull MenuPath menuPath(@Nonnull String moduleName, @Nullable String itemPath) {
		Customer c = CORE.getUser().getCustomer();
		Module m = c.getModule(moduleName);
		MenuPath result = new MenuPath();
		result.moduleTitle = m.getLocalisedTitle();
		if (itemPath == null) {
			return result;
		}
		String[] names = itemPath.split("/");
		List<MenuItem> items = m.getMenu().getItems();
		for (int i = 0; i < names.length; i++) {
			String name = names[i].trim();
			MenuItem found = null;
			for (MenuItem candidate : items) {
				String label = candidate.getLocalisedName();
				if (name.equals(candidate.getName()) || name.equals(label)) {
					found = candidate;
					break;
				}
			}
			if (found == null) {
				return result;
			}
			String label = found.getLocalisedName();
			if (label == null) {
				label = found.getName();
			}
			if (i < names.length - 1) {
				if (! (found instanceof MenuGroup group)) {
					return result;
				}
				result.groupLabels.add(label);
				items = group.getItems();
			}
			else {
				result.itemLabel = label;
				result.item = found;
			}
		}
		return result;
	}

	/**
	 * Switches the session to PrimeFaces, through SmartClient's header menu, if SmartClient is showing.
	 * Does nothing when PrimeFaces is already showing.
	 *
	 * @param switchMode	the step
	 */
	@Override
	public void executeSwitchMode(SwitchMode switchMode) {
		if ("desktop".equals(getCurrentUxUi())) {
			throw new UnsupportedOperationException("<switchMode /> to SmartClient (uxui desktop) is the SmartClient executor's job");
		}
		if (! selenide.smartClientShowing()) {
			selenide.trace("Switch mode: PrimeFaces is showing already");
			return;
		}
		selenide.trace("Switch mode: SmartClient to PrimeFaces");
		selenide.switchFromSmartClient();
		if (selenide.smartClientShowing()) {
			throw new DomainException("<switchMode /> chose Switch Mode but SmartClient is still showing");
		}
	}

	/**
	 * Runs the script, then waits for any request or page load it started.
	 *
	 * @param execute	the step
	 */
	@Override
	public void executeExecute(org.skyve.metadata.sail.language.step.Execute execute) {
		selenide.runScript(execute.getScript());
	}
}
