package util.sail;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

import org.primefaces.component.autocomplete.AutoComplete;
import org.primefaces.component.colorpicker.ColorPicker;
import org.primefaces.component.datatable.DataTable;
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
import org.skyve.domain.types.converters.Converter;
import org.skyve.impl.metadata.model.document.field.ConvertibleField;
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
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.document.DomainType;
import org.skyve.metadata.model.document.Relation;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.Module.DocumentRef;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;
import org.skyve.metadata.sail.language.Step;
import org.skyve.metadata.sail.language.step.CaptureCell;
import org.skyve.metadata.sail.language.step.CaptureTitle;
import org.skyve.metadata.sail.language.step.CaptureValue;
import org.skyve.metadata.sail.language.step.TestValue;
import org.skyve.metadata.sail.language.step.context.PushEditContext;
import org.skyve.metadata.sail.language.step.context.PushListContext;
import org.skyve.metadata.sail.language.step.interaction.DataEnter;
import org.skyve.metadata.sail.language.step.interaction.DataEnter.Match;
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
import org.skyve.metadata.sail.language.step.interaction.grids.DataGridLink;
import org.skyve.metadata.sail.language.step.interaction.grids.GridRowStep;
import org.skyve.metadata.sail.language.step.interaction.grids.DataGridNew;
import org.skyve.metadata.sail.language.step.interaction.grids.DataGridRemove;
import org.skyve.metadata.sail.language.step.interaction.grids.DataGridSelect;
import org.skyve.metadata.sail.language.step.interaction.grids.DataGridZoom;
import org.skyve.metadata.sail.language.step.interaction.grids.ListGridNew;
import org.skyve.metadata.sail.language.step.interaction.grids.ListGridClearFilter;
import org.skyve.metadata.sail.language.step.interaction.grids.ListGridFilter;
import org.skyve.metadata.sail.language.step.interaction.grids.ListGridFilter.FilterOperator;
import org.skyve.metadata.sail.language.step.interaction.grids.ListGridSort;
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
import util.sail.PrimeFacesSelenide.VueFilterInput;
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
		Match match = dataEnter.getEffectiveMatch();
		String matchValue = dataEnter.getValue();
		if ((match != Match.index) && (matchValue != null)) { // SAIL XML turns a blank value into null
			enterOption(dataEnter, substituteTokens(matchValue), match);
			return;
		}
		PrimeFacesAutomationContext context = peek();
		String identifier = dataEnter.getIdentifier(context);

		List<UIComponent> components = context.getFacesComponents(identifier);
		if (components == null) {
			throw new MetaDataException(
					String.format("<dataEnter /> with binding [%s] is not supported or is not on the view", identifier));
		}

		// A text area or a rich text editor takes the value's new lines (typed as Enter). Other widgets get spaces
		// instead: a new line typed into a single line input would press the form's default button.
		String multiLineValue = dataEnter.getValue();
		if (multiLineValue != null) {
			multiLineValue = substituteTokens(multiLineValue).replace("\r\n", "\n");
		}
		String singleLineValue = (multiLineValue == null) ? null : multiLineValue.replace('\n', ' ');

		boolean success = false;
		for (UIComponent component : components) {
			String clientId = PrimeFacesAutomationContext.clientId(component);
			String value = multiLine(component) ? multiLineValue : singleLineValue;
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
			} else if (component instanceof DatePicker) {
				success = selenide.typeOver(clientId + "_input", value);
			} else if (component instanceof Spinner) {
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
														singleLineValue));
		}
	}

	/**
	 * Indicates if a widget holds new lines: a text area or a rich text (or HTML) editor.
	 */
	private static boolean multiLine(@Nonnull UIComponent component) {
		return (component instanceof InputTextarea) || (component instanceof TextEditor);
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

		String expectedValue = test.getValue();
		// A text area or rich text editor shows new lines; other widgets were given spaces for them (see executeDataEnter)
		String multiLineExpected = (expectedValue == null) ? "" : substituteTokens(expectedValue).replace("\r\n", "\n").trim();
		String singleLineExpected = multiLineExpected.replace('\n', ' ');

		boolean found = false;
		for (UIComponent component : components) {
			String clientId = PrimeFacesAutomationContext.clientId(component);
			if (component instanceof PickList) {
				List<String> members = selenide.pickListMembers(clientId);
				if (members == null) {
					continue;
				}
				found = true;
				List<String> expectedMembers = membershipItems(singleLineExpected);
				selenide.trace(String.format("test %s (%s) members are %s", identifier, clientId, expectedMembers));
				check(new java.util.HashSet<>(members).equals(new java.util.HashSet<>(expectedMembers)),
						() -> String.format("expected [%s] members to be %s but they are %s", identifier, expectedMembers, members));
				continue;
			}
			boolean ignoreCase = (component instanceof ColorPicker);
			String expected = multiLine(component) ? multiLineExpected : singleLineExpected;
			List<String> actuals = actualValues(component, clientId);
			if (actuals == null) { // not on the page
				continue;
			}
			found = true;
			selenide.trace(String.format("test %s (%s) is %s", identifier, clientId, expected));

			boolean matched = false;
			for (String actual : actuals) {
				String trimmed = (actual == null) ? "" : actual.replace("\r\n", "\n").trim();
				if (ignoreCase ? trimmed.equalsIgnoreCase(expected) : trimmed.equals(expected)) {
					matched = true;
					break;
				}
			}
			check(matched, () -> String.format("expected [%s] to be '%s' but it was %s", identifier, expected, describeActuals(actuals)));
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
		popIfClosed("ok", redirectButton(ok, "ok", ok.getConfirm(), ok.getTestSuccess()));
	}

	@SuppressWarnings("null") // a null here fails the test
	@Override
	public void executeSave(Save save) {
		if (button(save, "save", true, save.getConfirm(), save.getTestSuccess()) || (! selenide.verifySuccess())) {
			// declined or refused: nothing was saved, so the view is still the one it was
			return;
		}

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
		popIfClosed("cancel", button(cancel, "cancel", false, null, cancel.getTestSuccess()));
	}

	@Override
	public void executeDelete(Delete delete) {
		popIfClosed("delete", redirectButton(delete, "delete", delete.getConfirm(), delete.getTestSuccess()));
	}

	@Override
	public void executeZoomOut(ZoomOut zoomOut) {
		popIfClosed("zoom out", button(zoomOut, "zoom out", false, zoomOut.getConfirm(), zoomOut.getTestSuccess()));
	}

	@Override
	public void executeRemove(Remove remove) {
		popIfClosed("remove", button(remove, "remove", false, remove.getConfirm(), remove.getTestSuccess()));
	}

	/**
	 * Pops the context after a gesture that closes the view (ok, cancel, delete, zoom out, remove), unless it was
	 * declined or refused (error messages, e.g. expected with {@code testSuccess="false"}): then the view stays.
	 *
	 * @param tagName	the gesture, for the trace
	 * @param declined	true if the gesture was declined
	 */
	private void popIfClosed(@Nonnull String tagName, boolean declined) {
		if (declined) {
			return;
		}
		if (! selenide.verifySuccess()) {
			selenide.trace(String.format("[%s] was refused; staying in this view", tagName));
			return;
		}
		pop();
	}

	@Override
	public void executeAction(Action action) {
		button(action, action.getActionName(), true, action.getConfirm(), action.getTestSuccess());
	}

	@Override
	public void executeLookupDescriptionAutoComplete(LookupDescriptionAutoComplete complete) {
		String search = complete.getSearch();
		String pick = complete.getPick();
		@SuppressWarnings("null") // SAIL XML turns a blank value into null
		boolean searchPresent = (search != null);
		if ((pick != null) && searchPresent) {
			autoCompleteLookupDescriptionAndPick(complete, substituteTokens(search), substituteTokens(pick));
			return;
		}
		lookupDescription(complete, complete.getBinding(), null, (search == null) ? null : substituteTokens(search));
	}

	@Override
	public void executeLookupDescriptionPick(LookupDescriptionPick pick) {
		String rowContaining = pick.getRowContaining();
		if (rowContaining != null) {
			pickLookupDescriptionRowContaining(pick, substituteTokens(rowContaining));
			return;
		}
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
		String text = substituteTokens(Objects.requireNonNull(step.getRowContaining(), "rowContaining"));
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
		return (row != null) ? "row " + row : "the row containing '" + substituteTokens(String.valueOf(step.getRowContaining())) + "'";
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
			String gridClientId = PrimeFacesAutomationContext.clientId(listGridComponent);
			if (selenide.isVueListGrid(gridClientId)) {
				// A Vue list grid (skyve-vue's ListGrid): its controls are in the browser, not in the component tree
				success = vueListGridGesture(step, rowStep, listGridIdentifier, gridClientId);
				continue;
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
	
	/**
	 * Does a list grid gesture on a Vue list grid: New with its New button, zoom with a row's View Detail button (or a
	 * row click, when the grid zooms on a row click), select with a row click.
	 *
	 * @return true if the gesture was done
	 */
	private boolean vueListGridGesture(@Nonnull Step step, @Nullable GridRowStep rowStep, @Nonnull String listGridIdentifier, @Nonnull String gridClientId) {
		if (rowStep == null) {
			selenide.trace(String.format("New row on list grid [%s] (%s)", listGridIdentifier, gridClientId));
			return selenide.vueListGridNew(gridClientId);
		}
		Integer row = resolveRow(rowStep, gridClientId);
		if (row == null) { // not on the page
			return false;
		}
		if (step instanceof ListGridSelect) {
			selenide.trace(String.format("Select on row %d on list grid [%s] (%s)", row, listGridIdentifier, gridClientId));
			return selenide.vueListGridSelect(gridClientId, row.intValue());
		}
		selenide.trace(String.format("Zoom on row %d on list grid [%s] (%s)", row, listGridIdentifier, gridClientId));
		return selenide.vueListGridZoom(gridClientId, row.intValue());
	}

	@SuppressWarnings("null") // a null here fails the test; uses a nullable result where the API expects non-null; checks values the annotations say are non-null
	private PushEditContext listGridContext(String queryName, String documentName, String modelName, Step step) {
		PushEditContext result = new PushEditContext();

		PrimeFacesAutomationContext context = peek();

		if (ViewType.list.equals(context.getViewType())) {
			result.setModuleName(context.getModuleName());
			result.setDocumentName(context.getDocumentName());
		} else if ((queryName == null) && (modelName != null)) {
			// A model of its document (the step's, or else the view's) lists its driving document
			Customer c = CORE.getUser().getCustomer();
			Module m = c.getModule(context.getModuleName());
			Document owner = m.getDocument(c, (documentName != null) ? documentName : context.getDocumentName());
			ListModel<Bean> lm = owner.getListModel(c, modelName, true);
			Document d = (lm == null) ? null : lm.getDrivingDocument();
			if (d == null) {
				throw new MetaDataException(String.format("<%s /> with identifier [%s]: model %s of document %s has no driving document",
															step.getClass().getSimpleName(),
															step.getIdentifier(context),
															modelName,
															owner.getName()));
			}
			result.setModuleName(d.getOwningModuleName());
			result.setDocumentName(d.getName());
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
					// a document of this module has no referenced module
					String referencedModuleName = r.getReferencedModuleName();
					result.setModuleName((referencedModuleName == null) ? m.getName() : referencedModuleName);
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
	 * A list grid of the view that is on the page.
	 *
	 * @param component	the grid's component, built in this JVM
	 * @param clientId	the grid's client id: a Vue list grid's container, or a PrimeFaces data table
	 * @param vue	true for a Vue list grid, false for a PrimeFaces data table
	 */
	private record ListGridOnPage(@Nonnull UIComponent component, @Nonnull String clientId, boolean vue) {
	}

	/**
	 * A column of a Vue list grid.
	 *
	 * @param field	the column's field in the grid (its sanitised binding)
	 * @param type	the grid's type for the column, such as text, numeric or enum
	 * @param title	the column's title, as its header shows it
	 * @param options	an enum column's [code, label] pairs; empty for other columns
	 */
	private record VueColumn(@Nonnull String field, @Nonnull String type, @Nonnull String title, @Nonnull List<List<String>> options) {
	}

	/**
	 * Filters a column of a Vue list grid in its filter menu, or of a PrimeFaces data table in its filter row.
	 * <p>
	 * A Vue list grid offers, by column type: text contains, startsWith, equals and notEquals (its menu's Contains,
	 * Starts With, Equals and Not Equals, which ignore case); numeric equals, notEquals, lessThan and greaterThan;
	 * enum (and constant domain) and boolean equals and notEquals; date, date time and timestamp equals, notEquals,
	 * lessThan and greaterThan (Before and After); time equals.
	 * <p>
	 * A PrimeFaces data table has one comparison per column: equals for a combo (constant domain) or check box (boolean)
	 * filter, and for a typed filter the one the server chooses from the column's type (see {@code SkyveLazyDataModel}):
	 * contains for text (and for a variant domain's description and an association's business key), equals otherwise.
	 * <p>
	 * Values: an option is named by its shown label, or else by its code; a boolean is true or false; a date, date time,
	 * time or timestamp is written as the column's attribute shows it (its converter's format, as {@code <dataEnter />}
	 * takes it), and is typed into a Vue calendar in the calendar's own format.
	 * Other columns and comparisons fail the step with the reason.
	 *
	 * @param filter	the step
	 * @param column	the column's binding
	 * @param value	the value, or null to clear the column's filter
	 * @param operator	the comparison; ignored when the value is null
	 */
	@Override
	protected void filterListGrid(ListGridFilter filter, String column, @Nullable String value, FilterOperator operator) {
		ListGridOnPage grid = listGridOnPage(filter, "listGridFilter");
		if (grid.vue()) {
			VueColumn vueColumn = vueColumn(grid, column, "listGridFilter", true);
			String type = vueColumn.type();
			VueFilterInput input = vueFilterInput(type);
			if (input == null) {
				throw new UnsupportedOperationException(String.format("<listGridFilter /> column [%s] of the Vue list grid %s is a %s column, which PrimeFaces SAIL cannot filter",
																		column, grid.clientId(), type));
			}
			String matchMode = null;
			String entered = null;
			if (value != null) {
				matchMode = vueMatchMode(type, operator);
				if (matchMode == null) {
					throw new UnsupportedOperationException(String.format("<listGridFilter /> column [%s] of the Vue list grid %s is a %s column, which cannot filter with %s: %s",
																			column,
																			grid.clientId(),
																			type,
																			operator,
																			vueOperators(type)));
				}
				entered = switch (input) {
					case text -> value;
					case option -> vueColumn.options().get(optionIndex(column, value, vueColumn.options())).get(1);
					case check -> booleanValue(column, value);
					case calendar -> vueCalendarText(filter, column, type, value);
				};
			}
			selenide.vueListGridFilter(grid.clientId(), vueColumn.field(), matchMode, entered, input);
		}
		else {
			UIComponent facesColumn = facesColumn(grid, column, "listGridFilter");
			String toggleId = facesFilterToggleId(grid);
			UIComponent chooser = facesColumn.getFacet("filter");
			if ((chooser instanceof SelectOneMenu) || (chooser instanceof TriStateCheckbox)) {
				if ((value != null) && (operator != FilterOperator.equals)) {
					throw new UnsupportedOperationException(String.format("<listGridFilter /> column [%s] of the PrimeFaces data table %s is filtered with a %s, which compares with equals only, so it cannot filter with %s",
																			column,
																			grid.clientId(),
																			(chooser instanceof SelectOneMenu) ? "combo" : "check box",
																			operator));
				}
				String chooserId = PrimeFacesAutomationContext.clientId(chooser);
				if (chooser instanceof SelectOneMenu) {
					int index = 0; // the empty option clears the filter
					if (value != null) {
						List<List<String>> options = selenide.selectOneOptions(chooserId);
						if (options == null) {
							throw new DomainException(String.format("<listGridFilter /> column [%s] of the PrimeFaces data table %s has no combo filter on the page",
																		column, grid.clientId()));
						}
						index = optionIndex(column, value, options);
					}
					selenide.facesListGridChooseFilter(chooserId, toggleId, index);
				}
				else {
					selenide.facesListGridCheckFilter(chooserId, toggleId, (value == null) ? null : Boolean.valueOf(booleanValue(column, value)));
				}
				checkSuccess("expected no error messages after <listGridFilter />");
				return;
			}
			FilterOperator only = facesFilterOperator(filter, column);
			if (only == null) {
				throw new UnsupportedOperationException(String.format("<listGridFilter /> column [%s] of the PrimeFaces data table %s has no typed filter",
																		column,
																		grid.clientId()));
			}
			if ((value != null) && (operator != only)) {
				throw new UnsupportedOperationException(String.format("<listGridFilter /> column [%s] of the PrimeFaces data table %s filters with %s only, so it cannot filter with %s",
																		column,
																		grid.clientId(),
																		only,
																		operator));
			}
			selenide.facesListGridFilter(PrimeFacesAutomationContext.clientId(facesColumn), toggleId, value);
		}
		checkSuccess("expected no error messages after <listGridFilter />");
	}

	/**
	 * Clears every column filter of a Vue list grid (Clear in each filtered column's menu), or of a PrimeFaces data table
	 * (deleting the text of each typed filter, choosing the empty option of each combo filter and unsetting each check
	 * box filter).
	 *
	 * @param clear	the step
	 */
	@Override
	protected void clearListGridFilters(ListGridClearFilter clear) {
		ListGridOnPage grid = listGridOnPage(clear, "listGridClearFilter");
		int cleared = grid.vue() ?
						selenide.vueListGridClearFilters(grid.clientId()) :
						selenide.facesListGridClearFilters(grid.clientId(), facesFilterToggleId(grid));
		selenide.trace(String.format("    cleared %d filter(s)", Integer.valueOf(cleared)));
		checkSuccess("expected no error messages after <listGridClearFilter />");
	}

	/**
	 * Sorts a Vue list grid or a PrimeFaces data table by a column alone, by clicking the column's title.
	 *
	 * @param sort	the step
	 * @param column	the column's binding
	 * @param descending	true for descending
	 */
	@Override
	protected void sortListGrid(ListGridSort sort, String column, boolean descending) {
		ListGridOnPage grid = listGridOnPage(sort, "listGridSort");
		if (grid.vue()) {
			selenide.vueListGridSort(grid.clientId(), vueColumn(grid, column, "listGridSort", false).field(), descending);
		}
		else {
			UIComponent facesColumn = facesColumn(grid, column, "listGridSort");
			selenide.facesListGridSort(grid.clientId(), PrimeFacesAutomationContext.clientId(facesColumn), descending);
		}
		checkSuccess("expected no error messages after <listGridSort />");
	}

	/**
	 * Finds the list grid that a list grid step names, on the page.
	 *
	 * @throws MetaDataException	if the view has no such list grid
	 * @throws DomainException	if the list grid is not on the page
	 */
	private @Nonnull ListGridOnPage listGridOnPage(@Nonnull Step step, @Nonnull String element) {
		PrimeFacesAutomationContext context = peek();
		@SuppressWarnings("null") // a null here fails the test
		String identifier = step.getIdentifier(context);
		@SuppressWarnings("null") // a null here fails the test
		List<UIComponent> components = context.getFacesComponents(identifier);
		if (components == null) {
			throw new MetaDataException(String.format("<%s /> list grid [%s] is not on the view", element, identifier));
		}
		for (UIComponent component : components) {
			String clientId = PrimeFacesAutomationContext.clientId(component);
			if (selenide.isVueListGrid(clientId)) {
				return new ListGridOnPage(component, clientId, true);
			}
			org.openqa.selenium.WebElement table = selenide.elementById(clientId);
			if ((table != null) && table.isDisplayed()) {
				return new ListGridOnPage(component, clientId, false);
			}
		}
		throw new DomainException(String.format("<%s /> list grid [%s] is not on the page", element, identifier));
	}

	/**
	 * Finds a column of a Vue list grid by its binding, and checks that it is shown and can be filtered or sorted.
	 *
	 * @throws MetaDataException	if the grid has no such column, or it cannot be filtered or sorted
	 * @throws DomainException	if the column is not shown
	 */
	private @Nonnull VueColumn vueColumn(@Nonnull ListGridOnPage grid, @Nonnull String column, @Nonnull String element, boolean filter) {
		VueColumn result = vueColumn(grid, column, element);
		List<Object> info = selenide.vueListGridColumn(grid.clientId(), result.field());
		if ((info == null) || (! Boolean.TRUE.equals(info.get(filter ? 3 : 4)))) {
			throw new MetaDataException(String.format("<%s /> column [%s] of the Vue list grid %s cannot be %s",
														element, column, grid.clientId(), filter ? "filtered" : "sorted"));
		}
		return result;
	}

	/**
	 * Finds a column of a Vue list grid by its binding, and checks that it is shown.
	 *
	 * @throws MetaDataException	if the grid has no such column
	 * @throws DomainException	if the column is not shown
	 */
	private @Nonnull VueColumn vueColumn(@Nonnull ListGridOnPage grid, @Nonnull String column, @Nonnull String element) {
		String field = column;
		List<Object> info = selenide.vueListGridColumn(grid.clientId(), field);
		String sanitised = BindUtil.sanitiseBinding(column);
		if ((info == null) && (sanitised != null) && (! sanitised.equals(column))) {
			field = sanitised;
			info = selenide.vueListGridColumn(grid.clientId(), field);
		}
		if (info == null) {
			throw new MetaDataException(String.format("<%s /> column [%s] is not a column of the Vue list grid %s", element, column, grid.clientId()));
		}
		if (! Boolean.TRUE.equals(info.get(2))) {
			throw new DomainException(String.format("<%s /> column [%s] (%s) is not shown in the Vue list grid %s: a user would show it with the grid's column chooser",
														element, column, info.get(1), grid.clientId()));
		}
		@SuppressWarnings("unchecked")
		List<List<String>> options = (List<List<String>>) info.get(5);
		return new VueColumn(field, String.valueOf(info.get(0)), String.valueOf(info.get(1)), options);
	}

	/**
	 * Returns the label of the match mode that a Vue list grid's column filter menu offers for an operator
	 * (see skyve-vue's MatchModes.js), or null if a column of the type cannot compare that way, or is not a text or
	 * numeric column (the only ones with a typed filter value that PF SAIL enters).
	 */
	private static @Nullable String vueMatchMode(@Nonnull String type, @Nonnull FilterOperator operator) {
		if ("text".equals(type)) {
			return switch (operator) {
				case contains -> "Contains";
				case startsWith -> "Starts With";
				case equals -> "Equals";
				case notEquals -> "Not Equals";
				default -> null;
			};
		}
		if ("numeric".equals(type)) {
			return switch (operator) {
				case equals -> "Equals";
				case notEquals -> "Not Equals";
				case lessThan -> "Less Than";
				case greaterThan -> "Greater Than";
				default -> null;
			};
		}
		if ("enum".equals(type) || "boolean".equals(type)) {
			return switch (operator) {
				case equals -> "Equals";
				case notEquals -> "Not Equals";
				default -> null;
			};
		}
		if ("date".equals(type) || "dateTime".equals(type) || "timestamp".equals(type)) {
			return switch (operator) {
				case equals -> "Equals";
				case notEquals -> "Not Equals";
				case lessThan -> "Before";
				case greaterThan -> "After";
				default -> null;
			};
		}
		if ("time".equals(type)) {
			return (operator == FilterOperator.equals) ? "Equals" : null;
		}
		return null;
	}

	/**
	 * Returns how a Vue list grid's column filter of the type takes its value, or null if PF SAIL cannot enter one.
	 */
	private static @Nullable VueFilterInput vueFilterInput(@Nonnull String type) {
		return switch (type) {
			case "text", "numeric" -> VueFilterInput.text;
			case "enum" -> VueFilterInput.option;
			case "boolean" -> VueFilterInput.check;
			case "date", "dateTime", "timestamp", "time" -> VueFilterInput.calendar;
			default -> null;
		};
	}

	/**
	 * Returns the text to type into a Vue list grid's filter calendar for a value written as the column's attribute
	 * shows it (its converter's format): the calendar's format is dd/mm/yyyy, then HH:mm for a date time and HH:mm:ss
	 * for a timestamp, and HH:mm for a time.
	 *
	 * @throws MetaDataException	if the value is not in the attribute's format
	 */
	private @Nonnull String vueCalendarText(@Nonnull ListGridFilter filter, @Nonnull String column, @Nonnull String type, @Nonnull String value) {
		Customer c = CORE.getUser().getCustomer();
		Attribute attribute = listGridAttribute(filter, column);
		Converter<?> converter = (attribute instanceof ConvertibleField field) ? field.getConverterForCustomer(c) : null;
		Object date;
		try {
			date = (attribute == null) ? null : BindUtil.fromString(c, converter, attribute.getImplementingType(), value);
		}
		catch (@SuppressWarnings("unused") Exception e) {
			date = null;
		}
		if (! (date instanceof java.util.Date d)) {
			throw new MetaDataException(String.format("<listGridFilter /> value '%s' for column [%s] is not a %s in the format its attribute shows%s",
														value, column, type,
														(converter == null) ? "" : " (" + converter.getClass().getSimpleName() + ")"));
		}
		String pattern = switch (type) {
			case "dateTime" -> "dd/MM/yyyy HH:mm";
			case "timestamp" -> "dd/MM/yyyy HH:mm:ss";
			case "time" -> "HH:mm";
			default -> "dd/MM/yyyy";
		};
		return new java.text.SimpleDateFormat(pattern).format(d);
	}

	/**
	 * Returns the index of the option whose shown label equals the value, or else whose code does.
	 *
	 * @param column	the column, for the messages
	 * @param value	the label or code
	 * @param options	the options' [code, label] pairs
	 * @throws DomainException	if no option, or more than one, has the value as its label, or else as its code
	 */
	private static int optionIndex(@Nonnull String column, @Nonnull String value, @Nonnull List<List<String>> options) {
		for (int part = 1; part >= 0; part--) {
			int index = -1;
			for (int i = 0, l = options.size(); i < l; i++) {
				if (value.equals(options.get(i).get(part))) {
					if (index >= 0) {
						throw new DomainException(String.format("<listGridFilter /> column [%s] has more than one option with the %s '%s': its options are %s",
																	column, (part == 1) ? "label" : "code", value, describeOptions(options)));
					}
					index = i;
				}
			}
			if (index >= 0) {
				return index;
			}
		}
		throw new DomainException(String.format("<listGridFilter /> column [%s] has no option with the label or code '%s': its options are %s",
													column, value, describeOptions(options)));
	}

	/**
	 * Describes options as {@code code=label} pairs, leaving out an empty option, for a failure.
	 */
	private static @Nonnull String describeOptions(@Nonnull List<List<String>> options) {
		StringBuilder result = new StringBuilder(64);
		for (List<String> option : options) {
			if (! option.get(0).isEmpty()) {
				result.append(result.isEmpty() ? "" : ", ").append(option.get(0)).append('=').append(option.get(1));
			}
		}
		return result.toString();
	}

	/**
	 * Checks a boolean filter value.
	 *
	 * @return true or false
	 * @throws MetaDataException	if the value is not true or false
	 */
	private static @Nonnull String booleanValue(@Nonnull String column, @Nonnull String value) {
		if ("true".equals(value) || "false".equals(value)) {
			return value;
		}
		throw new MetaDataException(String.format("<listGridFilter /> column [%s] is a boolean column, which filters with true or false, not '%s'", column, value));
	}

	/**
	 * Describes the operators a Vue list grid column of the type can filter with, for a failure.
	 */
	private static @Nonnull String vueOperators(@Nonnull String type) {
		if ("text".equals(type)) {
			return "a text column filters with contains, startsWith, equals or notEquals";
		}
		if ("numeric".equals(type)) {
			return "a numeric column filters with equals, notEquals, lessThan or greaterThan";
		}
		if ("enum".equals(type) || "boolean".equals(type)) {
			return "an enum or boolean column filters with equals or notEquals";
		}
		if ("time".equals(type)) {
			return "a time column filters with equals";
		}
		return "a date, date time or timestamp column filters with equals, notEquals, lessThan or greaterThan";
	}

	/**
	 * Finds the column of a PrimeFaces data table with the binding as its field.
	 *
	 * @throws MetaDataException	if the table has no such column
	 */
	private static @Nonnull UIComponent facesColumn(@Nonnull ListGridOnPage grid, @Nonnull String column, @Nonnull String element) {
		DataTable table = findChild(grid.component(), DataTable.class);
		if (table != null) {
			for (UIComponent child : table.getChildren()) {
				if ((child instanceof org.primefaces.component.column.Column facesColumn) && column.equals(facesColumn.getField())) {
					return facesColumn;
				}
			}
		}
		throw new MetaDataException(String.format("<%s /> column [%s] is not a column of the PrimeFaces data table %s", element, column, grid.clientId()));
	}

	/**
	 * Returns the client id of a PrimeFaces data table's filter toggle (in its action column's header), or null if it
	 * has none.
	 */
	private static @Nullable String facesFilterToggleId(@Nonnull ListGridOnPage grid) {
		DataTable table = findChild(grid.component(), DataTable.class);
		int count = (table == null) ? 0 : table.getChildCount();
		if (count == 0) {
			return null;
		}
		UIComponent header = table.getChildren().get(count - 1).getFacet("header");
		if (header != null) {
			for (UIComponent child : header.getChildren()) {
				if ((child instanceof org.primefaces.component.button.Button button) &&
						(button.getOnclick() != null) && button.getOnclick().contains("toggleFilters")) {
					return PrimeFacesAutomationContext.clientId(button);
				}
			}
		}
		return null;
	}

	/**
	 * Returns the one comparison that a PrimeFaces data table's typed column filter makes, as {@code SkyveLazyDataModel}
	 * applies it: contains for text (without a constant domain), a variant domain's description and an association's
	 * business key (and the implicit bizKey); equals otherwise.
	 *
	 * @return the comparison, or null if the column is filtered by choosing a value (a constant domain's combo or a
	 * 			boolean's check box)
	 */
	private @Nullable FilterOperator facesFilterOperator(@Nonnull ListGridFilter filter, @Nonnull String column) {
		Attribute attribute = listGridAttribute(filter, column);
		if (attribute == null) {
			return column.endsWith(Bean.BIZ_KEY) ? FilterOperator.contains : FilterOperator.equals;
		}
		DomainType domain = attribute.getDomainType();
		AttributeType type = attribute.getAttributeType();
		if ((domain == DomainType.constant) || (type == AttributeType.bool)) {
			return null;
		}
		if ((domain == DomainType.variant) || (type == AttributeType.association)) {
			return FilterOperator.contains;
		}
		boolean textual = (type == AttributeType.text) || (type == AttributeType.memo) || (type == AttributeType.markup) || (type == AttributeType.colour);
		return (textual && (domain == null)) ? FilterOperator.contains : FilterOperator.equals;
	}

	/**
	 * Returns the attribute that a list grid filter step's column binds to, in the list's driving document.
	 *
	 * @return the attribute, or null for an implicit attribute (such as bizKey)
	 */
	@SuppressWarnings("null") // a null here fails the test
	private @Nullable Attribute listGridAttribute(@Nonnull ListGridFilter filter, @Nonnull String column) {
		PrimeFacesAutomationContext context = peek();
		Customer c = CORE.getUser().getCustomer();
		String moduleName = filter.getModuleName();
		Module m = c.getModule((moduleName != null) ? moduleName : context.getModuleName());
		String documentName = filter.getDocumentName();
		if (documentName == null) {
			documentName = context.getDocumentName();
		}
		String queryName = filter.getQueryName();
		String modelName = filter.getModelName();
		Document driving;
		if (modelName != null) {
			driving = m.getDocument(c, documentName).getListModel(c, modelName, true).getDrivingDocument();
		}
		else {
			MetaDataQueryDefinition query = (queryName == null) ? null : m.getMetaDataQuery(queryName);
			if (query == null) {
				query = m.getDocumentDefaultQuery(c, (queryName == null) ? documentName : queryName);
			}
			driving = query.getDocumentModule(c).getDocument(c, query.getDocumentName());
		}
		TargetMetaData target = BindUtil.getMetaDataForBinding(c, c.getModule(driving.getOwningModuleName()), driving, column);
		return target.getAttribute();
	}

	/**
	 * Returns the text that the first displayed widget for the binding shows, as {@code <testValue />} reads it: the
	 * text of an input, the label of a combo's or radio's option, true or false (or empty) for a check box, the plain
	 * text of a rich text or HTML editor, the {@code |}-separated members of a list membership.
	 *
	 * @param capture	the step
	 * @return the text
	 * @throws DomainException	if no widget for the binding is shown
	 */
	@SuppressWarnings("null") // a null here fails the test
	@Override
	protected @Nullable String readWidgetValue(CaptureValue capture) {
		PrimeFacesAutomationContext context = peek();
		String identifier = capture.getIdentifier(context);
		List<UIComponent> components = context.getFacesComponents(identifier);
		if (components == null) {
			throw new MetaDataException(String.format("<captureValue /> with binding [%s] is not supported or is not on the view", identifier));
		}
		for (UIComponent component : components) {
			String clientId = PrimeFacesAutomationContext.clientId(component);
			if (! Boolean.TRUE.equals(selenide.widgetState(clientId).get(0))) { // not shown
				continue;
			}
			if (component instanceof PickList) {
				List<String> members = selenide.pickListMembers(clientId);
				if (members != null) {
					return String.join("|", members);
				}
				continue;
			}
			List<String> actuals = actualValues(component, clientId);
			if (actuals == null) { // not on the page
				continue;
			}
			// a combo or radio shows its option's label (after its index and value)
			return ((component instanceof SelectOneMenu) || (component instanceof SelectOneRadio)) ? actuals.get(2) : actuals.get(0);
		}
		throw new DomainException(String.format("<captureValue /> with binding [%s] has no widget shown on the page", identifier));
	}

	/**
	 * Chooses the option of a combo or radio whose code (its value in the page) or label (its text in the page) equals
	 * the value, as a user does: a combo's drop down item, or a radio's label, is clicked.
	 *
	 * @param dataEnter	the step
	 * @param value	the code or label
	 * @param match	{@link Match#code} or {@link Match#label}
	 * @throws MetaDataException	if the widget is not a combo or radio
	 * @throws DomainException	if no option, or more than one, has the code or label, or no widget is shown and enabled
	 */
	@SuppressWarnings("null") // a null here fails the test
	@Override
	protected void enterOption(DataEnter dataEnter, String value, Match match) {
		PrimeFacesAutomationContext context = peek();
		String identifier = dataEnter.getIdentifier(context);
		List<UIComponent> components = context.getFacesComponents(identifier);
		if (components == null) {
			throw new MetaDataException(String.format("<dataEnter /> with binding [%s] is not supported or is not on the view", identifier));
		}
		for (UIComponent component : components) {
			boolean combo = (component instanceof SelectOneMenu);
			if ((! combo) && (! (component instanceof SelectOneRadio))) {
				throw new MetaDataException(String.format("<dataEnter match=\"%s\" /> with binding [%s] is a %s: match=\"%s\" chooses an option of a combo or radio",
															match, identifier, component.getClass().getSimpleName(), match));
			}
			String clientId = PrimeFacesAutomationContext.clientId(component);
			List<List<String>> options = combo ? selenide.selectOneOptions(clientId) : selenide.radioOptions(clientId);
			if (options == null) { // not on the page
				continue;
			}
			int part = (match == Match.code) ? 0 : 1;
			int index = -1;
			for (int i = 0, l = options.size(); i < l; i++) {
				if (value.equals(options.get(i).get(part))) {
					if (index >= 0) {
						throw new DomainException(String.format("<dataEnter /> with binding [%s] has more than one option with the %s '%s': its options are %s",
																	identifier, match, value, options));
					}
					index = i;
				}
			}
			if (index < 0) {
				throw new DomainException(String.format("<dataEnter /> with binding [%s] has no option with the %s '%s': its options (code and label) are %s",
															identifier, match, value, options));
			}
			selenide.trace(String.format("set %s (%s) to the option with the %s '%s' (option %d)", identifier, clientId, match, value, Integer.valueOf(index)));
			boolean success = combo ?
								selenide.selectOne(new PrimeFacesSelectOneCommand(clientId, index)) :
								selenide.radio(new PrimeFacesRadioCommand(clientId, index));
			if (success) {
				return;
			}
		}
		throw new DomainException(String.format("Data entry failed: binding [%s] has no displayed, enabled combo or radio to choose '%s' in",
													identifier, value));
	}

	@Override
	protected void pickLookupDescriptionRowContaining(LookupDescriptionPick pick, String rowContaining) {
		lookupDescriptionContaining(pick, pick.getBinding(), null, rowContaining);
	}

	@Override
	protected void autoCompleteLookupDescriptionAndPick(LookupDescriptionAutoComplete complete, String search, String pick) {
		lookupDescriptionContaining(complete, complete.getBinding(), search, pick);
	}

	/**
	 * Picks the first row of a lookup description's drop down that contains the text, opening the drop down with its
	 * button or with a search.
	 */
	private void lookupDescriptionContaining(@Nonnull Step step, @Nonnull String binding, @Nullable String search, @Nonnull String text) {
		PrimeFacesAutomationContext context = peek();
		@SuppressWarnings("null") // a null here fails the test
		List<UIComponent> lookupComponents = context.getFacesComponents(binding);
		if (lookupComponents == null) {
			throw new MetaDataException(String.format("<%s /> with binding [%s] is not on the view.", step.getClass().getSimpleName(), binding));
		}
		for (UIComponent lookupComponent : lookupComponents) {
			String clientId = PrimeFacesAutomationContext.clientId(lookupComponent);
			if (search == null) {
				selenide.trace(String.format("Pick the row containing '%s' on lookup description [%s] (%s)", text, binding, clientId));
			}
			else {
				selenide.trace(String.format("Auto complete with search '%s' and pick the row containing '%s' on lookup description [%s] (%s)",
												search, text, binding, clientId));
			}
			if (selenide.lookupDescriptionContaining(clientId, search, text)) {
				return;
			}
		}
		throw new DomainException(String.format("<%s /> with binding [%s] has no lookup on the page", step.getClass().getSimpleName(), binding));
	}

	/**
	 * Clicks the first displayed link on the page whose visible text contains the text, and waits for the page it loads
	 * (or the ajax request it sends).
	 *
	 * @param link	the step
	 * @param text	the text
	 * @throws DomainException	if no displayed link contains the text
	 */
	@Override
	protected void clickLinkContaining(NavigateLink link, String text) {
		boolean exact = Boolean.TRUE.equals(link.getExact());
		if (! selenide.followLinkContaining(text, exact)) {
			throw new DomainException(String.format("<navigateLink text=\"%s\"%s /> found no displayed link %s it: the page's links are %s",
														text, exact ? " exact=\"true\"" : "", exact ? "with exactly that text for" : "containing",
														selenide.linkTexts()));
		}
	}

	/**
	 * Reads the text of a cell of a data grid (by binding) or a list grid of the view. The column is found by its header:
	 * a PrimeFaces data table's column by the column component built for it (a data grid's column by its place in the
	 * grid's metadata, a list's by its field), a Vue list grid's by its title.
	 *
	 * @param capture	the step
	 * @return the cell's text
	 * @throws DomainException	if the grid is not on the page, or the row or cell cannot be read
	 */
	@SuppressWarnings("null") // a null here fails the test
	@Override
	protected @Nullable String readGridCell(CaptureCell capture) {
		PrimeFacesAutomationContext context = peek();
		String identifier = capture.getIdentifier(context);
		List<UIComponent> components = context.getFacesComponents(identifier);
		if (components == null) {
			throw new MetaDataException(String.format("<captureCell /> grid [%s] is not on the view", identifier));
		}
		String column = capture.getColumn();
		for (UIComponent component : components) {
			String gridId = PrimeFacesAutomationContext.clientId(component);
			if (selenide.countRows(gridId, null) < 0) { // not on the page
				continue;
			}
			Integer row = resolveRow(capture, gridId);
			if (row == null) {
				continue;
			}
			String headerId = null;
			String headerTitle = null;
			if (selenide.isVueListGrid(gridId)) {
				ListGridOnPage grid = new ListGridOnPage(component, gridId, true);
				VueColumn vue = vueColumn(grid, column, "captureCell");
				headerTitle = vue.title();
			}
			else {
				headerId = PrimeFacesAutomationContext.clientId(cellColumn(context, component, capture.getBinding(), column, gridId));
			}
			selenide.trace(String.format("read column [%s] of row %d of grid [%s] (%s)", column, row, identifier, gridId));
			List<String> text = selenide.cellText(gridId, row.intValue(), headerId, headerTitle);
			if (text.isEmpty() || (text.get(0) == null)) {
				throw new DomainException(String.format("<captureCell /> could not read column [%s] of row %d of grid [%s]: %s",
															column, row, identifier, text.isEmpty() ? "" : text.get(1)));
			}
			return text.get(0);
		}
		throw new DomainException(String.format("<captureCell /> grid [%s] is not on the page", identifier));
	}

	/**
	 * Finds the column component of a PrimeFaces data table for a column binding: a data grid's by the column's place
	 * among the grid's columns in the view's metadata, a list grid's by its field.
	 *
	 * @throws MetaDataException	if the grid has no such column
	 */
	private static @Nonnull UIComponent cellColumn(@Nonnull PrimeFacesAutomationContext context,
													@Nonnull UIComponent component,
													@Nullable String binding,
													@Nonnull String column,
													@Nonnull String gridId) {
		if (binding == null) {
			return facesColumn(new ListGridOnPage(component, gridId, false), column, "captureCell");
		}
		DataTable table = (component instanceof DataTable dataTable) ? dataTable : findChild(component, DataTable.class);
		List<Object> widgets = context.getSkyveWidgets(binding);
		if ((table != null) && (widgets != null) && (! widgets.isEmpty()) && (widgets.get(0) instanceof DataGrid grid)) {
			List<org.skyve.impl.metadata.view.widget.bound.tabular.DataGridColumn> columns = grid.getColumns();
			List<String> bindings = new ArrayList<>(columns.size());
			for (int i = 0, l = columns.size(); i < l; i++) {
				if (columns.get(i) instanceof org.skyve.impl.metadata.view.widget.bound.tabular.DataGridBoundColumn bound) {
					String candidate = bound.getBinding();
					if (column.equals(candidate)) {
						if (i < table.getChildCount()) {
							return table.getChildren().get(i);
						}
						break;
					}
					bindings.add(candidate);
				}
			}
			throw new MetaDataException(String.format("<captureCell /> column [%s] is not a column of data grid [%s]: its columns are %s",
														column, binding, bindings));
		}
		throw new MetaDataException(String.format("<captureCell /> [%s] is not a data grid", binding));
	}

	/**
	 * Reads the title the page shows for the view.
	 *
	 * @param capture	the step
	 * @return the title
	 */
	@Override
	protected @Nullable String readViewTitle(CaptureTitle capture) {
		return selenide.viewTitle();
	}

	/**
	 * Clicks a link in a data grid's row (see {@link PrimeFacesSelenide#clickRowLink}).
	 *
	 * @param link	the step
	 * @param text	the link's text
	 * @param action	true if the link runs an action
	 * @return true if the action was declined
	 * @throws DomainException	if the grid is not on the page
	 */
	@SuppressWarnings("null") // a null here fails the test
	@Override
	protected boolean clickDataGridLink(DataGridLink link, String text, boolean action) {
		PrimeFacesAutomationContext context = peek();
		String binding = link.getBinding();
		List<UIComponent> components = context.getFacesComponents(binding);
		if (components == null) {
			throw new MetaDataException(String.format("<dataGridLink /> with binding [%s] is not on the view", binding));
		}
		for (UIComponent component : components) {
			String gridId = PrimeFacesAutomationContext.clientId(component);
			if (selenide.countRows(gridId, null) < 0) { // not on the page
				continue;
			}
			Integer row = resolveRow(link, gridId);
			if (row == null) {
				continue;
			}
			selenide.trace(String.format("    row %d of data grid [%s] (%s)", row, binding, gridId));
			return selenide.clickRowLink(gridId, row.intValue(), text, action, link.getConfirm());
		}
		throw new DomainException(String.format("<dataGridLink /> data grid [%s] is not on the page", binding));
	}

	/**
	 * Uploads a test classpath file: into a content widget (through its upload page), or through an upload action's
	 * upload page. For a content widget, the widget must then hold content. With {@code testSuccess="false"}, a refusal
	 * on the upload page is recorded for a following {@code <testFailure/>} (the upload page is closed, as a user does),
	 * and neither the content nor the messages are checked.
	 *
	 * @param upload	the step
	 */
	@SuppressWarnings("null") // a null here fails the test
	@Override
	public void executeUpload(Upload upload) {
		uploadRefused(null);
		if (Boolean.FALSE.equals(upload.getTestSuccess())) {
			uploadRefusable(upload);
			return;
		}
		PrimeFacesAutomationContext context = peek();
		String identifier = upload.getIdentifier(context);
		List<UIComponent> components = context.getFacesComponents(identifier);
		if (components == null) {
			throw new MetaDataException(String.format("<upload /> with %s [%s] is not on the view",
														(upload.getBinding() != null) ? "binding" : "action", identifier));
		}
		java.io.File file = testResourceFile(upload.getFile());

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
	 * Uploads a file that the application may refuse ({@code testSuccess="false"}): a refusal on the upload page is
	 * recorded, and nothing else is checked.
	 *
	 * @param upload	the step
	 */
	@SuppressWarnings("null") // a null here fails the test
	private void uploadRefusable(@Nonnull Upload upload) {
		PrimeFacesAutomationContext context = peek();
		String identifier = upload.getIdentifier(context);
		List<UIComponent> components = context.getFacesComponents(identifier);
		if (components == null) {
			throw new MetaDataException(String.format("<upload /> with %s [%s] is not on the view",
														(upload.getBinding() != null) ? "binding" : "action", identifier));
		}
		java.io.File file = testResourceFile(upload.getFile());
		for (UIComponent component : components) {
			String clientId = PrimeFacesAutomationContext.clientId(component);
			org.openqa.selenium.WebElement trigger = (upload.getBinding() != null) ?
														selenide.contentUploadTrigger(clientId) :
														selenide.uploadTrigger(clientId);
			if (trigger != null) {
				selenide.trace(String.format("upload %s to [%s] (%s), which may be refused", upload.getFile(), identifier, clientId));
				uploadRefused(selenide.uploadOrRefusal(trigger, file));
				return;
			}
		}
		throw new DomainException(String.format("<upload /> to [%s]: no displayed, enabled widget to upload with", identifier));
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
		String contains = substituteTokens(test.getContains());
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
	 * With a timeout, the grid is reloaded from the server and the rows recounted each second until the bounds hold or
	 * the time is up; this wait happens under the None strategy too (scripts use it to wait for jobs), with no check.
	 *
	 * @param test	the step
	 */
	@SuppressWarnings("null") // a null here fails the test
	@Override
	public void executeTestGridRows(TestGridRows test) {
		Integer timeout = test.getTimeoutMillis();
		// Under None there is nothing to check, but a timeout still waits: scripts use it to wait for jobs
		if (isNoneStrategy() && (timeout == null)) {
			return;
		}
		PrimeFacesAutomationContext context = peek();
		String identifier = test.getIdentifier(context);
		List<UIComponent> components = context.getFacesComponents(identifier);
		if (components == null) {
			throw new MetaDataException(String.format("<testGridRows /> grid [%s] is not on the view", identifier));
		}
		String text = test.getRowContaining();
		final String contains = (text == null) ? null : substituteTokens(text);

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
		Integer max = test.getMaxRows();
		selenide.trace(String.format("test grid [%s] (%s) has %d..%s rows%s", identifier, gridId, Integer.valueOf(test.getEffectiveMinRows()),
										(max == null) ? "" : max.toString(), (contains == null) ? "" : " containing '" + contains + "'"));
		int count = selenide.countRows(gridId, contains);
		if ((timeout != null) && (! rowCountHolds(test, count))) {
			// Author-requested (timeoutMillis): poll a server-side job's effect by reloading the grid each second
			final String grid = gridId;
			try {
				selenide.waitFor(java.time.Duration.ofMillis(timeout.longValue()), java.time.Duration.ofSeconds(1), d -> {
					selenide.refreshGrid(grid);
					return rowCountHolds(test, selenide.countRows(grid, contains)) ? Boolean.TRUE : null;
				});
			}
			catch (@SuppressWarnings("unused") org.openqa.selenium.TimeoutException e) {
				// reported below
			}
			count = selenide.countRows(gridId, contains);
		}
		checkRowCount(test, identifier, contains, count);
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
			String actionName = test.getActionName();
			if ((actionName != null) && Boolean.FALSE.equals(test.getVisible()) && viewHasAction(context, actionName)) {
				// An action of the view with no button (such as one that is not in the action panel and has no
				// button in the view) is not shown
				selenide.trace(String.format("test action [%s] is not shown: the view has it but renders no button for it", actionName));
				checkWidget(test, identifier, false, false);
				return;
			}
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
		checkWidget(test, identifier, shown, enabled);
	}

	/**
	 * Indicates if the context's view (in its UX/UI and view type) declares the action.
	 */
	@SuppressWarnings("null") // a null here fails the test
	private static boolean viewHasAction(@Nonnull PrimeFacesAutomationContext context, @Nonnull String actionName) {
		Customer c = CORE.getUser().getCustomer();
		String moduleName = context.getModuleName();
		String documentName = context.getDocumentName();
		ViewType viewType = context.getViewType();
		if ((moduleName == null) || (documentName == null) || (viewType == null)) {
			return false;
		}
		Document d = c.getModule(moduleName).getDocument(c, documentName);
		org.skyve.metadata.view.View view = d.getView(context.getUxui(), c, viewType.toString());
		return (view != null) && (view.getAction(actionName) != null);
	}

	/**
	 * Clicks through the menu (module, groups, item) and sets the context as the equivalent navigate step would.
	 *
	 * @param menu	the step
	 */
	@Override
	public void executeNavigateMenu(NavigateMenu menu) {
		SailMenuPath path = resolveMenuPath(menu.getModuleName(), menu.getItemName());
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
		SailMenuPath path = resolveMenuPath(test.getModuleName(), itemName);
		if ((itemName != null) && (path.item == null)) {
			throw new MetaDataException(String.format("<testMenu /> module %s has no menu item %s", test.getModuleName(), itemName));
		}
		checkMenu(test, path, selenide.menu(path.moduleTitle, path.groupLabels, path.itemLabel, false));
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
