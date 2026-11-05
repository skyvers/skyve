package util.sail;

import java.beans.Introspector;
import java.util.List;
import java.util.Objects;

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
import org.skyve.impl.sail.execution.sc.Locator.InputType;
import org.skyve.impl.sail.execution.sc.SmartClientAutomationContext;
import org.skyve.impl.sail.execution.sc.SmartClientGenerateEditContext;
import org.skyve.impl.sail.execution.sc.SmartClientGenerateListContext;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.Relation;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.Module.DocumentRef;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;
import org.skyve.metadata.sail.language.Step;
import org.skyve.metadata.sail.language.step.CaptureValue;
import org.skyve.metadata.sail.language.step.Execute;
import org.skyve.metadata.sail.language.step.TestGridRows;
import org.skyve.metadata.sail.language.step.TestMessage;
import org.skyve.metadata.sail.language.step.TestWidget;
import org.skyve.metadata.sail.language.step.interaction.Upload;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateMenu;
import org.skyve.metadata.sail.language.step.interaction.navigation.TestMenu;
import org.skyve.impl.metadata.module.menu.AbstractDocumentOrQueryOrModelMenuItem;
import org.skyve.metadata.module.menu.MenuItem;
import org.skyve.impl.metadata.module.menu.EditItem;
import org.skyve.metadata.sail.language.step.TestSuccess;
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
import org.skyve.metadata.sail.language.step.interaction.grids.DataGridNew;
import org.skyve.metadata.sail.language.step.interaction.grids.DataGridRemove;
import org.skyve.metadata.sail.language.step.interaction.grids.DataGridSelect;
import org.skyve.metadata.sail.language.step.interaction.grids.DataGridZoom;
import org.skyve.metadata.sail.language.step.interaction.grids.GridRowStep;
import org.skyve.metadata.sail.language.step.interaction.grids.ListGridClearFilter;
import org.skyve.metadata.sail.language.step.interaction.grids.ListGridFilter;
import org.skyve.metadata.sail.language.step.interaction.grids.ListGridFilter.FilterOperator;
import org.skyve.metadata.sail.language.step.interaction.grids.ListGridNew;
import org.skyve.metadata.sail.language.step.interaction.grids.ListGridSelect;
import org.skyve.metadata.sail.language.step.interaction.grids.ListGridSort;
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
import org.skyve.metadata.view.View;
import org.skyve.metadata.view.View.ViewType;
import org.skyve.metadata.view.model.list.ListModel;
import org.skyve.util.Binder.TargetMetaData;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
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
 * <p>
 * Locators come from {@link org.skyve.impl.sail.execution.sc.SmartClientSAILViewVisitor} and are resolved
 * in the browser against the view that is showing. After each step that opens or closes a view, the
 * executor waits for the browser to show the view the automation context expects, so that a step that
 * did not do what the script expects fails there and then.
 *
 * @author simeonsolomou
 */
public class SmartClientInterpretedWebDriverExecutor extends InterpretedWebDriverExecutor<SmartClientAutomationContext, SmartClientSelenide> {

	private static final List<ImplicitActionName> EXCLUDED_IMPLICIT_ACTIONS = List.of(ImplicitActionName.Upload);

	/**
	 * Creates an executor that drives the given browser.
	 *
	 * @param selenide	the SmartClient browser driver
	 */
	public SmartClientInterpretedWebDriverExecutor(@Nonnull SmartClientSelenide selenide) {
		this.selenide = selenide;
	}

	/**
	 * Switches the session to the automation's renderer with the user-visible control. This harness drives
	 * SmartClient, so the automation's UX/UI must be {@value org.skyve.metadata.router.UxUi#DESKTOP_NAME}.
	 */
	@Override
	public void executeSwitchMode(org.skyve.metadata.sail.language.step.interaction.session.SwitchMode switchMode) {
		if (! org.skyve.metadata.router.UxUi.DESKTOP_NAME.equals(getCurrentUxUi())) {
			throw new MetaDataException(String.format("<switchMode/> targets PrimeFaces for uxui %s, but this executor drives SmartClient " +
														"(uxui %s): run the script with the PrimeFaces executor",
														getCurrentUxUi(), org.skyve.metadata.router.UxUi.DESKTOP_NAME));
		}
		selenide.trace("Switch mode to SmartClient");
		selenide.switchToSmartClient();
	}

	/**
	 * Runs the script, then lets SmartClient settle.
	 */
	@Override
	public void executeExecute(Execute execute) {
		prepareForInteraction();
		super.executeExecute(execute);
		selenide.settle();
	}

	@Override
	public void executeNavigateList(NavigateList list) {
		super.executeNavigateList(list);

		selenide.waitForList();
	}

	@Override
	public void executeNavigateTree(NavigateTree tree) {
		super.executeNavigateTree(tree);

		selenide.waitForList();
	}

	@Override
	public void executeNavigateMap(NavigateMap map) {
		super.executeNavigateMap(map);

		selenide.waitForList();
	}

	@Override
	public void executeNavigateCalendar(NavigateCalendar calendar) {
		super.executeNavigateCalendar(calendar);

		selenide.waitForList();
	}

	@Override
	public void executeNavigateEdit(NavigateEdit edit) {
		super.executeNavigateEdit(edit);

		selenide.waitForView(edit.getModuleName(), edit.getDocumentName());
	}

	/**
	 * Pushes a list context. This only generates locators; the caller navigates and then waits for the list.
	 */
	@Override
	public void executePushListContext(PushListContext push) {
		SmartClientAutomationContext newContext = new SmartClientAutomationContext();
		newContext(push, newContext);
		push(newContext);

		newContext.generate(new SmartClientGenerateListContext(push));
	}

	/**
	 * Pushes an edit context. This only generates locators; the caller opens the view and then waits for it.
	 */
	@Override
	public void executePushEditContext(PushEditContext push) {
		SmartClientAutomationContext newContext = new SmartClientAutomationContext();
		newContext(push, newContext);
		push(newContext);

		newContext.generate(new SmartClientGenerateEditContext(push));
	}

	/**
	 * Selects a tab. A {@code ::}-separated path selects each tab along the path in turn.
	 */
	@Override
	public void executeTabSelect(TabSelect tabSelect) {
		prepareForInteraction();
		SmartClientAutomationContext context = context();

		String path = tabSelect.getTabPath();
		String[] titles = path.split("::");
		StringBuilder prefix = new StringBuilder(path.length());
		for (String title : titles) {
			if (prefix.length() > 0) {
				prefix.append("::");
			}
			prefix.append(title);
			String identifier = prefix + " Tab";
			List<Locator> locators = context.getLocators(identifier);
			if ((locators == null) && (titles.length > 1)) {
				locators = context.getLocators(title + " Tab");
			}
			if (locators == null) {
				throw new MetaDataException(String.format("<tabSelect /> with path [%s] is not valid or is not on the view (no tab [%s])",
															path, prefix));
			}

			boolean success = false;
			for (Locator locator : locators) {
				if (! success) {
					selenide.trace(String.format("click tab [%s] (%s)", prefix, locator));
					success = selenide.tab(new SmartClientTabCommand(locator.getLocator()));
				}
			}

			if (! success) {
				throw new DomainException("Tab [" + prefix + "] not selected");
			}
		}
	}

	@Override
	public void executeTestDataEnter(TestDataEnter testDataEnter) {
		prepareForInteraction();
		SmartClientAutomationContext context = context();

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
	 * Enters data into the widget(s) bound to the binding.
	 * <p>
	 * Checkboxes take {@code true} or {@code false}; combos and radios take a zero-based option index, or with
	 * {@code match} the option's code or label (see {@link #enterOption});
	 * sliders take a number; lookup descriptions take search text and pick the first match;
	 * everything else is typed. While a data grid row is being edited inline (after {@code dataGridEdit}
	 * or {@code dataGridNew} on an inline grid), the binding is first tried as a column of that grid.
	 */
	@Override
	public void executeDataEnter(DataEnter dataEnter) {
		Match match = dataEnter.getEffectiveMatch();
		String matchValue = dataEnter.getValue();
		@SuppressWarnings("null") // SAIL XML turns a blank value into null
		boolean matchValuePresent = (matchValue != null);
		if ((match != Match.index) && matchValuePresent) {
			enterOption(dataEnter, substituteTokens(matchValue), match);
			return;
		}
		prepareForDataEntry();
		SmartClientAutomationContext context = context();

		String identifier = dataEnter.getIdentifier(context);
		List<Locator> locators = inputLocators(context, identifier);
		if (locators == null) {
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
		boolean supported = false;
		for (Locator locator : locators) {
			InputType inputType = locator.getInputType();
			if ((! success) && (inputType != null)) {
				supported = true;
				String value = ((singleLineValue != null) && (! singleLineValue.equals(multiLineValue)) && multiLine(locator)) ?
									multiLineValue :
									singleLineValue;
				selenide.trace(String.format("set %s to %s", identifier, value));
				success = enter(locator.getLocator(), inputType, value);
			}
		}

		if (! supported) {
			throw new MetaDataException(String.format("<dataEnter /> with binding [%s] is on the view but its widget does not take data entry", identifier));
		}
		if (! success) {
			throw new DomainException(String.format("Data entry failed for binding [%s] (is the widget disabled or read only?)", identifier));
		}

		selenide.waitForFullPageResponse();
	}

	/**
	 * Indicates if a widget keeps new lines: a rich text editor, or a text area.
	 *
	 * @param locator	the widget's locator
	 * @return {@code true} if it does
	 */
	private boolean multiLine(@Nonnull Locator locator) {
		InputType inputType = locator.getInputType();
		return (inputType == InputType.RICH_TEXT) || ((inputType == InputType.TEXT) && selenide.multiLine(locator.getLocator()));
	}

	/**
	 * Enters a value into one widget.
	 *
	 * @param locator	the widget's locator
	 * @param inputType	how data is entered
	 * @param value		the value
	 * @return {@code true} if the value was entered
	 */
	private boolean enter(@Nonnull String locator, @Nonnull InputType inputType, @Nullable String value) {
		switch (inputType) {
			case CHECKBOX:
				return selenide.checkbox(new SmartClientCheckboxCommand(locator, Boolean.valueOf(value)));
			case COMBO:
				return selenide.selectOne(new SmartClientSelectOneCommand(locator, index(value)));
			case RADIO:
				return selenide.radio(new SmartClientRadioCommand(locator, index(value)));
			case LOOKUP_DESCRIPTION:
				return selenide.lookupDescriptionBySearch(new SmartClientLookupDescriptionBySearchCommand(locator, value));
			case RICH_TEXT:
				return selenide.richText(locator, value);
			case SLIDER:
				if (value == null) {
					throw new MetaDataException("<dataEnter /> on a slider needs a value");
				}
				return selenide.slider(locator, value);
			case MEMBERSHIP:
				return selenide.membership(locator, value);
			case HTML:
				return selenide.html(locator, value);
			case TEXT:
			default:
				return selenide.text(new SmartClientTextCommand(locator, value, false));
		}
	}

	/**
	 * Parses a combo or radio index.
	 *
	 * @param value	the value
	 * @return the index
	 */
	private static int index(@Nullable String value) {
		try {
			return Integer.parseInt(value);
		}
		catch (NumberFormatException e) {
			throw new MetaDataException("<dataEnter /> on a SmartClient combo or radio takes a zero-based option index, not [" + value + "]", e);
		}
	}

	/**
	 * Finds the input locators for a binding, preferring a column of a data grid row being edited inline.
	 *
	 * @param context		the context
	 * @param identifier	the binding
	 * @return the locators, or {@code null}
	 */
	private static @Nullable List<Locator> inputLocators(@Nonnull SmartClientAutomationContext context, @Nonnull String identifier) {
		String inline = context.getInlineGridBinding();
		if (inline != null) {
			List<Locator> result = context.getLocators(inline + '.' + identifier);
			if (result != null) {
				return result;
			}
		}
		return context.getLocators(identifier);
	}

	/**
	 * Attempts to click a button identified by the given {@link Step} and tag name within the current UI context.
	 *
	 * @param button the Step object representing the button to click
	 * @param tagName the XML tag name of the button action
	 * @param confirm how to answer a confirmation: null or true answers yes if one shows, false answers no (one must show)
	 * @param testSuccess optional flag to control triggering test success actions; if null or true, test success is executed
	 */
	private void button(@Nonnull Step button, @Nonnull String tagName, @Nullable Boolean confirm, @Nullable Boolean testSuccess) {
		prepareForInteraction();
		SmartClientAutomationContext context = context();

		User u = CORE.getUser();
		Customer c = u.getCustomer();
		@SuppressWarnings("null") // a null here fails the test; uses a nullable result where the API expects non-null
		Module m = c.getModule(context.getModuleName());
		@SuppressWarnings("null") // uses a nullable result where the API expects non-null
		Document d = m.getDocument(c, context.getDocumentName());
		@SuppressWarnings("null") // a null here fails the test
		View v = d.getView(context.getUxui(), c, context.getViewType().toString());
		org.skyve.metadata.view.Action a = v.getAction(tagName);

		// Guard against unsupported actions
		if (a != null && a.getImplicitName() != null && EXCLUDED_IMPLICIT_ACTIONS.contains(a.getImplicitName())) {
			throw new MetaDataException(String.format("<%s /> is an upload action: use <upload action=\"%s\" file=\"...\" />", tagName, tagName));
		}

		String identifier = button.getIdentifier(context);

		@SuppressWarnings("null") // uses a nullable result where the API expects non-null
		List<Locator> locators = context.getLocators(identifier);
		if (locators == null) {
			throw new MetaDataException(String.format("<%s /> is not on the view", tagName));
		}

		boolean success = false;
		for (Locator locator : locators) {
			if (! success) {
				selenide.trace(String.format("click [%s]", tagName));
				success = selenide.button(new SmartClientButtonCommand(locator.getLocator(), confirm));
			}
		}

		if (! success) {
			throw new DomainException(String.format("Could not click button [%s]: it is disabled", tagName));
		}

		// testSuccess="false" leaves any message for a following testSuccess or testFailure step to check;
		// a confirmation answered no ran nothing, so there is no outcome to check (messages showing are older)
		if (Boolean.FALSE.equals(confirm)) {
			selenide.trace("    Declined: the action did not run");
		}
		else if (BooleanUtils.isNotFalse(testSuccess)) {
			// an information message stays up for a testMessage step; the next click elsewhere dismisses it
			checkSuccess("expected no error messages after <" + tagName + " />");
		}
	}

	@Override
	public void executeOk(Ok ok) {
		button(ok, "ok", ok.getConfirm(), ok.getTestSuccess());
		closed(ok.getConfirm(), ok.getTestSuccess());
	}

	@SuppressWarnings("null") // a null here fails the test
	@Override
	public void executeSave(Save save) {
		button(save, "save", save.getConfirm(), save.getTestSuccess());

		// The browser now shows the create or edit view the script says: replace the context with one for that view,
		// so that the locators are the new view's
		Boolean createView = save.getCreateView();
		if (createView != null) {
			SmartClientAutomationContext context = pop();
			PushEditContext push = new PushEditContext();
			push.setModuleName(context.getModuleName());
			push.setDocumentName(context.getDocumentName());
			push.setCreateView(createView);
			push.setUxui(context.getUxui());
			push.setUserAgentType(context.getUserAgentType());
			push.execute(this);
		}
	}

	@Override
	public void executeCancel(Cancel cancel) {
		button(cancel, "cancel", null, cancel.getTestSuccess());
		closed(null, cancel.getTestSuccess());
	}

	@Override
	public void executeDelete(Delete delete) {
		button(delete, "delete", delete.getConfirm(), delete.getTestSuccess());
		closed(delete.getConfirm(), delete.getTestSuccess());
	}

	@Override
	public void executeZoomOut(ZoomOut zoomOut) {
		button(zoomOut, "zoom out", zoomOut.getConfirm(), zoomOut.getTestSuccess());
		closed(zoomOut.getConfirm(), zoomOut.getTestSuccess());
	}

	@Override
	public void executeRemove(Remove remove) {
		button(remove, "remove", remove.getConfirm(), remove.getTestSuccess());
		closed(remove.getConfirm(), remove.getTestSuccess());
	}

	/**
	 * Pops the context of a view that an action closed and waits for the browser to show the view underneath.
	 * A confirmation answered no leaves the view open, so nothing is popped; nor is it when the action was allowed to
	 * fail ({@code testSuccess="false"}) and the view is still showing (it refused to close, and its problems are left
	 * for a testFailure step).
	 *
	 * @param confirm		how the action's confirmation was answered
	 * @param testSuccess	the step's testSuccess
	 */
	@SuppressWarnings("null") // edit contexts have a module and document
	private void closed(@Nullable Boolean confirm, @Nullable Boolean testSuccess) {
		if (Boolean.FALSE.equals(confirm)) {
			return;
		}
		SmartClientAutomationContext closing = context();
		if (Boolean.FALSE.equals(testSuccess) && (ViewType.list != closing.getViewType()) &&
				selenide.showsView(closing.getModuleName(), closing.getDocumentName())) {
			selenide.trace("    The view did not close");
			return;
		}
		pop();
		SmartClientAutomationContext context = peek();
		if (context != null) {
			awaitView(context);
		}
	}

	/**
	 * Waits until the browser shows the view a context describes.
	 *
	 * @param context	the context
	 */
	@SuppressWarnings("null") // edit contexts have a module and document
	private void awaitView(@Nonnull SmartClientAutomationContext context) {
		if (ViewType.list == context.getViewType()) {
			selenide.waitForList();
		}
		else {
			selenide.waitForView(context.getModuleName(), context.getDocumentName());
		}
	}

	@Override
	public void executeAction(Action action) {
		button(action, action.getActionName(), action.getConfirm(), action.getTestSuccess());
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
	 * Performs a lookup description action on UI elements identified by the given binding.
	 *
	 * @param step the Step object representing the current automation step
	 * @param binding the binding string used to identify UI elements
	 * @param row optional row index for row-based lookup; if null, search parameter is used
	 * @param search optional search text for autocomplete lookup; used if row is null
	 */
	private void lookupDescription(@Nonnull Step step, @Nonnull String binding, @Nullable Integer row, @Nullable String search) {
		prepareForInteraction();
		List<Locator> locators = locators(step, binding);

		boolean success = false;
		for (Locator locator : locators) {
			if (! success) {
				if (row != null) {
					selenide.trace(String.format("Pick on row %d on lookup description [%s]", row, binding));
					success = selenide.lookupDescriptionByRow(new SmartClientLookupDescriptionByRowCommand(locator.getLocator(), row.intValue()));
				} else {
					selenide.trace(String.format("Auto complete with search '%s' on lookup description [%s]", search, binding));
					success = selenide.lookupDescriptionBySearch(new SmartClientLookupDescriptionBySearchCommand(locator.getLocator(), search));
				}
			}
		}

		if (! success) {
			throw new DomainException(String.format("Lookup description [%s] failed (is it disabled?)", binding));
		}
	}

	/**
	 * Returns the locators for an identifier, failing if there are none.
	 *
	 * @param step			the step, for the message
	 * @param identifier	the identifier
	 * @return the locators
	 */
	private @Nonnull List<Locator> locators(@Nonnull Step step, @Nonnull String identifier) {
		List<Locator> result = context().getLocators(identifier);
		if (result == null) {
			throw new MetaDataException(String.format("<%s /> with binding [%s] is not on the view",
														step.getClass().getSimpleName(), identifier));
		}
		return result;
	}

	@Override
	public void executeLookupDescriptionNew(LookupDescriptionNew nu) {
		lookupDescriptionMenu(nu, nu.getBinding(), true);
	}

	@Override
	public void executeLookupDescriptionEdit(LookupDescriptionEdit edit) {
		lookupDescriptionMenu(edit, edit.getBinding(), false);
	}

	/**
	 * Chooses New or Edit on a lookup description, then pushes the context of the view that opens.
	 *
	 * @param step		the step
	 * @param binding	the lookup description's binding
	 * @param nu		{@code true} for New, {@code false} for Edit
	 */
	private void lookupDescriptionMenu(@Nonnull Step step, @Nonnull String binding, boolean nu) {
		prepareForInteraction();
		List<Locator> locators = locators(step, binding);

		boolean success = false;
		for (Locator locator : locators) {
			if (! success) {
				selenide.trace(String.format("%s on lookup description [%s]", nu ? "New" : "Edit", binding));
				success = nu ? selenide.lookupDescriptionNew(locator.getLocator()) : selenide.lookupDescriptionEdit(locator.getLocator());
			}
		}

		if (! success) {
			throw new DomainException(String.format("%s on lookup description [%s] failed (is it disabled?)", nu ? "New" : "Edit", binding));
		}

		pushRelated(binding);
	}

	/**
	 * Pushes the edit context of the document a relation binding points to, and waits for its view to show.
	 *
	 * @param binding	the relation binding, relative to the current context's document
	 */
	private void pushRelated(@Nonnull String binding) {
		SmartClientAutomationContext context = context();

		User u = CORE.getUser();
		Customer c = u.getCustomer();
		@SuppressWarnings("null") // a null here fails the test; uses a nullable result where the API expects non-null
		Module m = c.getModule(context.getModuleName());
		@SuppressWarnings("null") // uses a nullable result where the API expects non-null
		Document d = m.getDocument(c, context.getDocumentName());
		TargetMetaData target = BindUtil.getMetaDataForBinding(c, m, d, binding);
		if (target.getAttribute() instanceof Relation relation) {
			String newDocumentName = relation.getDocumentName();
			d = m.getDocument(c, newDocumentName);
			String newModuleName = d.getOwningModuleName();

			// Push it
			PushEditContext push = new PushEditContext();
			push.setModuleName(newModuleName);
			push.setDocumentName(newDocumentName);
			push.execute(this);

			selenide.waitForView(newModuleName, newDocumentName);
		}
		else {
			throw new MetaDataException("Binding [" + binding + "] is not a relation, so it does not open a view");
		}
	}

	/**
	 * Clicks the zoom in button and pushes the context of the view it opens.
	 * With {@code testSuccess="false"}, a zoom in that is refused (an empty reference, a problem on the view)
	 * leaves its message for a following {@code testFailure} and opens no view, so no context is pushed.
	 */
	@Override
	public void executeZoomIn(ZoomIn zoom) {
		Boolean testSuccess = zoom.getTestSuccess();
		button(zoom, "zoomIn", null, testSuccess);
		if (Boolean.FALSE.equals(testSuccess) && (selenide.dialogKind() != null)) {
			selenide.trace("    The zoom in was refused: " + selenide.dialogMessage());
			return;
		}
		pushRelated(zoom.getBinding());
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
	 * Selects the row, then clicks the grid's Edit button so the row is edited inline.
	 * Following {@code dataEnter} steps address the grid's columns by their bindings,
	 * until any other kind of step ends the inline edit.
	 */
	@Override
	public void executeDataGridEdit(DataGridEdit edit) {
		dataGridGesture(edit, edit.getBinding(), edit.getRow());
	}

	/**
	 * Finds the row a {@code rowContaining} step means: the first row, in the order shown, with a cell showing the text.
	 *
	 * @param context	the context
	 * @param rowsKey	the identifier of the grid's row locator
	 * @param step		the step
	 * @return the zero-based row
	 */
	@SuppressWarnings("null") // checked by the caller
	private int rowContaining(@Nonnull SmartClientAutomationContext context, @Nonnull String rowsKey, @Nonnull GridRowStep step) {
		List<Locator> rows = context.getLocators(rowsKey);
		if (rows == null) {
			throw new MetaDataException(String.format("<%s /> grid [%s] is not on the view", step.getClass().getSimpleName(), rowsKey));
		}
		String text = substituteTokens(step.getRowContaining());
		selenide.trace(String.format("Find the row containing '%s'", text));
		return selenide.rowContaining(rows.get(0).getLocator(), text);
	}

	/**
	 * Executes a data grid gesture (new, select, zoom, edit or remove) on the UI element identified by the binding.
	 *
	 * @param step the Step object representing the data grid action to perform
	 * @param binding the binding string used to identify the data grid UI elements
	 * @param row optional row index for row-specific gestures; required for select, zoom, and remove actions
	 */
	private void dataGridGesture(@Nonnull Step step, @Nonnull String binding, @Nullable Integer row) {
		prepareForInteraction();
		SmartClientAutomationContext context = context();
		@SuppressWarnings("null") // uses a nullable result where the API expects non-null
		String buttonIdentifier = step.getIdentifier(context);

		@SuppressWarnings("null") // a null here fails the test; uses a nullable result where the API expects non-null
		List<Locator> locators = context.getLocators(buttonIdentifier);
		if (locators == null) {
			throw new MetaDataException(String.format("<%s /> with binding [%s] is not on the view", step.getClass()
					.getSimpleName(), binding));
		}
		boolean inline = (context.getLocators(binding + ".inline") != null);

		if ((row == null) && (step instanceof GridRowStep gridRow) && (gridRow.getRowContaining() != null)) {
			row = Integer.valueOf(rowContaining(context, binding + ".select", gridRow));
		}
		if ((! (step instanceof DataGridNew)) && (row == null)) {
			throw new MetaDataException(String.format("<%s /> with binding [%s] needs a row", step.getClass().getSimpleName(), binding));
		}

		// Must select row before completing zoom, edit or remove actions
		if ((step instanceof DataGridZoom) || (step instanceof DataGridRemove) || (step instanceof DataGridEdit)) {
			DataGridSelect select = new DataGridSelect();
			select.setBinding(binding);
			select.setRow(row);
			executeDataGridSelect(select);
		}

		boolean success = false;
		if (step instanceof DataGridEdit) {
			// clicking a row of an inline grid can start editing it straight away
			for (Locator locator : locators) {
				if ((! success) && (row != null) && (selenide.editRow(locator.getLocator()) == row.intValue())) {
					selenide.trace(String.format("Row %d of data grid [%s] is being edited", row, binding));
					success = true;
				}
			}
		}
		for (Locator locator : locators) {
			if (! success) {
				if (step instanceof DataGridSelect) {
					selenide.trace(String.format("Select on row %d on data grid [%s]", row, binding));
					@SuppressWarnings("null") // checked above
					int r = row.intValue();
					success = selenide.dataGridSelect(new SmartClientDataGridSelectCommand(locator.getLocator(), r));
				}
				else {
					selenide.trace(String.format("%s on data grid [%s]%s",
													step.getClass().getSimpleName(),
													binding,
													(row == null) ? "" : " row " + row));
					success = selenide.dataGridButton(new SmartClientDataGridButtonCommand(locator.getLocator(), null));
				}
			}
		}

		if (! success) {
			throw new DomainException(String.format("<%s /> on data grid [%s] failed: the grid's button is disabled",
														step.getClass().getSimpleName(), binding));
		}

		if ((step instanceof DataGridEdit) || (inline && (step instanceof DataGridNew))) {
			// the row is now being edited inline
			context.setInlineGridBinding(binding);
		}
		else if ((step instanceof DataGridNew) || (step instanceof DataGridZoom)) {
			pushRelated(binding);
		}
	}

	@Override
	public void executeListGridNew(ListGridNew nu) {
		listGridGesture(nu, null);

		PushEditContext push = listGridContext(nu.getQueryName(), nu.getDocumentName(), nu.getModelName(), nu);
		push.setCreateView(nu.getCreateView());
		push.execute(this);

		selenide.waitForView(push.getModuleName(), push.getDocumentName());
	}

	@Override
	public void executeListGridZoom(ListGridZoom zoom) {
		listGridGesture(zoom, zoom.getRow());

		PushEditContext push = listGridContext(zoom.getQueryName(), zoom.getDocumentName(), zoom.getModelName(), zoom);
		push.execute(this);

		selenide.waitForView(push.getModuleName(), push.getDocumentName());
	}

	@Override
	public void executeListGridSelect(ListGridSelect select) {
		listGridGesture(select, select.getRow());
	}

	/**
	 * Performs a list grid gesture (new, select, or zoom) on UI elements identified by the step's binding.
	 *
	 * @param step the Step object representing the list grid action to perform
	 * @param row the row index for row-specific actions; may be required depending on the step type
	 */
	private void listGridGesture(@Nonnull Step step, @Nullable Integer row) {
		prepareForInteraction();
		SmartClientAutomationContext context = context();
		@SuppressWarnings("null") // uses a nullable result where the API expects non-null
		String buttonIdentifier = step.getIdentifier(context);
		@SuppressWarnings("null") // a null here fails the test
		String listGridIdentifier = buttonIdentifier.substring(0, buttonIdentifier.lastIndexOf('.'));

		@SuppressWarnings("null") // a null here fails the test
		List<Locator> locators = context.getLocators(buttonIdentifier);
		if (locators == null) {
			throw new MetaDataException(String.format("<%s /> with identifier [%s] is not defined", step.getClass()
					.getSimpleName(), listGridIdentifier));
		}
		if ((row == null) && (step instanceof GridRowStep gridRow) && (gridRow.getRowContaining() != null)) {
			row = Integer.valueOf(rowContaining(context, listGridIdentifier + ".select", gridRow));
		}
		if ((! (step instanceof ListGridNew)) && (row == null)) {
			throw new MetaDataException(String.format("<%s /> on list grid [%s] needs a row", step.getClass().getSimpleName(), listGridIdentifier));
		}

		if (step instanceof ListGridZoom zoom) {
			// Must select row before completing zoom action
			ListGridSelect select = new ListGridSelect();
			select.setModuleName(zoom.getModuleName());
			select.setDocumentName(zoom.getDocumentName());
			select.setModelName(zoom.getModelName());
			select.setQueryName(zoom.getQueryName());
			select.setRow(row);

			executeListGridSelect(select);
		}

		boolean success = false;
		for (Locator locator : locators) {
			if (! success) {
				if (step instanceof ListGridSelect) {
					selenide.trace(String.format("Select on row %d on list grid [%s]", row, listGridIdentifier));
					@SuppressWarnings("null") // checked above
					int r = row.intValue();
					success = selenide.listGridSelect(new SmartClientListGridSelectCommand(locator.getLocator(), r));
				} else {
					selenide.trace(String.format("%s on list grid [%s]%s",
													(step instanceof ListGridNew) ? "New" : "Zoom",
													listGridIdentifier,
													(row == null) ? "" : " row " + row));
					success = selenide.listGridButton(new SmartClientListGridButtonCommand(locator.getLocator(), null));
				}
			}
		}

		if (! success) {
			throw new DomainException(String.format("<%s /> on list grid [%s] failed: the grid's button is disabled",
														step.getClass().getSimpleName(), listGridIdentifier));
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
	@SuppressWarnings("null") // a null here fails the test; uses a nullable result where the API expects non-null; checks values the annotations say are non-null
	private @Nonnull PushEditContext listGridContext(@Nullable String queryName,
														@Nullable String documentName,
														@Nullable String modelName,
														@Nonnull Step step) {
		PushEditContext result = new PushEditContext();

		SmartClientAutomationContext context = context();

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

	/**
	 * Compares the value the widget bound to the binding displays with the expected value.
	 * <p>
	 * The displayed value is compared as a trimmed string: checkboxes display {@code true} or {@code false},
	 * combos and radios their option's text, dates and times their formatted text, rich text its text
	 * (or its HTML), sliders their number. A lookup description displays its description.
	 */
	@Override
	public void executeTestValue(TestValue test) {
		if (isNoneStrategy()) {
			return;
		}
		prepareForDataEntry(); // a row being edited inline stays in edit
		SmartClientAutomationContext context = context();
		String binding = test.getBinding();
		List<Locator> locators = inputLocators(context, binding);
		if (locators == null) {
			throw new MetaDataException(String.format("<testValue /> with binding [%s] is not on the view", binding));
		}

		// A text area or rich text editor shows new lines; other widgets were given spaces for them (see executeDataEnter)
		String expectedValue = test.getValue();
		String multiLineExpected = (expectedValue == null) ? "" : substituteTokens(expectedValue).replace("\r\n", "\n").trim();
		Locator first = locators.get(0);
		String expected = ((multiLineExpected.indexOf('\n') >= 0) && multiLine(first)) ? multiLineExpected : multiLineExpected.replace('\n', ' ');
		List<String> actual = selenide.values(first.getLocator());
		selenide.trace(String.format("Test value of %s is '%s' (displayed '%s')", binding, expected, actual.get(0)));
		for (String value : actual) {
			if (expected.equals(value.replace("\r\n", "\n").trim())) {
				return;
			}
		}

		String shown = actual.get(0);
		String alternatives = (actual.size() > 1) ? " (value " + actual.subList(1, actual.size()) + ')' : "";
		String wanted = expected;
		check(false, () -> String.format("<testValue /> of [%s] expected '%s' but the widget displays '%s'%s",
											binding, wanted, shown, alternatives));
	}

	// ------------------------------------------------------------------ phase 2 steps

	/**
	 * Uploads a test classpath file to a content widget (through its Upload button) or with an upload action,
	 * using Skyve's upload page in the window that opens.
	 */
	@Override
	public void executeUpload(Upload upload) {
		prepareForInteraction();
		SmartClientAutomationContext context = context();
		String identifier = upload.getIdentifier(context);
		List<Locator> locators = context.getLocators(identifier);
		if (locators == null) {
			throw new MetaDataException(String.format("<upload /> with %s [%s] is not on the view",
														(upload.getBinding() != null) ? "binding" : "action", identifier));
		}
		java.io.File file = testResourceFile(upload.getFile());
		String locator = locators.get(0).getLocator();
		selenide.trace(String.format("upload %s to [%s]", upload.getFile(), identifier));
		org.openqa.selenium.WebElement trigger;
		if (upload.getBinding() != null) {
			trigger = selenide.uploadButton(locator);
		}
		else {
			SmartClientSelenide.Found found = selenide.require(locator);
			if (found.disabled()) {
				throw new DomainException("<upload /> action [" + identifier + "] is disabled");
			}
			trigger = found.element().toWebElement();
		}
		selenide.upload(trigger, file);
		if (upload.getBinding() != null) {
			List<String> values = selenide.values(locator);
			check(! values.get(0).isEmpty() || ((values.size() > 1) && (! values.get(1).isEmpty()) && (! "null".equals(values.get(1)))),
					() -> String.format("<upload /> to [%s] finished but the widget holds no content", identifier));
		}
		checkSuccess(String.format("expected no error messages after uploading to [%s]", identifier));
	}

	/**
	 * Asserts that a message (of the type, if given) contains the text: a SmartClient information or warning dialog,
	 * the Problems dialog or a field error (error), or a growl, once the page has settled.
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
		// messages() waits for the page to settle, after which the step's messages are showing
		boolean found = selenide.messages(severity).stream().anyMatch(m -> m.contains(contains));
		check(found, () -> String.format("expected %s message containing '%s' but found: %s",
											(severity == null) ? "a" : "an " + severity, contains, selenide.describeMessages()));
	}

	/**
	 * Counts the rows of a list or data grid (only those containing the text, if given) and checks the bounds.
	 * With the script's timeout, a list grid fetches its rows again and they are recounted until the bounds hold or the time is up.
	 */
	@Override
	public void executeTestGridRows(TestGridRows test) {
		Integer timeout = test.getTimeoutMillis();
		// Under None there is nothing to check, but a timeout still waits: scripts use it to wait for jobs
		if (isNoneStrategy() && (timeout == null)) {
			return;
		}
		prepareForInteraction();
		SmartClientAutomationContext context = context();
		String identifier = test.getIdentifier(context);
		List<Locator> locators = context.getLocators(identifier + ".select");
		if (locators == null) {
			throw new MetaDataException(String.format("<testGridRows /> grid [%s] is not on the view", identifier));
		}
		String locator = locators.get(0).getLocator();
		String text = test.getRowContaining();
		String contains = (text == null) ? null : substituteTokens(text);
		Integer max = test.getMaxRows();
		selenide.trace(String.format("test grid [%s] has %d..%s rows%s", identifier, Integer.valueOf(test.getEffectiveMinRows()),
										(max == null) ? "" : max.toString(), (contains == null) ? "" : " containing '" + contains + "'"));
		int rows = selenide.awaitRows(locator, contains, c -> rowCountHolds(test, c), (timeout == null) ? 0L : timeout.longValue());
		if (! isNoneStrategy()) {
			checkRowCount(test, identifier, contains, rows);
		}
	}

	/**
	 * Asserts that a widget (by binding), an action's button or a tab is shown or not, and enabled or not.
	 */
	@Override
	public void executeTestWidget(TestWidget test) {
		if (isNoneStrategy()) {
			return;
		}
		prepareForDataEntry();
		SmartClientAutomationContext context = context();
		String identifier = test.getIdentifier(context);
		String tabPath = test.getTabPath();
		if (tabPath != null) {
			String[] titles = tabPath.split("::");
			List<Locator> tab = context.getLocators(tabPath + " Tab");
			identifier = (tab != null) ? tabPath + " Tab" : titles[titles.length - 1] + " Tab";
		}
		List<Locator> locators = (test.getBinding() != null) ? inputLocators(context, identifier) : context.getLocators(identifier);
		if (locators == null) {
			throw new MetaDataException(String.format("<testWidget /> [%s] is not in the view", identifier));
		}
		boolean shown = false;
		boolean enabled = false;
		for (Locator locator : locators) {
			boolean[] state = selenide.widgetState(locator.getLocator());
			if (state[1]) {
				shown = true;
				enabled = enabled || state[2];
			}
		}
		checkWidget(test, identifier, shown, enabled);
	}

	/**
	 * Clicks through the SmartClient menu (the module's section, then the item) and sets the context as the
	 * equivalent navigation would.
	 */
	@Override
	public void executeNavigateMenu(NavigateMenu menu) {
		prepareForDataEntry();
		SailMenuPath path = resolveMenuPath(menu.getModuleName(), menu.getItemName());
		MenuItem item = path.item;
		if (item == null) {
			throw new MetaDataException(String.format("<navigateMenu /> module %s has no menu item %s", menu.getModuleName(), menu.getItemName()));
		}
		Customer c = CORE.getUser().getCustomer();
		Module m = c.getModule(menu.getModuleName());
		selenide.trace(String.format("Menu %s / %s", path.moduleTitle, String.join(" / ", path.labels())));
		if (item instanceof AbstractDocumentOrQueryOrModelMenuItem listItem) {
			PushListContext push = new PushListContext();
			push.setModuleName(m.getName());
			push.setDocumentName(listItem.getDocumentName());
			push.setQueryName(listItem.getQueryName());
			push.setModelName(listItem.getModelName());
			executePushListContext(push);
		}
		else if (item instanceof EditItem editItem) {
			Document d = m.getDocument(c, java.util.Objects.requireNonNull(editItem.getDocumentName()));
			PushEditContext push = new PushEditContext();
			push.setModuleName(d.getOwningModuleName());
			push.setDocumentName(d.getName());
			executePushEditContext(push);
		}
		else {
			SmartClientAutomationContext context = peek();
			if (context != null) {
				context.setDocumentName(null);
				context.setViewType(null);
			}
		}
		int windows = selenide.openWindows();
		if (windows > 0) {
			throw new IllegalStateException(String.format("<navigateMenu /> %s / %s: %d window(s) are open over the menu (%s): close them first",
															path.moduleTitle, String.join(" / ", path.labels()), Integer.valueOf(windows), selenide.state()));
		}
		if (! selenide.menu(path.moduleTitle, path.labels(), true)) {
			throw new DomainException(String.format("<navigateMenu /> %s / %s is not in the menu", path.moduleTitle, String.join(" / ", path.labels())));
		}
		SmartClientAutomationContext context = peek();
		if ((context != null) && (context.getViewType() != null)) {
			awaitView(context);
		}
	}

	/**
	 * Asserts that a module's menu section, or one of its entries, is shown or not.
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
		checkMenu(test, path, selenide.menu(path.moduleTitle, path.labels(), false));
	}

	// ------------------------------------------------------------------ renderer hooks

	/**
	 * Filters the column in the list grid's filter row (substring for text, equality otherwise), or with the grid's
	 * advanced filter for the other operators and the columns the row does not show, then checks that no error shows:
	 * see {@link SmartClientSelenide#filterListGrid}.
	 */
	@Override
	protected void filterListGrid(@Nonnull ListGridFilter filter, @Nonnull String column, @Nullable String value, @Nonnull FilterOperator operator) {
		prepareForInteraction();
		selenide.filterListGrid(listGridRows(filter), sanitised(column), value, operator.name());
		checkSuccess("expected no error messages after <listGridFilter />");
	}

	/**
	 * Presses the list grid's Clear filter button.
	 */
	@Override
	protected void clearListGridFilters(@Nonnull ListGridClearFilter clear) {
		prepareForInteraction();
		selenide.clearListGridFilters(listGridRows(clear));
		checkSuccess("expected no error messages after <listGridClearFilter />");
	}

	/**
	 * Clicks the column's header until the list grid is sorted by it alone, in the direction asked for.
	 */
	@Override
	protected void sortListGrid(@Nonnull ListGridSort sort, @Nonnull String column, boolean descending) {
		prepareForInteraction();
		selenide.sortListGrid(listGridRows(sort), sanitised(column), descending);
		checkSuccess("expected no error messages after <listGridSort />");
	}

	/**
	 * Returns the locator of the rows of the list grid a step names.
	 *
	 * @param step	the list grid step
	 * @return the locator
	 */
	private @Nonnull String listGridRows(@Nonnull Step step) {
		SmartClientAutomationContext context = context();
		String identifier = step.getIdentifier(context);
		List<Locator> locators = context.getLocators(identifier + ".select");
		if (locators == null) {
			throw new MetaDataException(String.format("<%s /> list grid [%s] is not on the view",
														Introspector.decapitalize(step.getClass().getSimpleName()), identifier));
		}
		return locators.get(0).getLocator();
	}

	/**
	 * Returns a column's binding as the grid names its field.
	 *
	 * @param column	the binding
	 * @return the sanitised binding
	 */
	private static @Nonnull String sanitised(@Nonnull String column) {
		return Objects.requireNonNull(BindUtil.sanitiseBinding(column));
	}

	/**
	 * Reads what the first displayed widget for the binding shows, as {@code testValue} reads it.
	 */
	@Override
	protected @Nullable String readWidgetValue(@Nonnull CaptureValue capture) {
		prepareForDataEntry(); // a row being edited inline stays in edit
		String binding = capture.getBinding();
		List<Locator> locators = inputLocators(context(), binding);
		if (locators == null) {
			throw new MetaDataException(String.format("<captureValue /> with binding [%s] is not on the view", binding));
		}
		for (Locator locator : locators) {
			if (selenide.widgetState(locator.getLocator())[1]) {
				return selenide.value(locator.getLocator());
			}
		}
		throw new IllegalStateException(String.format("<captureValue /> binding [%s] has no widget showing", binding));
	}

	/**
	 * Chooses the combo or radio option with the code or label: the option's position is found in the browser, then it is
	 * chosen as {@code dataEnter} chooses a position.
	 */
	@Override
	protected void enterOption(@Nonnull DataEnter dataEnter, @Nonnull String value, @Nonnull Match match) {
		prepareForDataEntry();
		SmartClientAutomationContext context = context();
		String identifier = dataEnter.getIdentifier(context);
		List<Locator> locators = inputLocators(context, identifier);
		if (locators == null) {
			throw new MetaDataException(
					String.format("<dataEnter /> with binding [%s] is not supported or is not on the view", identifier));
		}

		boolean option = false;
		boolean success = false;
		for (Locator locator : locators) {
			InputType inputType = locator.getInputType();
			if ((! success) && ((inputType == InputType.COMBO) || (inputType == InputType.RADIO))) {
				option = true;
				int index = selenide.optionIndex(locator.getLocator(), value, match == Match.code);
				selenide.trace(String.format("set %s to the option with %s %s (option %d)", identifier, match, value, Integer.valueOf(index)));
				success = enter(locator.getLocator(), inputType, String.valueOf(index));
			}
		}

		if (! option) {
			throw new MetaDataException(String.format("<dataEnter match=\"%s\" /> with binding [%s] needs a combo or radio, but its widget is neither",
														match, identifier));
		}
		if (! success) {
			throw new DomainException(String.format("Data entry failed for binding [%s] (is the widget disabled or read only?)", identifier));
		}

		selenide.waitForFullPageResponse();
	}

	/**
	 * Opens the lookup's drop-down with its picker and picks the first row that contains the text.
	 */
	@Override
	protected void pickLookupDescriptionRowContaining(@Nonnull LookupDescriptionPick pick, @Nonnull String rowContaining) {
		lookupDescriptionContaining(pick, pick.getBinding(), null, rowContaining);
	}

	/**
	 * Types the search into the lookup and picks the first suggestion that contains the pick text.
	 */
	@Override
	protected void autoCompleteLookupDescriptionAndPick(@Nonnull LookupDescriptionAutoComplete complete, @Nonnull String search, @Nonnull String pick) {
		lookupDescriptionContaining(complete, complete.getBinding(), search, pick);
	}

	/**
	 * Picks the first row of a lookup description's drop-down that contains the text.
	 *
	 * @param step		the step
	 * @param binding	the lookup description's binding
	 * @param search	the text to type, or {@code null} to open the drop-down with its picker
	 * @param text		the text the row contains
	 */
	private void lookupDescriptionContaining(@Nonnull Step step, @Nonnull String binding, @Nullable String search, @Nonnull String text) {
		prepareForInteraction();
		List<Locator> locators = locators(step, binding);

		boolean success = false;
		for (Locator locator : locators) {
			if (! success) {
				selenide.trace(String.format("%s lookup description [%s] and pick the row containing '%s'",
												(search == null) ? "Open" : "Search '" + search + "' in", binding, text));
				success = selenide.lookupDescriptionContaining(locator.getLocator(), search, text);
			}
		}

		if (! success) {
			throw new DomainException(String.format("Lookup description [%s] failed (is it disabled?)", binding));
		}
	}

	/**
	 * Clicks the first displayed link on the page whose text contains the text, then waits for its page, or for
	 * SmartClient to settle. Fails listing the page's links if none contains the text.
	 */
	@Override
	protected void clickLinkContaining(@Nonnull NavigateLink link, @Nonnull String text) {
		prepareForInteraction();
		if (! selenide.followLinkContaining(text)) {
			throw new IllegalStateException(String.format("<navigateLink text=\"%s\" /> found no displayed link containing it: the page's links are %s",
															text, selenide.linkTexts()));
		}
	}

	/**
	 * Returns the current context.
	 *
	 * @return the context
	 * @throws IllegalStateException if no view has been navigated to
	 */
	private @Nonnull SmartClientAutomationContext context() {
		SmartClientAutomationContext result = peek();
		if (result == null) {
			throw new IllegalStateException("There is no view to interact with: navigate to one first");
		}
		return result;
	}

	/**
	 * Ends any inline grid edit, then checks that no dialog is in the way.
	 */
	private void prepareForInteraction() {
		SmartClientAutomationContext context = peek();
		if ((context != null) && (context.getInlineGridBinding() != null)) {
			selenide.trace("End inline editing of data grid [" + context.getInlineGridBinding() + ']');
			selenide.endInlineEditing();
			context.setInlineGridBinding(null);
		}
		prepareForDataEntry();
	}

	/**
	 * Checks that no dialog is in the way. A message left by an earlier step is dismissed now that the step needs the
	 * view; an unanswered question fails the step, because answering it is the script's job.
	 */
	private void prepareForDataEntry() {
		String kind = selenide.dialogKind();
		if ("ask".equals(kind)) {
			throw new IllegalStateException("An unanswered question is showing: " + selenide.dialogMessage());
		}
		if (kind != null) {
			selenide.ok();
		}
	}
}
