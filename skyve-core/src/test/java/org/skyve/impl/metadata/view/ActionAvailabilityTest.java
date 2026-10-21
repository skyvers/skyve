package org.skyve.impl.metadata.view;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.beans.Introspector;
import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Stream;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.function.Executable;
import org.mockito.Mockito;
import org.skyve.domain.Bean;
import org.skyve.domain.messages.ActionUnavailableException;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.module.ModuleImpl;
import org.skyve.impl.metadata.view.component.Component;
import org.skyve.impl.metadata.view.component.ComponentTranslation;
import org.skyve.impl.metadata.view.container.Tab;
import org.skyve.impl.metadata.view.container.TabPane;
import org.skyve.impl.metadata.view.container.VBox;
import org.skyve.impl.metadata.view.container.form.Form;
import org.skyve.impl.metadata.view.container.form.FormItem;
import org.skyve.impl.metadata.view.container.form.FormRow;
import org.skyve.impl.metadata.view.event.EventAction;
import org.skyve.impl.metadata.view.event.ServerSideActionEventAction;
import org.skyve.impl.metadata.view.reference.ActionReference;
import org.skyve.impl.metadata.view.reference.ExternalReference;
import org.skyve.impl.metadata.view.widget.Button;
import org.skyve.impl.metadata.view.widget.Link;
import org.skyve.impl.metadata.view.widget.bound.input.LookupDescription;
import org.skyve.impl.metadata.view.widget.bound.input.TextField;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGrid;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGridBoundColumn;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGridColumn;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGridContainerColumn;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataRepeater;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.metadata.MetaData;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.model.document.Association;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.View.ViewType;

/**
 * Tests {@link ActionAvailability} exhaustively: each view is checked under every truth assignment of its
 * conditions, against the expected outcome written independently as plain boolean logic.
 */
class ActionAvailabilityTest {
	private static final String UXUI = "desktop";
	private static final String MODULE_NAME = "m";
	private static final String DOCUMENT_NAME = "D";
	private static final String ROW_DOCUMENT_NAME = "R";

	private CustomerImpl customer;
	private ModuleImpl module;
	private DocumentImpl document;
	private DocumentImpl rowDocument;
	private User user;

	@BeforeEach
	void setUp() {
		customer = mock(CustomerImpl.class);
		module = mock(ModuleImpl.class);
		document = mock(DocumentImpl.class);
		rowDocument = mock(DocumentImpl.class);
		user = mock(User.class);
		when(user.getCustomer()).thenReturn(customer);
		when(user.getName()).thenReturn("tester");
		when(customer.getModule(MODULE_NAME)).thenReturn(module);
		when(module.getName()).thenReturn(MODULE_NAME);
		when(module.getDocument(customer, DOCUMENT_NAME)).thenReturn(document);
		when(document.getOwningModuleName()).thenReturn(MODULE_NAME);
		when(document.getName()).thenReturn(DOCUMENT_NAME);
		when(rowDocument.getOwningModuleName()).thenReturn(MODULE_NAME);
		when(rowDocument.getName()).thenReturn(ROW_DOCUMENT_NAME);
	}

	@Test
	void toolbarActionIsAvailableWhenVisibleAndEnabled() {
		ViewImpl view = editView();
		ActionImpl action = customAction("act", "ActClass");
		action.setXmlInvisibleCondition("hidden");
		action.setXmlEnabledCondition("allowed"); // disabled when notAllowed
		view.putAction(action);
		ImplicitActionName[] classBased = {ImplicitActionName.BizExport, ImplicitActionName.BizImport, ImplicitActionName.Download, ImplicitActionName.Upload};
		for (ImplicitActionName implicitName : classBased) {
			ActionImpl classAction = new ActionImpl();
			classAction.setImplicitName(implicitName);
			classAction.setResourceName(implicitName + "Class");
			classAction.setXmlInvisibleCondition("hidden");
			classAction.setXmlEnabledCondition("allowed");
			view.putAction(classAction);
		}

		Predicate<Map<String, Boolean>> expected = t -> (! is(t, "hidden")) && is(t, "allowed");
		List<Stream<Executable>> streams = new ArrayList<>();
		// The server looks the class up by the requested text, so a request by a name that differs from the class is refused
		streams.add(cases("toolbar by name differing from its class", t -> false, t -> passes(view, t, "act"), "hidden", "allowed"));
		streams.add(cases("toolbar by class", expected, t -> passes(view, t, "ActClass"), "hidden", "allowed"));
		streams.add(cases("toolbar other", t -> false, t -> passes(view, t, "Other"), "hidden", "allowed"));
		for (ImplicitActionName implicitName : classBased) {
			streams.add(cases("toolbar " + implicitName + " by class", expected, t -> passes(view, t, implicitName + "Class"), "hidden", "allowed"));
			streams.add(cases("toolbar " + implicitName + " by implicit name", expected, t -> passes(view, t, implicitName), "hidden", "allowed"));
		}
		assertAll(streams.stream().flatMap(Function.identity()));
	}

	@Test
	void buttonOutsideActionPanelNeedsVisibleEnabledForm() {
		ViewImpl view = editView();
		ActionImpl action = customAction("btn", "BtnClass");
		action.setInActionPanel(Boolean.FALSE);
		action.setXmlDisabledCondition("actionDisabled");
		action.setXmlInvisibleCondition("actionHidden");
		view.putAction(action);
		Form form = new Form();
		form.setXmlDisabledCondition("formDisabled");
		form.setXmlInvisibleCondition("formHidden");
		addToForm(form, button("btn"));
		view.getContained().add(form);

		String[] conditions = {"actionDisabled", "actionHidden", "formDisabled", "formHidden"};
		assertAll(Stream.of(cases("button in form",
									t -> (! is(t, "actionDisabled")) && (! is(t, "actionHidden")) && (! is(t, "formDisabled")) && (! is(t, "formHidden")),
									t -> passes(view, t, "BtnClass"),
									conditions),
								cases("button other", t -> false, t -> passes(view, t, "Other"), conditions))
							.flatMap(Function.identity()));
	}

	@Test
	void buttonInTabNeedsVisibleEnabledTab() {
		ViewImpl view = editView();
		ActionImpl action = customAction("tabbed", "TabbedClass");
		action.setInActionPanel(Boolean.FALSE);
		view.putAction(action);
		TabPane pane = new TabPane();
		pane.setXmlInvisibleCondition("paneHidden");
		pane.setXmlDisabledCondition("false"); // a literal
		Tab tab = new Tab();
		tab.setXmlDisabledCondition("tabDisabled");
		tab.setXmlVisibleCondition("tabShown"); // invisible when notTabShown
		tab.getContained().add(button("tabbed"));
		pane.getTabs().add(tab);
		view.getContained().add(pane);

		assertAll(cases("button in tab",
							t -> (! is(t, "paneHidden")) && (! is(t, "tabDisabled")) && is(t, "tabShown"),
							t -> passes(view, t, "TabbedClass"),
							"paneHidden", "tabDisabled", "tabShown"));
	}

	@Test
	void eventActionOnVisibleFieldIgnoresActionAndEnabledConditions() {
		ViewImpl view = editView();
		ActionImpl action = customAction("evt", "EvtClass");
		action.setInActionPanel(Boolean.FALSE);
		action.setXmlDisabledCondition("actionDisabled");
		action.setXmlInvisibleCondition("actionHidden");
		view.putAction(action);
		Form form = new Form();
		form.setXmlDisabledCondition("formDisabled");
		TextField field = new TextField();
		field.setBinding("text");
		field.setXmlDisabledCondition("fieldDisabled");
		field.getChangedActions().add(server("evt"));
		addToForm(form, field);
		view.getContained().add(form);

		String[] conditions = {"actionDisabled", "actionHidden", "formDisabled", "fieldDisabled"};
		assertAll(Stream.of(cases("event on visible field", t -> true, t -> passes(view, t, "EvtClass"), conditions),
								cases("event other", t -> false, t -> passes(view, t, "Other"), conditions))
							.flatMap(Function.identity()));
	}

	@Test
	void eventActionNeedsVisibleWidgetForEveryHandler() {
		// Each handler type is on its own widget with its own condition, so a handler attributed to the wrong widget shows
		ViewImpl view = editView();
		VBox box = new VBox();
		box.setXmlInvisibleCondition("boxHidden");
		Form form = new Form();
		TextField changed = textField("changedHidden");
		changed.getChangedActions().add(server(eventAction(view, "changed")));
		addToForm(form, changed);
		TextField focus = textField("focusHidden");
		focus.getFocusActions().add(server(eventAction(view, "focus")));
		addToForm(form, focus);
		TextField blur = textField("blurHidden");
		blur.getBlurActions().add(server(eventAction(view, "blur")));
		addToForm(form, blur);
		LookupDescription picked = lookup("pickedHidden");
		picked.getPickedActions().add(server(eventAction(view, "picked")));
		addToForm(form, picked);
		LookupDescription cleared = lookup("clearedHidden");
		cleared.getClearedActions().add(server(eventAction(view, "cleared")));
		addToForm(form, cleared);
		box.getContained().add(form);
		DataGrid added = grid("addedHidden");
		added.getAddedActions().add(server(eventAction(view, "added")));
		box.getContained().add(added);
		DataGrid edited = grid("editedHidden");
		edited.getEditedActions().add(server(eventAction(view, "edited")));
		box.getContained().add(edited);
		DataGrid removed = grid("removedHidden");
		removed.getRemovedActions().add(server(eventAction(view, "removed")));
		box.getContained().add(removed);
		DataGrid selected = grid("selectedHidden");
		selected.getSelectedActions().add(server(eventAction(view, "selected")));
		box.getContained().add(selected);
		view.getContained().add(box);

		String[] names = {"changed", "focus", "blur", "picked", "cleared", "added", "edited", "removed", "selected"};
		String[] conditions = new String[names.length + 1];
		conditions[0] = "boxHidden";
		for (int i = 0; i < names.length; i++) {
			conditions[i + 1] = names[i] + "Hidden";
		}
		List<Stream<Executable>> streams = new ArrayList<>();
		for (String name : names) {
			streams.add(cases("event " + name, t -> (! is(t, "boxHidden")) && (! is(t, name + "Hidden")), t -> passes(view, t, name + "Class"), conditions));
		}
		assertAll(streams.stream().flatMap(Function.identity()));
	}

	@Test
	void eventActionOfGridColumnWidgetNeedsVisibleGridOnly() {
		ViewImpl view = editView();
		VBox box = new VBox();
		box.setXmlInvisibleCondition("boxHidden");
		DataGrid grid = grid("gridHidden");
		TextField editor = textField("editorHidden"); // not applied, as SmartClient ignores it for a column
		editor.getChangedActions().add(server(eventAction(view, "cellChanged")));
		WidgetReference reference = new WidgetReference();
		reference.setWidget(editor);
		DataGridBoundColumn column = new DataGridBoundColumn();
		column.setBinding("text");
		column.setInputWidget(reference);
		grid.getColumns().add(column);
		// Visited after the column, so it needs the grid's own visibility again
		grid.getAddedActions().add(server(eventAction(view, "gridAdded")));
		box.getContained().add(grid);
		view.getContained().add(box);

		Predicate<Map<String, Boolean>> expected = t -> (! is(t, "boxHidden")) && (! is(t, "gridHidden"));
		assertAll(Stream.of(cases("grid column event", expected, t -> passes(view, t, "cellChangedClass"), "boxHidden", "gridHidden", "editorHidden"),
								cases("grid event after the column", expected, t -> passes(view, t, "gridAddedClass"), "boxHidden", "gridHidden", "editorHidden"))
							.flatMap(Function.identity()));
	}

	@Test
	void sharedClassIsAvailableThroughEitherAction() {
		ViewImpl view = editView();
		ActionImpl first = customAction("first", "Shared");
		first.setXmlDisabledCondition("firstDisabled");
		view.putAction(first);
		ActionImpl second = customAction("second", "Shared");
		second.setXmlDisabledCondition("secondDisabled");
		view.putAction(second);

		assertAll(Stream.of(cases("shared by name first", t -> false, t -> passes(view, t, "first"), "firstDisabled", "secondDisabled"),
								cases("shared by name second", t -> false, t -> passes(view, t, "second"), "firstDisabled", "secondDisabled"),
								cases("shared class",
										t -> (! is(t, "firstDisabled")) || (! is(t, "secondDisabled")),
										t -> passes(view, t, "Shared"),
										"firstDisabled", "secondDisabled"))
							.flatMap(Function.identity()));
	}

	@Test
	void defaultsOfferOkSaveAndDeleteUnderTheirConditions() {
		ViewImpl view = editView();
		ActionImpl defaults = new ActionImpl();
		defaults.setImplicitName(ImplicitActionName.DEFAULTS);
		defaults.setXmlDisabledCondition("defaultsDisabled");
		defaults.setXmlInvisibleCondition("defaultsHidden");
		view.putAction(defaults);

		Predicate<Map<String, Boolean>> expected = t -> (! is(t, "defaultsDisabled")) && (! is(t, "defaultsHidden"));
		List<Stream<Executable>> streams = new ArrayList<>();
		for (ImplicitActionName name : new ImplicitActionName[] {ImplicitActionName.Save, ImplicitActionName.OK, ImplicitActionName.Delete}) {
			streams.add(cases("defaults " + name, expected, t -> passes(view, t, name), "defaultsDisabled", "defaultsHidden"));
		}
		streams.add(cases("defaults ZoomOut", t -> false, t -> passes(view, t, ImplicitActionName.ZoomOut), "defaultsDisabled", "defaultsHidden"));
		for (String name : new String[] {ImplicitActionName.Save.name(), ImplicitActionName.DEFAULTS.name()}) {
			streams.add(cases("defaults custom name " + name, t -> false, t -> passes(view, t, name), "defaultsDisabled", "defaultsHidden"));
		}
		assertAll(streams.stream().flatMap(Function.identity()));
	}

	@Test
	void viewWithoutActionsOffersNone() {
		ViewImpl view = editView();
		Form form = new Form();
		TextField field = new TextField();
		field.setBinding("text");
		addToForm(form, field);
		view.getContained().add(form);

		assertAll(Stream.of(cases("no actions Save", t -> false, t -> passes(view, t, ImplicitActionName.Save)),
								cases("no actions OK", t -> false, t -> passes(view, t, ImplicitActionName.OK)),
								cases("no actions Delete", t -> false, t -> passes(view, t, ImplicitActionName.Delete)),
								cases("no actions custom", t -> false, t -> passes(view, t, "anything")))
							.flatMap(Function.identity()));
	}

	@Test
	void linkNeedsVisibleLinkAndVisibleActionWhetherEnabledOrNot() {
		ViewImpl view = editView();
		ActionImpl action = customAction("lnk", "LnkClass");
		action.setInActionPanel(Boolean.FALSE);
		action.setXmlDisabledCondition("actionDisabled");
		action.setXmlInvisibleCondition("actionHidden");
		view.putAction(action);
		Form form = new Form();
		form.setXmlDisabledCondition("true"); // a literal; a link ignores whether its container is enabled
		form.setXmlInvisibleCondition("formHidden");
		addToForm(form, link("ghost", null)); // references no action of the view
		Link external = new Link();
		ExternalReference reference = new ExternalReference();
		reference.setHref("https://skyve.org");
		external.setReference(reference);
		addToForm(form, external);
		addToForm(form, link("lnk", "linkHidden"));
		view.getContained().add(form);

		String[] conditions = {"actionDisabled", "actionHidden", "formHidden", "linkHidden"};
		assertAll(Stream.of(cases("link",
									t -> (! is(t, "actionHidden")) && (! is(t, "formHidden")) && (! is(t, "linkHidden")), // actionDisabled ignored
									t -> passes(view, t, "LnkClass"),
									conditions),
								cases("link other", t -> false, t -> passes(view, t, "Other"), conditions))
							.flatMap(Function.identity()));
	}

	@Test
	void gridRowLinkNeedsVisibleGridAndLinkButIgnoresRowActionConditions() {
		ViewImpl formView = editView();
		VBox box = new VBox();
		box.setXmlInvisibleCondition("boxHidden");
		DataGrid grid = new DataGrid();
		grid.setBinding("rows");
		grid.setXmlInvisibleCondition("gridHidden");
		addRowLink(grid.getColumns(), link("rowAct", "linkHidden"));
		box.getContained().add(grid);
		DataRepeater repeater = new DataRepeater();
		repeater.setBinding("items");
		repeater.setXmlInvisibleCondition("repeaterHidden");
		addRowLink(repeater.getColumns(), link("rowAct", "itemLinkHidden"));
		box.getContained().add(repeater);
		formView.getContained().add(box);
		DataGrid others = new DataGrid();
		others.setBinding("others");
		addRowLink(others.getColumns(), link("ghost", null)); // not declared by the row view
		addRowLink(others.getColumns(), link("otherRowAct", null));
		addRowLink(others.getColumns(), link("rowAct", null));
		formView.getContained().add(others);
		ActionImpl formAction = customAction("rowAct", "RowActClass");
		formAction.setInActionPanel(Boolean.FALSE);
		formView.putAction(formAction);
		// A form-level trigger whose condition the row requests' form bean does not know, so evaluating it fails them
		Form form = new Form();
		TextField field = textField("fieldHidden");
		field.getChangedActions().add(server("rowAct"));
		addToForm(form, field);
		formView.getContained().add(form);

		ViewImpl rowView = editView();
		ActionImpl rowAction = customAction("rowAct", "RowActClass");
		rowAction.setXmlDisabledCondition("rowDisabled"); // not applied, as SmartClient shows the link regardless
		rowAction.setXmlInvisibleCondition("rowHidden"); // not applied
		rowView.putAction(rowAction);
		rowView.putAction(customAction("otherRowAct", "OtherRowActClass"));

		String[] formConditions = {"boxHidden", "gridHidden", "repeaterHidden"};
		String[] rowConditions = {"linkHidden", "itemLinkHidden", "rowDisabled", "rowHidden"};
		String[] conditions = {"boxHidden", "gridHidden", "repeaterHidden", "linkHidden", "itemLinkHidden", "rowDisabled", "rowHidden", "fieldHidden"};
		Predicate<Map<String, Boolean>> gridExpected = t -> (! is(t, "boxHidden")) && (! is(t, "gridHidden")) && (! is(t, "linkHidden"));
		assertAll(Stream.of(cases("row grid by name differing from its class",
									t -> false,
									t -> passesRow(formView, rowView, t, formConditions, rowConditions, "rows", "rowAct"),
									conditions),
								cases("row grid by class",
										gridExpected,
										t -> passesRow(formView, rowView, t, formConditions, rowConditions, "rows", "RowActClass"),
										conditions),
								cases("row repeater",
										t -> (! is(t, "boxHidden")) && (! is(t, "repeaterHidden")) && (! is(t, "itemLinkHidden")),
										t -> passesRow(formView, rowView, t, formConditions, rowConditions, "items", "RowActClass"),
										conditions),
								cases("row action not linked from the grid",
										t -> false,
										t -> passesRow(formView, rowView, t, formConditions, rowConditions, "rows", "OtherRowActClass"),
										conditions),
								cases("row other grid",
										t -> true,
										t -> passesRow(formView, rowView, t, formConditions, rowConditions, "others", "RowActClass"),
										conditions),
								cases("row unknown grid",
										t -> false,
										t -> passesRow(formView, rowView, t, formConditions, rowConditions, "unknown", "RowActClass"),
										conditions),
								cases("row action as form trigger, only through the field",
										t -> ! is(t, "fieldHidden"),
										t -> passes(formView, subset(t, "boxHidden", "gridHidden", "repeaterHidden", "fieldHidden"), "RowActClass"),
										conditions))
							.flatMap(Function.identity()));
	}

	@Test
	void gridRowLinkInBoundComponentNeedsPrefixedBinding() throws Exception {
		DocumentImpl childDocument = mock(DocumentImpl.class);
		when(childDocument.getOwningModuleName()).thenReturn(MODULE_NAME);
		when(childDocument.getName()).thenReturn("C");
		doReturn(Bean.class).when(childDocument).getBeanClass(customer);
		when(module.getDocument(customer, "C")).thenReturn(childDocument);
		Association association = mock(Association.class);
		when(association.getDocumentName()).thenReturn("C");
		when(document.getAttribute("child")).thenReturn(association);

		ViewImpl fragmentView = new ViewImpl();
		fragmentView.setName("fragment");
		DataGrid grid = new DataGrid();
		grid.setBinding("rows");
		grid.setXmlInvisibleCondition("gridHidden");
		addRowLink(grid.getColumns(), link("rowAct", "linkHidden"));
		fragmentView.getContained().add(grid);
		when(childDocument.getView(UXUI, customer, "fragment")).thenReturn(fragmentView);

		ViewImpl formView = editView();
		Component component = new Component();
		component.setBinding("child");
		component.setName("fragment");
		component.link(UXUI, customer, module, document, ViewType.edit.toString());
		formView.getContained().add(component);
		ViewImpl rowView = editView();
		rowView.putAction(customAction("rowAct", "RowActClass"));

		String[] formConditions = {"gridHidden"};
		String[] rowConditions = {"linkHidden"};
		assertAll(Stream.of(cases("component grid prefixed",
									t -> (! is(t, "gridHidden")) && (! is(t, "linkHidden")),
									t -> passesRow(formView, rowView, t, formConditions, rowConditions, "child.rows", "RowActClass"),
									"gridHidden", "linkHidden"),
								cases("component grid unprefixed",
										t -> false,
										t -> passesRow(formView, rowView, t, formConditions, rowConditions, "rows", "RowActClass"),
										"gridHidden", "linkHidden"))
							.flatMap(Function.identity()));
	}

	@Test
	void builtInSaveThroughButtonNeedsEnabledForm() {
		ViewImpl view = editView();
		ActionImpl save = new ActionImpl();
		save.setImplicitName(ImplicitActionName.Save);
		save.setInActionPanel(Boolean.FALSE);
		save.setXmlDisabledCondition("saveDisabled");
		view.putAction(save);
		Form form = new Form();
		form.setXmlDisabledCondition("formDisabled");
		addToForm(form, button(ImplicitActionName.Save.name()));
		view.getContained().add(form);

		assertAll(cases("Save through button",
							t -> (! is(t, "saveDisabled")) && (! is(t, "formDisabled")),
							t -> passes(view, t, ImplicitActionName.Save),
							"saveDisabled", "formDisabled"));
	}

	@Test
	void buttonInComponentFragmentUsesHostAction() {
		ViewImpl fragmentView = new ViewImpl();
		fragmentView.setName("fragment");
		fragmentView.getContained().add(button("inner"));
		when(document.getView(UXUI, customer, "fragment")).thenReturn(fragmentView);

		ViewImpl view = editView();
		ActionImpl action = customAction("host", "HostClass");
		action.setInActionPanel(Boolean.FALSE);
		action.setXmlDisabledCondition("hostDisabled");
		view.putAction(action);
		VBox box = new VBox();
		box.setXmlInvisibleCondition("boxHidden");
		Component component = new Component();
		component.setName("fragment");
		ComponentTranslation translation = new ComponentTranslation();
		translation.setFromComponent("inner");
		translation.setMappedTo("host");
		component.getActions().add(translation);
		component.link(UXUI, customer, module, document, ViewType.edit.toString());
		box.getContained().add(component);
		view.getContained().add(box);

		Predicate<Map<String, Boolean>> expected = t -> (! is(t, "boxHidden")) && (! is(t, "hostDisabled"));
		assertAll(Stream.of(cases("component by name differing from its class", t -> false, t -> passes(view, t, "host"), "boxHidden", "hostDisabled"),
								cases("component by class", expected, t -> passes(view, t, "HostClass"), "boxHidden", "hostDisabled"))
							.flatMap(Function.identity()));
	}

	@Test
	void inlineRoleConditionsEvaluateForTheUser() {
		ViewImpl view = editView();
		ActionImpl action = customAction("roled", "RoledClass");
		action.setXmlVisibleCondition("{role:m.viewer}"); // invisible when not{role:m.viewer}
		action.setXmlDisabledCondition("{role:m.locked}");
		view.putAction(action);

		AbstractPersistence persistence = mock(AbstractPersistence.class, Mockito.withSettings().defaultAnswer(Mockito.CALLS_REAL_METHODS));
		persistence.setForThread();
		try {
			assertAll(cases("role",
								t -> is(t, "viewer") && (! is(t, "locked")),
								t -> {
									doReturn(Boolean.valueOf(is(t, "viewer"))).when(user).isInRole(MODULE_NAME, "viewer");
									doReturn(Boolean.valueOf(is(t, "locked"))).when(user).isInRole(MODULE_NAME, "locked");
									persistence.setUser(user);
									Bean bean = mock(Bean.class);
									doAnswer(i -> Boolean.valueOf(BindUtil.evaluateCondition(bean, i.getArgument(0)))).when(bean).evaluateCondition(anyString());
									return passes(() -> ActionAvailability.check(user, document, view, UXUI, bean, "RoledClass"));
								},
								"viewer", "locked"));
		}
		finally {
			clearPersistenceThreadLocal();
		}
	}

	@Test
	void refusalThrowsWithLocalisedMessage() {
		ViewImpl view = editView();
		Bean bean = truthTableBean(new TreeMap<>());

		ActionUnavailableException e = assertThrows(ActionUnavailableException.class,
														() -> ActionAvailability.check(user, document, view, UXUI, bean, "missing"));
		assertEquals("This action is no longer available.", e.getMessages().get(0).getText());
	}

	private static ViewImpl editView() {
		ViewImpl result = new ViewImpl();
		result.setName(ViewType.edit.toString());
		return result;
	}

	private static ActionImpl customAction(String name, String className) {
		ActionImpl result = new ActionImpl();
		result.setName(name);
		result.setResourceName(className);
		return result;
	}

	/** Declares an event-only custom action. */
	private static String eventAction(ViewImpl view, String name) {
		ActionImpl action = customAction(name, name + "Class");
		action.setInActionPanel(Boolean.FALSE);
		view.putAction(action);
		return name;
	}

	private static TextField textField(String invisibleCondition) {
		TextField result = new TextField();
		result.setBinding("text");
		result.setXmlInvisibleCondition(invisibleCondition);
		return result;
	}

	private static LookupDescription lookup(String invisibleCondition) {
		LookupDescription result = new LookupDescription();
		result.setBinding("lookup");
		result.setXmlInvisibleCondition(invisibleCondition);
		return result;
	}

	private static DataGrid grid(String invisibleCondition) {
		DataGrid result = new DataGrid();
		result.setBinding("rows");
		result.setXmlInvisibleCondition(invisibleCondition);
		return result;
	}

	private static Button button(String actionName) {
		Button result = new Button();
		result.setActionName(actionName);
		return result;
	}

	private static Link link(String actionName, String invisibleCondition) {
		ActionReference reference = new ActionReference();
		reference.setActionName(actionName);
		Link result = new Link();
		result.setReference(reference);
		result.setXmlInvisibleCondition(invisibleCondition);
		return result;
	}

	private static EventAction server(String actionName) {
		ServerSideActionEventAction result = new ServerSideActionEventAction();
		result.setActionName(actionName);
		return result;
	}

	private static void addToForm(Form form, MetaData widget) {
		FormItem item = new FormItem();
		item.setWidget(widget);
		FormRow row = new FormRow();
		row.getItems().add(item);
		form.getRows().add(row);
	}

	private static void addRowLink(List<DataGridColumn> columns, Link link) {
		DataGridContainerColumn column = new DataGridContainerColumn();
		column.getWidgets().add(link);
		columns.add(column);
	}

	private static boolean is(Map<String, Boolean> table, String condition) {
		return Boolean.TRUE.equals(table.get(condition));
	}

	/**
	 * Evaluates a condition from a truth table the way {@link BindUtil#evaluateCondition(Bean, String)} does:
	 * the literals {@code true} and {@code false}, a condition name, or a condition name with the {@code not} prefix.
	 * A condition missing from the table fails the case, so evaluating against the wrong bean is caught.
	 */
	private static boolean evaluate(Map<String, Boolean> table, String condition) {
		if (String.valueOf(true).equals(condition)) {
			return true;
		}
		if (String.valueOf(false).equals(condition)) {
			return false;
		}
		Boolean value = table.get(condition);
		if (value != null) {
			return value.booleanValue();
		}
		if (condition.startsWith("not")) {
			value = table.get(Introspector.decapitalize(condition.substring(3)));
			if (value != null) {
				return ! value.booleanValue();
			}
		}
		throw new IllegalArgumentException("Condition " + condition + " is not in " + table);
	}

	private static Bean truthTableBean(Map<String, Boolean> table) {
		Bean result = mock(Bean.class);
		doAnswer(i -> Boolean.valueOf(evaluate(table, i.getArgument(0)))).when(result).evaluateCondition(anyString());
		when(result.getBizId()).thenReturn("bizId");
		return result;
	}

	private static Map<String, Boolean> subset(Map<String, Boolean> table, String... conditions) {
		Map<String, Boolean> result = new TreeMap<>();
		for (String condition : conditions) {
			result.put(condition, table.get(condition));
		}
		return result;
	}

	/** Returns every truth assignment of the conditions; one empty assignment when there are none. */
	private static List<Map<String, Boolean>> assignments(String... conditions) {
		int count = 1 << conditions.length;
		List<Map<String, Boolean>> result = new ArrayList<>(count);
		for (int mask = 0; mask < count; mask++) {
			Map<String, Boolean> table = new TreeMap<>();
			for (int i = 0; i < conditions.length; i++) {
				table.put(conditions[i], Boolean.valueOf((mask & (1 << i)) != 0));
			}
			result.add(table);
		}
		return result;
	}

	/** Returns one executable per truth assignment, asserting the check's outcome equals the expected formula. */
	private static Stream<Executable> cases(String caseName,
												Predicate<Map<String, Boolean>> expected,
												Predicate<Map<String, Boolean>> available,
												String... conditions) {
		return assignments(conditions).stream()
										.map(t -> () -> assertEquals(Boolean.valueOf(expected.test(t)),
																		Boolean.valueOf(available.test(t)),
																		caseName + " with " + t));
	}

	private static boolean passes(Executable check) {
		try {
			check.execute();
			return true;
		}
		catch (@SuppressWarnings("unused") ActionUnavailableException e) {
			return false;
		}
		catch (Throwable t) {
			throw new AssertionError(t);
		}
	}

	private boolean passes(ViewImpl view, Map<String, Boolean> table, String actionName) {
		return passes(() -> ActionAvailability.check(user, document, view, UXUI, truthTableBean(table), actionName));
	}

	private boolean passes(ViewImpl view, Map<String, Boolean> table, ImplicitActionName implicitName) {
		return passes(() -> ActionAvailability.check(user, document, view, UXUI, truthTableBean(table), implicitName));
	}

	/** Checks a row link with the form conditions on the form bean and the row conditions on the row bean only. */
	private boolean passesRow(ViewImpl formView,
								ViewImpl rowView,
								Map<String, Boolean> table,
								String[] formConditions,
								String[] rowConditions,
								String gridBinding,
								String actionName) {
		Bean formBean = truthTableBean(subset(table, formConditions));
		Bean rowBean = truthTableBean(subset(table, rowConditions));
		return passes(() -> ActionAvailability.checkRow(user, document, formView, UXUI, formBean, gridBinding, rowDocument, rowView, rowBean, actionName));
	}

	private static void clearPersistenceThreadLocal() {
		try {
			Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
			field.setAccessible(true);
			@SuppressWarnings("unchecked")
			ThreadLocal<AbstractPersistence> threadLocal = (ThreadLocal<AbstractPersistence>) field.get(null);
			threadLocal.remove();
		}
		catch (ReflectiveOperationException e) {
			throw new IllegalStateException(e);
		}
	}
}
