package org.skyve.impl.generate.sail;

import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.argThat;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.skyve.domain.Bean;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.model.document.CollectionImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.module.ModuleImpl;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.impl.metadata.view.ActionImpl;
import org.skyve.impl.metadata.view.ViewImpl;
import org.skyve.impl.metadata.view.container.HBox;
import org.skyve.impl.metadata.view.container.Sidebar;
import org.skyve.impl.metadata.view.container.Tab;
import org.skyve.impl.metadata.view.container.TabPane;
import org.skyve.impl.metadata.view.container.VBox;
import org.skyve.impl.metadata.view.container.form.Form;
import org.skyve.impl.metadata.view.widget.Button;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGrid;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.Persistent;
import org.skyve.metadata.repository.ProvidedRepository;
import org.skyve.metadata.sail.language.Step;
import org.skyve.metadata.sail.language.step.Comment;
import org.skyve.metadata.sail.language.step.interaction.TabSelect;
import org.skyve.metadata.sail.language.step.interaction.TestDataEnter;
import org.skyve.metadata.sail.language.step.interaction.actions.Action;
import org.skyve.metadata.sail.language.step.interaction.actions.ZoomOut;
import org.skyve.metadata.sail.language.step.interaction.grids.DataGridNew;
import org.skyve.metadata.user.User;

@SuppressWarnings("static-method")
class GenerateViewVisitorTest {
	private static final String LINE = "Line";

	private final ProvidedRepository originalRepository = ProvidedRepositoryFactory.get();

	@AfterEach
	void tearDown() {
		if (originalRepository == null) {
			ProvidedRepositoryFactory.clear();
		}
		else {
			ProvidedRepositoryFactory.set(originalRepository);
		}
	}

	@Test
	void constructorStartsPopulateStepsWithTestDataEnter() {
		GenerateViewVisitor visitor = visitor();

		assertEquals(1, visitor.getPopulateSteps().size());
		assertThat(visitor.getPopulateSteps().get(0), instanceOf(TestDataEnter.class));
		assertTrue(visitor.getActionSteps().isEmpty());
	}

	@Test
	void visitTabAddsTabSelectToPopulateAndActionSteps() {
		GenerateViewVisitor visitor = visitor();
		Tab tab = new Tab();
		tab.setTitle("Details");

		visitor.visitTab(tab, true, true);

		assertEquals(2, visitor.getPopulateSteps().size());
		assertThat(visitor.getPopulateSteps().get(1), instanceOf(TabSelect.class));
		assertThat(((TabSelect) visitor.getPopulateSteps().get(1)).getTabPath(), is("Details"));
		assertEquals(1, visitor.getActionSteps().size());
		assertThat(((TabSelect) visitor.getActionSteps().get(0)).getTabPath(), is("Details"));
	}

	@Test
	void visitTabThrowsForATabWithABlankTitle() {
		GenerateViewVisitor visitor = visitor();
		Tab tab = new Tab();
		tab.setTitle(" ");

		assertThrows(MetaDataException.class, () -> visitor.visitTab(tab, true, true));
		assertEquals(1, visitor.getPopulateSteps().size());
		assertTrue(visitor.getActionSteps().isEmpty());
	}

	@Test
	void visitDataGridSkipsNonAddableGrids() {
		GenerateViewVisitor visitor = visitor();
		DataGrid readOnly = new DataGrid();
		readOnly.setBinding("lines");
		readOnly.setEditable(Boolean.FALSE);
		DataGrid addHidden = new DataGrid();
		addHidden.setBinding("lines");
		addHidden.setShowAdd(Boolean.FALSE);

		visitor.visitDataGrid(readOnly, true, true);
		visitor.visitDataGrid(addHidden, true, true);

		assertEquals(1, visitor.getPopulateSteps().size());
		assertTrue(visitor.getActionSteps().isEmpty());
	}

	@Test
	void visitDataGridAddsRowAndNestedPopulateStepsForCollectionBinding() {
		CustomerImpl customer = new CustomerImpl();
		ModuleImpl module = new ModuleImpl();
		module.setName("sales");
		DocumentImpl order = document(null);
		DocumentImpl line = document(null);
		line.setName("Line");
		ViewImpl orderView = view();
		ViewImpl lineView = view(); // document() gives the order its "lines" collection of Lines
		ProvidedRepository repository = mock(ProvidedRepository.class);
		when(repository.getView("desktop", customer, order, "create")).thenReturn(orderView);
		when(repository.getView("desktop", customer, order, "edit")).thenReturn(orderView);
		when(repository.getModule(customer, "sales")).thenReturn(module);
		when(repository.getDocument(customer, module, "Line")).thenReturn(line);
		when(repository.getView("desktop", customer, line, "create")).thenReturn(lineView);
		when(repository.getView("desktop", customer, line, "edit")).thenReturn(lineView);
		ProvidedRepositoryFactory.set(repository);
		GenerateViewVisitor visitor = new GenerateViewVisitor(user(), customer, module, order, "desktop");
		DataGrid grid = new DataGrid();
		grid.setBinding("lines");

		visitor.visitDataGrid(grid, true, true);

		assertEquals(4, visitor.getPopulateSteps().size());
		assertThat(visitor.getPopulateSteps().get(1), instanceOf(DataGridNew.class));
		assertThat(((DataGridNew) visitor.getPopulateSteps().get(1)).getBinding(), is("lines"));
		assertThat(visitor.getPopulateSteps().get(2), instanceOf(TestDataEnter.class));
		assertThat(visitor.getPopulateSteps().get(3), instanceOf(ZoomOut.class));
	}

	@Test
	void visitDataGridThrowsForAnAddableGridWithoutABinding() {
		GenerateViewVisitor visitor = visitor();

		DataGrid grid = new DataGrid();
		MetaDataException e = assertThrows(MetaDataException.class, () -> visitor.visitDataGrid(grid, true, true));
		assertEquals("A data grid in view create of document Order has no binding", e.getMessage());
		assertEquals(1, visitor.getPopulateSteps().size());
	}

	@Test
	void visitButtonThrowsForANonExistentViewAction() {
		GenerateViewVisitor visitor = visitor();
		Button button = new Button();
		button.setActionName("missing");

		MetaDataException e = assertThrows(MetaDataException.class, () -> visitor.visitButton(button, true, true));
		assertEquals("Button action missing does not exist in view create of document Order", e.getMessage());
		assertTrue(visitor.getActionSteps().isEmpty());
	}

	@Test
	void actionFlagsOnlyRequirePersistenceForSaveOkAndDelete() {
		DocumentImpl transientDocument = document(null);
		GenerateViewVisitor transientVisitor = visitor(transientDocument);

		transientVisitor.visitSaveAction(new ActionImpl());
		transientVisitor.visitOKAction(new ActionImpl());
		transientVisitor.visitCancelAction(new ActionImpl());
		transientVisitor.visitDeleteAction(new ActionImpl());

		assertFalse(transientVisitor.getHasSave());
		assertFalse(transientVisitor.getHasOk());
		assertTrue(transientVisitor.getHasCancel());
		assertFalse(transientVisitor.getHasDelete());

		Persistent persistent = new Persistent();
		persistent.setName("orders");
		GenerateViewVisitor persistentVisitor = visitor(document(persistent));

		persistentVisitor.visitSaveAction(new ActionImpl());
		persistentVisitor.visitOKAction(new ActionImpl());
		persistentVisitor.visitDeleteAction(new ActionImpl());

		assertTrue(persistentVisitor.getHasSave());
		assertTrue(persistentVisitor.getHasOk());
		assertTrue(persistentVisitor.getHasDelete());
	}

	@Test
	void customActionsRespectActionPanelAndConfirmation() {
		GenerateViewVisitor visitor = visitor();
		ActionImpl hidden = action("hidden", Boolean.FALSE, null);
		ActionImpl confirmed = action("approve", null, "Continue?");

		visitor.visitCustomAction(hidden);
		visitor.visitCustomAction(confirmed);

		assertEquals(1, visitor.getActionSteps().size());
		Step step = visitor.getActionSteps().get(0);
		assertThat(step, instanceOf(Action.class));
		Action action = (Action) step;
		assertThat(action.getActionName(), is("approve"));
		assertThat(action.getConfirm(), is(Boolean.TRUE));
	}

	@Test
	void visitButtonAddsReferencedViewAction() {
		ViewImpl view = view();
		ActionImpl action = action("approve", null, null);
		view.putAction(action);
		GenerateViewVisitor visitor = visitor(document(null), view);
		Button button = new Button();
		button.setActionName("approve");

		visitor.visitButton(button, true, true);

		assertEquals(1, visitor.getActionSteps().size());
		assertThat(((Action) visitor.getActionSteps().get(0)).getActionName(), is("approve"));
	}

	/**
	 * A button's action is generated only when the action and every container of the button can be used: a hidden or
	 * disabled action, or a button in a hidden or disabled container, is left out; once the walk leaves the
	 * container, a button is generated again.
	 */
	@Test
	void buttonsAreGeneratedOnlyWhenTheActionAndTheirContainersCanBeUsed() {
		ViewImpl view = view();
		ActionImpl shown = action("shown", Boolean.FALSE, null);
		ActionImpl hidden = action("hidden", Boolean.FALSE, null);
		hidden.setXmlInvisibleCondition("true");
		ActionImpl disabled = action("disabled", Boolean.FALSE, null);
		disabled.setXmlDisabledCondition("true");
		ActionImpl enabled = action("enabled", Boolean.FALSE, null);
		enabled.setXmlEnabledCondition("true");
		view.putAction(shown);
		view.putAction(hidden);
		view.putAction(disabled);
		view.putAction(enabled);
		GenerateViewVisitor visitor = visitor(document(null), view);
		Form hiddenForm = new Form();
		hiddenForm.setXmlVisibleCondition("false");
		Form disabledForm = new Form();
		disabledForm.setXmlDisabledCondition("true");
		VBox vbox = new VBox();
		vbox.setXmlInvisibleCondition("true");
		HBox hbox = new HBox();
		hbox.setXmlInvisibleCondition("true");
		TabPane tabPane = new TabPane();
		tabPane.setXmlDisabledCondition("true");
		Sidebar sidebar = new Sidebar();
		sidebar.setXmlInvisibleCondition("true");

		visitor.visitButton(button("shown"), true, true);
		visitor.visitButton(button("hidden"), true, true);
		visitor.visitButton(button("disabled"), true, true);
		visitor.visitButton(button("enabled"), true, true);
		visitor.visitForm(hiddenForm, true, true);
		visitor.visitButton(button("shown"), true, true);
		visitor.visitedForm(hiddenForm, true, true);
		visitor.visitForm(disabledForm, true, true);
		visitor.visitButton(button("shown"), true, true);
		visitor.visitedForm(disabledForm, true, true);
		visitor.visitVBox(vbox, true, true);
		visitor.visitButton(button("shown"), true, true);
		visitor.visitedVBox(vbox, true, true);
		visitor.visitHBox(hbox, true, true);
		visitor.visitButton(button("shown"), true, true);
		visitor.visitedHBox(hbox, true, true);
		visitor.visitTabPane(tabPane, true, true);
		visitor.visitButton(button("shown"), true, true);
		visitor.visitedTabPane(tabPane, true, true);
		visitor.visitSidebar(sidebar, true, true);
		visitor.visitButton(button("shown"), true, true);
		visitor.visitedSidebar(sidebar, true, true);
		visitor.visitButton(button("shown"), true, true);

		assertEquals(List.of("shown", "enabled", "shown"), actionNames(visitor.getActionSteps()));
		assertEquals(List.of(), comments(visitor.getActionSteps()));
	}

	/**
	 * A hidden tab is not selected, and neither are the buttons on it; a disabled tab cannot be selected either.
	 */
	@Test
	void aHiddenOrDisabledTabIsNotSelectedNorAreItsButtons() {
		ViewImpl view = view();
		view.putAction(action("approve", Boolean.FALSE, null));
		GenerateViewVisitor visitor = visitor(document(null), view);
		Tab hidden = tab("Hidden");
		hidden.setXmlInvisibleCondition("true");
		Tab disabled = tab("Disabled");
		disabled.setXmlEnabledCondition("false");

		visitor.visitTab(hidden, true, true);
		visitor.visitButton(button("approve"), true, true);
		visitor.visitedTab(hidden, true, true);
		visitor.visitTab(disabled, true, true);
		visitor.visitedTab(disabled, true, true);

		assertEquals(1, visitor.getPopulateSteps().size());
		assertTrue(visitor.getActionSteps().isEmpty());
	}

	/**
	 * A condition that changes as the script enters data and saves, such as {@code persisted}, could hide a step, so
	 * the step is left out and a comment names the condition, for a tab, the buttons on it, a toolbar action, a new
	 * data grid row, a Save and a Delete.
	 */
	@Test
	void aStepThatAConditionOnTheScriptsStateCouldHideIsACommentNamingTheCondition() {
		ViewImpl view = view();
		view.putAction(action("approve", Boolean.FALSE, null));
		ActionImpl toolbar = action("archive", null, null);
		toolbar.setXmlVisibleCondition("changed");
		Persistent persistent = new Persistent();
		persistent.setName("orders");
		GenerateViewVisitor visitor = visitor(document(persistent), view);
		Tab tab = tab("Subscriptions");
		tab.setXmlVisibleCondition("persisted");
		DataGrid grid = new DataGrid();
		grid.setBinding("lines");
		grid.setXmlEnableAddCondition("persisted");
		ActionImpl save = new ActionImpl();
		save.setXmlVisibleCondition("notPersisted");
		ActionImpl delete = new ActionImpl();
		delete.setXmlDisabledCondition("notChanged");

		visitor.visitTab(tab, true, true);
		visitor.visitButton(button("approve"), true, true);
		visitor.visitedTab(tab, true, true);
		visitor.visitCustomAction(toolbar);
		visitor.visitDataGrid(grid, true, true);
		visitor.visitSaveAction(save);
		visitor.visitDeleteAction(delete);

		assertAll(() -> assertEquals(List.of("not generated: tab Subscriptions depends on visible=\"persisted\", which depends on what the script enters and saves",
												"not generated: a new row in data grid lines depends on enableAdd=\"persisted\", which depends on what the script enters and saves",
												"not generated: save depends on visible=\"notPersisted\", which depends on what the script enters and saves"),
										comments(visitor.getPopulateSteps())),
					() -> assertEquals(List.of("not generated: tab Subscriptions depends on visible=\"persisted\", which depends on what the script enters and saves",
												"not generated: action approve depends on visible=\"persisted\", which depends on what the script enters and saves",
												"not generated: action archive depends on visible=\"changed\", which depends on what the script enters and saves"),
										comments(visitor.getActionSteps())),
					// only a generator that deletes takes the Delete's comment
					() -> assertEquals(List.of("not generated: delete depends on disabled=\"notChanged\", which depends on what the script enters and saves"),
										comments(visitor.getDeleteComments())),
					() -> assertEquals(List.of(), actionNames(visitor.getActionSteps())),
					() -> assertFalse(visitor.getHasSave()),
					() -> assertFalse(visitor.getHasDelete()));
	}

	/**
	 * A condition that cannot be evaluated (here: the document cannot make the instances it is evaluated against) is a
	 * comment naming the condition and why, and is evaluated only once.
	 */
	@Test
	void aConditionThatCannotBeEvaluatedIsACommentGivingTheReason() throws Exception {
		ViewImpl view = view();
		ActionImpl action = action("approve", Boolean.FALSE, null);
		action.setXmlVisibleCondition("approvable");
		view.putAction(action);
		DocumentImpl document = spy(document(null));
		doThrow(new IllegalStateException("no instances")).when(document).newInstance(any(User.class));
		GenerateViewVisitor visitor = visitor(document, view);

		visitor.visitButton(button("approve"), true, true);
		visitor.visitButton(button("approve"), true, true);

		String comment = "not generated: action approve depends on visible=\"approvable\", which cannot be evaluated here (no instances)";
		assertEquals(List.of(comment, comment), comments(visitor.getActionSteps()));
		verify(document, times(1)).newInstance(any(User.class));
	}

	/**
	 * Privileges decide as the renderers do: a custom action or a Download the user cannot execute, a Save when the
	 * user can neither create nor update and a Delete when the user cannot delete are left out silently, as the user's
	 * view has none of them; other implicit actions are not decided by privileges.
	 */
	@Test
	void actionsTheUserIsNotPermittedAreNotGenerated() {
		ViewImpl view = view();
		view.putAction(action("approve", Boolean.FALSE, null));
		ActionImpl download = action("Export", Boolean.FALSE, null);
		download.setImplicitName(ImplicitActionName.Download);
		view.putAction(download);
		ActionImpl zoomOut = action("ZoomOut", Boolean.FALSE, null);
		zoomOut.setImplicitName(ImplicitActionName.ZoomOut);
		view.putAction(zoomOut);
		Persistent persistent = new Persistent();
		persistent.setName("orders");
		DocumentImpl document = document(persistent);
		GenerateViewVisitor visitor = visitor(document, view, mock(User.class));

		visitor.visitButton(button("approve"), true, true);
		visitor.visitButton(button("Export"), true, true);
		visitor.visitButton(button("ZoomOut"), true, true);
		visitor.visitSaveAction(new ActionImpl());
		visitor.visitDeleteAction(new ActionImpl());

		assertAll(() -> assertEquals(List.of("ZoomOut"), actionNames(visitor.getActionSteps())),
					() -> assertEquals(List.of(), comments(visitor.getActionSteps())),
					() -> assertFalse(visitor.getHasSave()),
					() -> assertFalse(visitor.getHasDelete()));
	}

	/**
	 * A Save or Delete that a known condition hides is left out silently; one that nothing hides is generated.
	 */
	@Test
	void aSaveOrDeleteThatAKnownConditionHidesIsNotGenerated() {
		Persistent persistent = new Persistent();
		persistent.setName("orders");
		GenerateViewVisitor hiding = visitor(document(persistent));
		ActionImpl hiddenSave = new ActionImpl();
		hiddenSave.setXmlInvisibleCondition("true");
		ActionImpl disabledDelete = new ActionImpl();
		disabledDelete.setXmlDisabledCondition("true");

		hiding.visitSaveAction(hiddenSave);
		hiding.visitDeleteAction(disabledDelete);

		assertAll(() -> assertFalse(hiding.getHasSave()),
					() -> assertFalse(hiding.getHasDelete()),
					() -> assertEquals(1, hiding.getPopulateSteps().size()),
					() -> assertTrue(hiding.getActionSteps().isEmpty()));
	}

	/**
	 * A data grid that is hidden, disabled or cannot add, or is in a hidden container, gets no new row.
	 */
	@Test
	void aDataGridThatCannotAddGetsNoNewRow() {
		GenerateViewVisitor visitor = visitor();
		DataGrid hidden = new DataGrid();
		hidden.setBinding("lines");
		hidden.setXmlVisibleCondition("false");
		DataGrid disabled = new DataGrid();
		disabled.setBinding("lines");
		disabled.setXmlDisabledCondition("true");
		DataGrid cannotAdd = new DataGrid();
		cannotAdd.setBinding("lines");
		cannotAdd.setXmlDisableAddCondition("true");
		DataGrid inHiddenForm = new DataGrid();
		inHiddenForm.setBinding("lines");
		Form form = new Form();
		form.setXmlInvisibleCondition("true");

		visitor.visitDataGrid(hidden, true, true);
		visitor.visitDataGrid(disabled, true, true);
		visitor.visitDataGrid(cannotAdd, true, true);
		visitor.visitForm(form, true, true);
		visitor.visitDataGrid(inHiddenForm, true, true);
		visitor.visitedForm(form, true, true);

		assertEquals(1, visitor.getPopulateSteps().size());
	}

	/**
	 * The renderers show a data grid's New only to a user who can create the grid's document, so no row is added for
	 * one who cannot, and nothing says so.
	 */
	@Test
	void aDataGridWhoseDocumentTheUserCannotCreateGetsNoNewRow() {
		User user = user();
		when(Boolean.valueOf(user.canCreateDocument(argThat(d -> LINE.equals(d.getName()))))).thenReturn(Boolean.FALSE);
		GenerateViewVisitor visitor = visitor(document(null), view(), user);
		DataGrid grid = new DataGrid();
		grid.setBinding("lines");

		visitor.visitDataGrid(grid, true, true);

		assertEquals(1, visitor.getPopulateSteps().size());
	}

	/**
	 * A button bound to a built-in action gets the renderers' privilege checks: a Delete button needs delete, a Save
	 * button create or update; the same buttons are generated for a user who has them.
	 */
	@Test
	void aButtonForABuiltInActionNeedsItsPrivilege() {
		ViewImpl view = view();
		ActionImpl delete = action("Delete", Boolean.FALSE, null);
		delete.setImplicitName(ImplicitActionName.Delete);
		view.putAction(delete);
		ActionImpl save = action("Save", Boolean.FALSE, null);
		save.setImplicitName(ImplicitActionName.Save);
		view.putAction(save);
		User readOnly = mock(User.class);
		when(Boolean.valueOf(readOnly.canReadDocument(any(Document.class)))).thenReturn(Boolean.TRUE);
		GenerateViewVisitor refused = visitor(document(null), view, readOnly);
		GenerateViewVisitor permitted = visitor(document(null), view, user());

		for (GenerateViewVisitor visitor : List.of(refused, permitted)) {
			visitor.visitButton(button("Delete"), true, true);
			visitor.visitButton(button("Save"), true, true);
		}

		assertAll(() -> assertEquals(List.of(), actionNames(refused.getActionSteps())),
					() -> assertEquals(List.of("Delete", "Save"), actionNames(permitted.getActionSteps())));
	}

	/**
	 * A toolbar Save or Delete that is not in the action panel is not on the toolbar, so it is not generated.
	 */
	@Test
	void aSaveOrDeleteOutOfTheActionPanelIsNotGenerated() {
		Persistent persistent = new Persistent();
		persistent.setName("orders");
		GenerateViewVisitor visitor = visitor(document(persistent));
		ActionImpl save = new ActionImpl();
		save.setInActionPanel(Boolean.FALSE);
		ActionImpl delete = new ActionImpl();
		delete.setInActionPanel(Boolean.FALSE);

		visitor.visitSaveAction(save);
		visitor.visitDeleteAction(delete);

		assertAll(() -> assertFalse(visitor.getHasSave()),
					() -> assertFalse(visitor.getHasDelete()));
	}

	/**
	 * Conditions are evaluated only when a step needs deciding: a container whose contents generate nothing creates
	 * no instances. Once creating them failed, it is not tried again, and a failure without a message gives a fixed
	 * reason.
	 */
	@Test
	void conditionsAreEvaluatedOnlyForStepsAndAFailureIsNotRetried() throws Exception {
		ViewImpl view = view();
		view.putAction(action("approve", Boolean.FALSE, null));
		DocumentImpl document = spy(document(null));
		doThrow(new IllegalStateException()).when(document).newInstance(any(User.class));
		GenerateViewVisitor visitor = visitor(document, view);
		Form empty = new Form();
		empty.setXmlVisibleCondition("approvable");
		Form holding = new Form();
		holding.setXmlVisibleCondition("approving");
		Form nested = new Form();
		nested.setXmlEnabledCondition("open");

		visitor.visitForm(empty, true, true);
		visitor.visitedForm(empty, true, true);
		verify(document, never()).newInstance(any(User.class));
		visitor.visitForm(holding, true, true);
		visitor.visitForm(nested, true, true);
		visitor.visitButton(button("approve"), true, true);
		visitor.visitedForm(nested, true, true);
		visitor.visitedForm(holding, true, true);

		assertAll(() -> assertEquals(List.of("not generated: action approve depends on visible=\"approving\", " +
												"which cannot be evaluated here (IllegalStateException without a message)"),
										comments(visitor.getActionSteps())),
					() -> verify(document, times(1)).newInstance(any(User.class)));
	}

	/**
	 * The two-instance rule (M17): a condition false on both test instances lets the step be generated; one true on
	 * both, or one the instances disagree on, leaves the step out with a comment saying which; only a constant condition
	 * leaves a step out without a word.
	 */
	@Test
	void nonConstantConditionsLeaveStepsOutWithAComment() throws Exception {
		ViewImpl view = view();
		view.putAction(conditioned("agreeShown", "hiddenOnNeither"));
		view.putAction(conditioned("agreeHidden", "hiddenOnBoth"));
		view.putAction(conditioned("disagree", "hiddenWhenOpened"));
		ActionImpl constant = action("constant", Boolean.FALSE, null);
		constant.setXmlVisibleCondition("false");
		view.putAction(constant);
		Bean opened = mock(Bean.class);
		when(Boolean.valueOf(opened.evaluateCondition("hiddenOnBoth"))).thenReturn(Boolean.TRUE);
		when(Boolean.valueOf(opened.evaluateCondition("hiddenWhenOpened"))).thenReturn(Boolean.TRUE);
		Bean entered = mock(Bean.class);
		when(Boolean.valueOf(entered.evaluateCondition("hiddenOnBoth"))).thenReturn(Boolean.TRUE);
		DocumentImpl document = spy(document(null));
		doReturn(opened).when(document).newInstance(any(User.class));
		GenerateViewVisitor visitor = visitor(document, view, user(), entered);

		for (String action : List.of("agreeShown", "agreeHidden", "disagree", "constant")) {
			visitor.visitButton(button(action), true, true);
		}

		assertAll(() -> assertEquals(List.of("agreeShown"), actionNames(visitor.getActionSteps())),
					() -> assertEquals(List.of("not generated: action agreeHidden depends on invisible=\"hiddenOnBoth\", " +
													"which hides or disables it on both test instances",
												"not generated: action disagree depends on invisible=\"hiddenWhenOpened\", " +
													"which depends on what the script enters"),
										comments(visitor.getActionSteps())));
	}

	private static ActionImpl conditioned(String name, String invisibleCondition) {
		ActionImpl result = action(name, Boolean.FALSE, null);
		result.setXmlInvisibleCondition(invisibleCondition);
		return result;
	}

	private static Button button(String actionName) {
		Button result = new Button();
		result.setActionName(actionName);
		return result;
	}

	private static Tab tab(String title) {
		Tab result = new Tab();
		result.setTitle(title);
		return result;
	}

	private static GenerateViewVisitor visitor() {
		return visitor(document(null));
	}

	private static GenerateViewVisitor visitor(DocumentImpl document) {
		return visitor(document, view());
	}

	private static GenerateViewVisitor visitor(DocumentImpl document, ViewImpl view) {
		return visitor(document, view, user());
	}

	private static GenerateViewVisitor visitor(DocumentImpl document, ViewImpl view, User user) {
		return visitor(document, view, user, null);
	}

	/**
	 * Returns a visitor whose test data instance is {@code entered}, or a built one when it is {@code null}.
	 */
	private static GenerateViewVisitor visitor(DocumentImpl document, ViewImpl view, User user, Bean entered) {
		CustomerImpl customer = new CustomerImpl();
		ModuleImpl module = new ModuleImpl();
		ProvidedRepository repository = mock(ProvidedRepository.class);
		module.setName("sales");
		when(repository.getView("desktop", customer, document, "create")).thenReturn(view);
		when(repository.getView("desktop", customer, document, "edit")).thenReturn(view);
		// the documents' "lines" collection holds Lines, for the data grid tests
		DocumentImpl line = new DocumentImpl();
		line.setName(LINE);
		line.setOwningModuleName("sales");
		when(repository.getModule(customer, "sales")).thenReturn(module);
		when(repository.getDocument(customer, module, LINE)).thenReturn(line);
		when(repository.getView("desktop", customer, line, "create")).thenReturn(view());
		when(repository.getView("desktop", customer, line, "edit")).thenReturn(view());
		ProvidedRepositoryFactory.set(repository);
		if (entered == null) {
			return new GenerateViewVisitor(user, customer, module, document, "desktop");
		}
		return new GenerateViewVisitor(user, customer, module, document, "desktop") {
			@Override
			Bean newTestDataInstance() {
				return entered;
			}
		};
	}

	/**
	 * Returns a user permitted everything, as the tests that are not about privileges need.
	 */
	private static User user() {
		User result = mock(User.class);
		when(Boolean.valueOf(result.canExecuteAction(any(Document.class), anyString()))).thenReturn(Boolean.TRUE);
		when(Boolean.valueOf(result.canCreateDocument(any(Document.class)))).thenReturn(Boolean.TRUE);
		when(Boolean.valueOf(result.canUpdateDocument(any(Document.class)))).thenReturn(Boolean.TRUE);
		when(Boolean.valueOf(result.canDeleteDocument(any(Document.class)))).thenReturn(Boolean.TRUE);
		when(Boolean.valueOf(result.canReadDocument(any(Document.class)))).thenReturn(Boolean.TRUE);
		return result;
	}

	private static List<String> comments(List<Step> steps) {
		return steps.stream().filter(Comment.class::isInstance).map(step -> ((Comment) step).getComment()).toList();
	}

	private static List<String> actionNames(List<Step> steps) {
		return steps.stream().filter(Action.class::isInstance).map(step -> ((Action) step).getActionName()).toList();
	}

	private static DocumentImpl document(Persistent persistent) {
		DocumentImpl result = new DocumentImpl();
		result.setName("Order");
		result.setOwningModuleName("sales");
		result.setPersistent(persistent);
		CollectionImpl lines = new CollectionImpl();
		lines.setName("lines");
		lines.setDocumentName(LINE);
		result.putRelation(lines);
		return result;
	}

	private static ViewImpl view() {
		ViewImpl result = new ViewImpl();
		result.setName("create");
		return result;
	}

	private static ActionImpl action(String name, Boolean inActionPanel, String confirmation) {
		ActionImpl result = new ActionImpl();
		result.setName(name);
		result.setResourceName(name);
		result.setInActionPanel(inActionPanel);
		result.setConfirmationText(confirmation);
		return result;
	}
}
