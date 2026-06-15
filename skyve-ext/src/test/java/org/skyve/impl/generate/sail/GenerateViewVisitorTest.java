package org.skyve.impl.generate.sail;

import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.model.document.CollectionImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.module.ModuleImpl;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.impl.metadata.view.ActionImpl;
import org.skyve.impl.metadata.view.ViewImpl;
import org.skyve.impl.metadata.view.container.Tab;
import org.skyve.impl.metadata.view.widget.Button;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGrid;
import org.skyve.metadata.model.Persistent;
import org.skyve.metadata.repository.ProvidedRepository;
import org.skyve.metadata.sail.language.Step;
import org.skyve.metadata.sail.language.step.interaction.TabSelect;
import org.skyve.metadata.sail.language.step.interaction.TestDataEnter;
import org.skyve.metadata.sail.language.step.interaction.actions.Action;
import org.skyve.metadata.sail.language.step.interaction.actions.ZoomOut;
import org.skyve.metadata.sail.language.step.interaction.grids.DataGridNew;

@SuppressWarnings("static-method")
class GenerateViewVisitorTest {
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
		ViewImpl lineView = view();
		CollectionImpl lines = new CollectionImpl();
		lines.setName("lines");
		lines.setDocumentName("Line");
		order.putRelation(lines);
		ProvidedRepository repository = mock(ProvidedRepository.class);
		when(repository.getView("desktop", customer, order, "create")).thenReturn(orderView);
		when(repository.getView("desktop", customer, order, "edit")).thenReturn(orderView);
		when(repository.getModule(customer, "sales")).thenReturn(module);
		when(repository.getDocument(customer, module, "Line")).thenReturn(line);
		when(repository.getView("desktop", customer, line, "create")).thenReturn(lineView);
		when(repository.getView("desktop", customer, line, "edit")).thenReturn(lineView);
		ProvidedRepositoryFactory.set(repository);
		GenerateViewVisitor visitor = new GenerateViewVisitor(customer, module, order, "desktop");
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

	private static GenerateViewVisitor visitor() {
		return visitor(document(null));
	}

	private static GenerateViewVisitor visitor(DocumentImpl document) {
		return visitor(document, view());
	}

	private static GenerateViewVisitor visitor(DocumentImpl document, ViewImpl view) {
		CustomerImpl customer = new CustomerImpl();
		ModuleImpl module = new ModuleImpl();
		ProvidedRepository repository = mock(ProvidedRepository.class);
		module.setName("sales");
		when(repository.getView("desktop", customer, document, "create")).thenReturn(view);
		when(repository.getView("desktop", customer, document, "edit")).thenReturn(view);
		ProvidedRepositoryFactory.set(repository);
		return new GenerateViewVisitor(customer, module, document, "desktop");
	}

	private static DocumentImpl document(Persistent persistent) {
		DocumentImpl result = new DocumentImpl();
		result.setName("Order");
		result.setOwningModuleName("sales");
		result.setPersistent(persistent);
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
		result.setInActionPanel(inActionPanel);
		result.setConfirmationText(confirmation);
		return result;
	}
}
