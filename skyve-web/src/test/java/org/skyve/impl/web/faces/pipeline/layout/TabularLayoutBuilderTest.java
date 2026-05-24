package org.skyve.impl.web.faces.pipeline.layout;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.primefaces.component.column.Column;
import org.primefaces.component.message.Message;
import org.primefaces.component.outputlabel.OutputLabel;
import org.primefaces.component.panel.Panel;
import org.primefaces.component.panelgrid.PanelGrid;
import org.primefaces.component.row.Row;
import org.primefaces.component.toolbar.ToolbarGroup;
import org.skyve.impl.metadata.view.container.HBox;
import org.skyve.impl.metadata.view.container.Sidebar;
import org.skyve.impl.metadata.view.container.VBox;
import org.skyve.impl.metadata.view.container.form.Form;
import org.skyve.impl.metadata.view.container.form.FormColumn;
import org.skyve.impl.metadata.view.container.form.FormItem;
import org.skyve.impl.metadata.view.container.form.FormRow;
import org.skyve.impl.metadata.view.widget.bound.input.ContentSignature;
import org.skyve.impl.web.faces.views.FacesView;

import jakarta.el.ELContext;
import jakarta.el.ExpressionFactory;
import jakarta.el.ValueExpression;
import jakarta.faces.application.Application;
import jakarta.faces.component.UIComponent;
import jakarta.faces.component.html.HtmlPanelGrid;
import jakarta.faces.component.html.HtmlPanelGroup;
import jakarta.faces.context.FacesContext;

class TabularLayoutBuilderTest {

	private abstract static class FacesContextBridge extends FacesContext {
		static void setCurrent(FacesContext facesContext) {
			setCurrentInstance(facesContext);
		}
	}

	private static Application mockApplication;
	private static ExpressionFactory mockExpressionFactory;
	private static int idCounter;

	// Per-test component mocks (configured in @BeforeEach)
	private PanelGrid mockPanelGrid;
	private HtmlPanelGrid mockHtmlPanelGrid;
	private HtmlPanelGroup mockHtmlPanelGroup;
	private ToolbarGroup mockToolbarGroup;
	private Row mockRow;
	private Column mockColumn;
	private OutputLabel mockOutputLabel;
	private Message mockMessage;
	private Panel mockPanel;

	@BeforeAll
	@SuppressWarnings("static-method")
	static void setUpFacesContext() {
		FacesContext facesContext = mock(FacesContext.class);
		mockApplication = mock(Application.class);
		mockExpressionFactory = mock(ExpressionFactory.class);
		ELContext elContext = mock(ELContext.class);
		when(facesContext.getApplication()).thenReturn(mockApplication);
		when(facesContext.getELContext()).thenReturn(elContext);
		when(mockApplication.getExpressionFactory()).thenReturn(mockExpressionFactory);

		ValueExpression mockValueExpr = mock(ValueExpression.class);
		when(mockExpressionFactory.createValueExpression(any(ELContext.class), anyString(), any(Class.class))).thenReturn(mockValueExpr);
		when(mockExpressionFactory.createValueExpression(anyString(), any(Class.class))).thenReturn(mockValueExpr);

		idCounter = 0;
		FacesContextBridge.setCurrent(facesContext);
	}

	@AfterAll
	@SuppressWarnings("static-method")
	static void tearDownFacesContext() {
		FacesContextBridge.setCurrent(null);
	}

	private TabularLayoutBuilder builder;
	private FacesView managedBean;

	@BeforeEach
	void setUp() {
		// Fresh mocks per test — Mockito mocks absorb setValueExpression() calls
		mockPanelGrid = mock(PanelGrid.class);
		mockHtmlPanelGrid = mock(HtmlPanelGrid.class);
		mockHtmlPanelGroup = mock(HtmlPanelGroup.class);
		mockToolbarGroup = mock(ToolbarGroup.class);
		mockRow = mock(Row.class);
		mockColumn = mock(Column.class);
		mockOutputLabel = mock(OutputLabel.class);
		mockMessage = mock(Message.class);
		mockPanel = mock(Panel.class);

		when(mockPanelGrid.getChildren()).thenReturn(new ArrayList<>());
		when(mockHtmlPanelGroup.getChildren()).thenReturn(new ArrayList<>());
		when(mockToolbarGroup.getChildren()).thenReturn(new ArrayList<>());
		when(mockRow.getChildren()).thenReturn(new ArrayList<>());
		when(mockColumn.getChildren()).thenReturn(new ArrayList<>());
		when(mockHtmlPanelGrid.getChildren()).thenReturn(new ArrayList<>());

		when(mockApplication.createComponent(PanelGrid.COMPONENT_TYPE)).thenReturn(mockPanelGrid);
		when(mockApplication.createComponent(HtmlPanelGrid.COMPONENT_TYPE)).thenReturn(mockHtmlPanelGrid);
		when(mockApplication.createComponent(HtmlPanelGroup.COMPONENT_TYPE)).thenReturn(mockHtmlPanelGroup);
		when(mockApplication.createComponent(ToolbarGroup.COMPONENT_TYPE)).thenReturn(mockToolbarGroup);
		when(mockApplication.createComponent(Row.COMPONENT_TYPE)).thenReturn(mockRow);
		when(mockApplication.createComponent(Column.COMPONENT_TYPE)).thenReturn(mockColumn);
		when(mockApplication.createComponent(OutputLabel.COMPONENT_TYPE)).thenReturn(mockOutputLabel);
		when(mockApplication.createComponent(Message.COMPONENT_TYPE)).thenReturn(mockMessage);
		when(mockApplication.createComponent(Panel.COMPONENT_TYPE)).thenReturn(mockPanel);

		builder = new TabularLayoutBuilder();
		managedBean = mock(FacesView.class);
		when(managedBean.nextId()).thenAnswer(inv -> "id" + (++idCounter));
		builder.setSAILManagedBean(managedBean);
	}

	// ── Short-circuit tests (component != null → return as-is) ──────────────

	@Test
	void viewLayoutReturnsExistingComponent() {
		UIComponent existing = mock(UIComponent.class);
		assertSame(existing, builder.viewLayout(existing));
	}

	@Test
	void tabLayoutReturnsExistingComponent() {
		UIComponent existing = mock(UIComponent.class);
		assertSame(existing, builder.tabLayout(existing));
	}

	@Test
	void addTabLayoutReturnsExistingComponent() {
		UIComponent existing = mock(UIComponent.class);
		UIComponent tab = mock(UIComponent.class);
		UIComponent tabLayout = mock(UIComponent.class);
		assertSame(existing, builder.addTabLayout(existing, tab, tabLayout));
	}

	@Test
	void addedTabReturnsExistingComponent() {
		UIComponent existing = mock(UIComponent.class);
		UIComponent tab = mock(UIComponent.class);
		assertSame(existing, builder.addedTab(existing, tab));
	}

	@Test
	void addedBorderLayoutReturnsExistingComponent() {
		UIComponent existing = mock(UIComponent.class);
		UIComponent borderLayout = mock(UIComponent.class);
		assertSame(existing, builder.addedBorderLayout(existing, borderLayout));
	}

	@Test
	void vboxLayoutReturnsExistingComponent() {
		UIComponent existing = mock(UIComponent.class);
		assertSame(existing, builder.vboxLayout(existing, new VBox()));
	}

	@Test
	void hboxLayoutReturnsExistingComponent() {
		UIComponent existing = mock(UIComponent.class);
		assertSame(existing, builder.hboxLayout(existing, new HBox()));
	}

	@Test
	void sidebarLayoutReturnsExistingComponent() {
		UIComponent existing = mock(UIComponent.class);
		Sidebar sidebar = new Sidebar();
		assertSame(existing, builder.sidebarLayout(existing, sidebar, false));
	}

	@Test
	void formLayoutReturnsExistingComponent() {
		UIComponent existing = mock(UIComponent.class);
		assertSame(existing, builder.formLayout(existing, new Form()));
	}

	@Test
	void formRowLayoutReturnsExistingComponent() {
		UIComponent existing = mock(UIComponent.class);
		assertSame(existing, builder.formRowLayout(existing, new FormRow()));
	}

	@Test
	void addFormRowLayoutReturnsExistingComponent() {
		UIComponent existing = mock(UIComponent.class);
		UIComponent formLayout = mock(UIComponent.class);
		UIComponent rowLayout = mock(UIComponent.class);
		assertSame(existing, builder.addFormRowLayout(existing, formLayout, rowLayout));
	}

	@Test
	void addedFormRowLayoutReturnsExistingComponent() {
		UIComponent existing = mock(UIComponent.class);
		UIComponent rowLayout = mock(UIComponent.class);
		assertSame(existing, builder.addedFormRowLayout(existing, rowLayout));
	}

	@Test
	void contentSignatureLayoutReturnsExistingComponent() {
		UIComponent existing = mock(UIComponent.class);
		assertSame(existing, builder.contentSignatureLayout(existing, new ContentSignature()));
	}

	@Test
	void addToContainerReturnsExistingComponent() {
		UIComponent existing = mock(UIComponent.class);
		UIComponent container = mock(UIComponent.class);
		UIComponent child = mock(UIComponent.class);
		assertSame(existing, builder.addToContainer(existing, new HBox(), container, child, null, null, null, null, null, null, null, null));
	}

	@Test
	void addedToContainerReturnsExistingComponent() {
		UIComponent existing = mock(UIComponent.class);
		UIComponent container = mock(UIComponent.class);
		assertSame(existing, builder.addedToContainer(existing, new HBox(), container));
	}

	// ── Creation tests (component == null → create new) ─────────────────────

	@Test
	void viewLayoutCreatesPanelGrid() {
		UIComponent result = builder.viewLayout(null);
		assertSame(mockPanelGrid, result);
		verify(mockPanelGrid).setColumns(1);
	}

	@Test
	void toolbarLayoutsCreatesToolbarGroup() {
		List<UIComponent> result = builder.toolbarLayouts(null);
		assertNotNull(result);
		assertTrue(result.size() == 1);
		assertSame(mockToolbarGroup, result.get(0));
	}

	@Test
	void toolbarLayoutsReturnsExistingList() {
		List<UIComponent> existing = new ArrayList<>();
		assertSame(existing, builder.toolbarLayouts(existing));
	}

	@Test
	void addToolbarLayoutsAddsFacetToToolbar() {
		ToolbarGroup toolbar = mock(ToolbarGroup.class);
		java.util.Map<String, UIComponent> facets = new java.util.HashMap<>();
		when(toolbar.getFacets()).thenReturn(facets);
		List<UIComponent> toolbars = new ArrayList<>();
		toolbars.add(toolbar);
		UIComponent layout = mock(UIComponent.class);
		List<UIComponent> layouts = new ArrayList<>();
		layouts.add(layout);
		builder.addToolbarLayouts(toolbars, layouts);
		assertSame(layout, facets.get("left"));
	}

	@Test
	void addToolbarsOrLayoutsAddsToViewAtIndex0() {
		UIComponent view = mock(UIComponent.class);
		List<UIComponent> viewChildren = new ArrayList<>();
		when(view.getChildren()).thenReturn(viewChildren);
		UIComponent toolbar = mock(UIComponent.class);
		List<UIComponent> toolbarsOrLayouts = new ArrayList<>();
		toolbarsOrLayouts.add(toolbar);
		builder.addToolbarsOrLayouts(view, toolbarsOrLayouts);
		assertSame(toolbar, viewChildren.get(0));
	}

	@Test
	void tabLayoutCreatesPanelGrid() {
		UIComponent result = builder.tabLayout(null);
		assertSame(mockPanelGrid, result);
		verify(mockPanelGrid).setColumns(1);
	}

	@Test
	void addTabLayoutAddsToTabAndReturnsTabLayout() {
		UIComponent tab = mock(UIComponent.class);
		List<UIComponent> tabChildren = new ArrayList<>();
		when(tab.getChildren()).thenReturn(tabChildren);
		UIComponent tabLayout = mock(UIComponent.class);
		UIComponent result = builder.addTabLayout(null, tab, tabLayout);
		assertSame(tabLayout, result);
		assertTrue(tabChildren.contains(tabLayout));
	}

	@Test
	void addTabAddsTabToTabPane() {
		UIComponent tabPane = mock(UIComponent.class);
		List<UIComponent> tabPaneChildren = new ArrayList<>();
		when(tabPane.getChildren()).thenReturn(tabPaneChildren);
		UIComponent tab = mock(UIComponent.class);
		builder.addTab(tabPane, tab);
		assertTrue(tabPaneChildren.contains(tab));
	}

	@Test
	void addedTabReturnsTabPaneWhenNoExistingComponent() {
		UIComponent tabPane = mock(UIComponent.class);
		UIComponent tabLayout = mock(UIComponent.class);
		when(tabLayout.getParent()).thenReturn(tabPane);
		UIComponent tab = mock(UIComponent.class);
		when(tab.getParent()).thenReturn(tabLayout);
		UIComponent result = builder.addedTab(null, tab);
		assertSame(tabPane, result);
	}

	@Test
	void addBorderLayoutAddsBorderLayoutToBorder() {
		UIComponent border = mock(UIComponent.class);
		List<UIComponent> borderChildren = new ArrayList<>();
		when(border.getChildren()).thenReturn(borderChildren);
		UIComponent borderLayout = mock(UIComponent.class);
		builder.addBorderLayout(border, borderLayout);
		assertTrue(borderChildren.contains(borderLayout));
	}

	@Test
	void addedBorderLayoutReturnsBorderLayoutsParent() {
		UIComponent border = mock(UIComponent.class);
		UIComponent borderLayout = mock(UIComponent.class);
		when(borderLayout.getParent()).thenReturn(border);
		UIComponent result = builder.addedBorderLayout(null, borderLayout);
		assertSame(border, result);
	}

	@Test
	void vboxLayoutCreatesPanelGridWithOneColumn() {
		VBox vbox = new VBox();
		UIComponent result = builder.vboxLayout(null, vbox);
		assertSame(mockPanelGrid, result);
		verify(mockPanelGrid).setColumns(1);
	}

	@Test
	void hboxLayoutCreatesPanelGrid() {
		HBox hbox = new HBox();
		UIComponent result = builder.hboxLayout(null, hbox);
		assertSame(mockPanelGrid, result);
	}

	@Test
	void sidebarLayoutCreatesHtmlPanelGroupWithSidebarEditClass() {
		Sidebar sidebar = new Sidebar();
		UIComponent result = builder.sidebarLayout(null, sidebar, false);
		assertSame(mockHtmlPanelGroup, result);
		verify(mockHtmlPanelGroup).setStyleClass("sidebarEdit");
	}

	@Test
	void sidebarLayoutCreateViewModeUsesCreateClass() {
		Sidebar sidebar = new Sidebar();
		UIComponent result = builder.sidebarLayout(null, sidebar, true);
		assertSame(mockHtmlPanelGroup, result);
		verify(mockHtmlPanelGroup).setStyleClass("sidebarCreate");
	}

	@Test
	void formLayoutCreatesPanelGrid() {
		Form form = new Form();
		UIComponent result = builder.formLayout(null, form);
		assertSame(mockPanelGrid, result);
	}

	@Test
	void formRowLayoutCreatesRow() {
		FormRow row = new FormRow();
		UIComponent result = builder.formRowLayout(null, row);
		assertSame(mockRow, result);
	}

	@Test
	void addFormRowLayoutAddsRowToFormAndReturnsRow() {
		UIComponent formLayout = mock(UIComponent.class);
		List<UIComponent> formChildren = new ArrayList<>();
		when(formLayout.getChildren()).thenReturn(formChildren);
		UIComponent rowLayout = mock(UIComponent.class);
		UIComponent result = builder.addFormRowLayout(null, formLayout, rowLayout);
		assertSame(rowLayout, result);
		assertTrue(formChildren.contains(rowLayout));
	}

	@Test
	void addedFormRowLayoutReturnsFormFromRow() {
		UIComponent formLayout = mock(UIComponent.class);
		UIComponent rowLayout = mock(UIComponent.class);
		when(rowLayout.getParent()).thenReturn(formLayout);
		UIComponent result = builder.addedFormRowLayout(null, rowLayout);
		assertSame(formLayout, result);
	}

	@Test
	void layoutFormItemLabelAddsColumnToRow() {
		UIComponent rowLayout = mock(UIComponent.class);
		List<UIComponent> rowChildren = new ArrayList<>();
		when(rowLayout.getChildren()).thenReturn(rowChildren);
		UIComponent widget = mock(UIComponent.class);
		when(widget.getId()).thenReturn("widget1");
		Form form = new Form();
		FormItem formItem = new FormItem();
		FormColumn formColumn = new FormColumn();
		builder.layoutFormItemLabel(rowLayout, widget, form, formItem, formColumn, "My Label", null, null, null);
		// Should add a Column to the row
		assertTrue(rowChildren.size() == 1);
		assertSame(mockColumn, rowChildren.get(0));
	}

	@Test
	void layoutFormItemWidgetAddsColumnToRow() {
		UIComponent rowLayout = mock(UIComponent.class);
		List<UIComponent> rowChildren = new ArrayList<>();
		when(rowLayout.getChildren()).thenReturn(rowChildren);
		UIComponent widget = mock(UIComponent.class);
		Form form = new Form();
		FormItem formItem = new FormItem();
		FormColumn formColumn = new FormColumn();
		builder.layoutFormItemWidget(rowLayout, widget, form, formItem, formColumn, "My Label", 1, null, null, null, null, true, false);
		assertTrue(rowChildren.size() == 1);
		assertSame(mockColumn, rowChildren.get(0));
	}

	@Test
	void contentSignatureLayoutCreatesHtmlPanelGridWithTwoColumns() {
		ContentSignature signature = new ContentSignature();
		UIComponent result = builder.contentSignatureLayout(null, signature);
		assertSame(mockHtmlPanelGrid, result);
		verify(mockHtmlPanelGrid).setColumns(2);
	}

	@Test
	void addToContainerVboxAddsDirectlyToContainer() {
		// container must be a PanelGrid (addToContainer checks instanceof PanelGrid)
		List<UIComponent> containerChildren = new ArrayList<>();
		when(mockPanelGrid.getChildren()).thenReturn(containerChildren);
		UIComponent child = mock(UIComponent.class);
		VBox vbox = new VBox();
		UIComponent result = builder.addToContainer(null, vbox, mockPanelGrid, child, null, null, null, null, null, null, null, null);
		assertSame(child, result);
		assertTrue(containerChildren.contains(child));
	}

	@Test
	void addToContainerHboxAddsColumnWithChildInRow() {
		// container must be a PanelGrid; HBox → creates Row and Column
		List<UIComponent> containerChildren = new ArrayList<>();
		when(mockPanelGrid.getChildren()).thenReturn(containerChildren);
		when(mockPanelGrid.getChildCount()).thenReturn(0); // empty → create new Row
		List<UIComponent> rowChildren = new ArrayList<>();
		when(mockRow.getChildren()).thenReturn(rowChildren);
		List<UIComponent> columnChildren = new ArrayList<>();
		when(mockColumn.getChildren()).thenReturn(columnChildren);
		UIComponent child = mock(UIComponent.class);
		HBox hbox = new HBox();
		UIComponent result = builder.addToContainer(null, hbox, mockPanelGrid, child, null, null, null, null, null, null, null, null);
		assertSame(child, result);
		assertTrue(containerChildren.size() == 1);
		assertSame(mockRow, containerChildren.get(0));
		assertTrue(rowChildren.size() == 1);
		assertSame(mockColumn, rowChildren.get(0));
		assertTrue(columnChildren.contains(child));
	}

	@Test
	void addToContainerHboxReusesExistingRow() {
		// HBox reuses existing Row when container already has a child
		Row existingRow = mock(Row.class);
		List<UIComponent> containerChildren = new ArrayList<>();
		containerChildren.add(existingRow);
		when(mockPanelGrid.getChildren()).thenReturn(containerChildren);
		when(mockPanelGrid.getChildCount()).thenReturn(1); // non-zero → reuse Row at index 0
		List<UIComponent> existingRowChildren = new ArrayList<>();
		when(existingRow.getChildren()).thenReturn(existingRowChildren);
		Column mockCol1 = mock(Column.class);
		Column mockCol2 = mock(Column.class);
		when(mockCol1.getChildren()).thenReturn(new ArrayList<>());
		when(mockCol2.getChildren()).thenReturn(new ArrayList<>());
		when(mockApplication.createComponent(Column.COMPONENT_TYPE))
			.thenReturn(mockCol1)
			.thenReturn(mockCol2);
		HBox hbox = new HBox();
		builder.addToContainer(null, hbox, mockPanelGrid, mock(UIComponent.class), null, null, null, null, null, null, null, null);
		builder.addToContainer(null, hbox, mockPanelGrid, mock(UIComponent.class), null, null, null, null, null, null, null, null);
		// Container still has only the original Row
		assertTrue(containerChildren.size() == 1);
		assertSame(existingRow, containerChildren.get(0));
		// Both columns were added to the existing row
		assertTrue(existingRowChildren.size() == 2);
	}

	@Test
	void addedToContainerVboxReturnsContainersParent() {
		UIComponent viewContainer = mock(UIComponent.class);
		UIComponent container = mock(UIComponent.class);
		when(container.getParent()).thenReturn(viewContainer);
		VBox vbox = new VBox();
		UIComponent result = builder.addedToContainer(null, vbox, container);
		assertSame(viewContainer, result);
	}

	@Test
	void addedToContainerHboxStripsColumnAndRow() {
		// HBox: addedToContainer does:
		//   result = container.getParent()        (= col)
		//   result = result.getParent().getParent() (= col.parent=row, row.parent=outer)
		UIComponent outer = mock(UIComponent.class);
		UIComponent row = mock(UIComponent.class);
		UIComponent col = mock(UIComponent.class);
		UIComponent container = mock(UIComponent.class);
		when(container.getParent()).thenReturn(col);
		when(col.getParent()).thenReturn(row);
		when(row.getParent()).thenReturn(outer);
		HBox hbox = new HBox();
		UIComponent result = builder.addedToContainer(null, hbox, container);
		assertSame(outer, result);
	}
}
