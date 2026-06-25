package org.skyve.impl.web.faces.pipeline.layout;

import static org.junit.jupiter.api.Assertions.assertSame;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.Container;
import org.skyve.impl.metadata.view.container.HBox;
import org.skyve.impl.metadata.view.container.Sidebar;
import org.skyve.impl.metadata.view.container.VBox;
import org.skyve.impl.metadata.view.container.form.Form;
import org.skyve.impl.metadata.view.container.form.FormColumn;
import org.skyve.impl.metadata.view.container.form.FormItem;
import org.skyve.impl.metadata.view.container.form.FormRow;
import org.skyve.impl.metadata.view.widget.bound.input.ContentSignature;
import org.skyve.impl.web.faces.views.FacesView;
import org.skyve.web.UserAgentType;

import jakarta.el.ELContext;
import jakarta.el.ExpressionFactory;
import jakarta.faces.application.Application;
import jakarta.faces.component.UIComponent;
import jakarta.faces.context.FacesContext;

/**
 * Tests for LayoutBuilderChain delegation and state propagation.
 */
@SuppressWarnings({"static-method", "boxing"})
class LayoutBuilderChainTest {

	private abstract static class FacesContextBridge extends FacesContext {
		static void setCurrent(FacesContext facesContext) {
			setCurrentInstance(facesContext);
		}
	}

	@BeforeAll
	static void setUpFacesContext() {
		FacesContext facesContext = mock(FacesContext.class);
		Application mockApplication = mock(Application.class);
		ExpressionFactory mockExpressionFactory = mock(ExpressionFactory.class);
		ELContext elContext = mock(ELContext.class);
		when(facesContext.getApplication()).thenReturn(mockApplication);
		when(facesContext.getELContext()).thenReturn(elContext);
		when(mockApplication.getExpressionFactory()).thenReturn(mockExpressionFactory);
		FacesContextBridge.setCurrent(facesContext);
	}

	@AfterAll
	static void tearDownFacesContext() {
		FacesContextBridge.setCurrent(null);
	}

	private LayoutBuilder mockBuilder;
	private LayoutBuilderChain chain;
	private UIComponent mockComponent;

	@BeforeEach
	void setUp() {
		mockBuilder = mock(LayoutBuilder.class);
		chain = new LayoutBuilderChain(mockBuilder);
		mockComponent = mock(UIComponent.class);
	}

	// ===== Empty chain =====

	@Test
	void emptyChainViewLayoutReturnsInputComponent() {
		LayoutBuilderChain emptyChain = new LayoutBuilderChain();
		assertSame(mockComponent, emptyChain.viewLayout(mockComponent));
	}

	@Test
	void emptyChainToolbarLayoutsReturnsInputList() {
		LayoutBuilderChain emptyChain = new LayoutBuilderChain();
		List<UIComponent> list = new ArrayList<>();
		assertSame(list, emptyChain.toolbarLayouts(list));
	}

	// ===== State propagation =====

	@Test
	void setSAILManagedBeanPropagates() {
		FacesView view = mock(FacesView.class);
		chain.setSAILManagedBean(view);
		verify(mockBuilder).setSAILManagedBean(view);
	}

	@Test
	void setProcessPropagates() {
		chain.setProcess("@form");
		verify(mockBuilder).setProcess("@form");
	}

	@Test
	void setUpdatePropagates() {
		chain.setUpdate("@(form)");
		verify(mockBuilder).setUpdate("@(form)");
	}

	@Test
	void setUserAgentTypePropagates() {
		chain.setUserAgentType(UserAgentType.desktop);
		verify(mockBuilder).setUserAgentType(UserAgentType.desktop);
	}

	// ===== Delegation methods =====

	@Test
	void viewLayoutDelegatesToBuilder() {
		UIComponent result = mock(UIComponent.class);
		when(mockBuilder.viewLayout(mockComponent)).thenReturn(result);
		assertSame(result, chain.viewLayout(mockComponent));
	}

	@Test
	void toolbarLayoutsDelegatesToBuilder() {
		List<UIComponent> list = new ArrayList<>();
		List<UIComponent> result = new ArrayList<>();
		when(mockBuilder.toolbarLayouts(list)).thenReturn(result);
		assertSame(result, chain.toolbarLayouts(list));
	}

	@Test
	void addToolbarLayoutsDelegatesToBuilder() {
		List<UIComponent> toolbars = new ArrayList<>();
		List<UIComponent> layouts = new ArrayList<>();
		chain.addToolbarLayouts(toolbars, layouts);
		verify(mockBuilder).addToolbarLayouts(toolbars, layouts);
	}

	@Test
	void addToolbarsOrLayoutsDelegatesToBuilder() {
		List<UIComponent> items = new ArrayList<>();
		chain.addToolbarsOrLayouts(mockComponent, items);
		verify(mockBuilder).addToolbarsOrLayouts(mockComponent, items);
	}

	@Test
	void tabLayoutDelegatesToBuilder() {
		UIComponent result = mock(UIComponent.class);
		when(mockBuilder.tabLayout(mockComponent)).thenReturn(result);
		assertSame(result, chain.tabLayout(mockComponent));
	}

	@Test
	void addTabLayoutDelegatesToBuilder() {
		UIComponent tab = mock(UIComponent.class);
		UIComponent tabLayout = mock(UIComponent.class);
		UIComponent result = mock(UIComponent.class);
		when(mockBuilder.addTabLayout(mockComponent, tab, tabLayout)).thenReturn(result);
		assertSame(result, chain.addTabLayout(mockComponent, tab, tabLayout));
	}

	@Test
	void addTabDelegatesToBuilder() {
		UIComponent tabPane = mock(UIComponent.class);
		UIComponent tab = mock(UIComponent.class);
		chain.addTab(tabPane, tab);
		verify(mockBuilder).addTab(tabPane, tab);
	}

	@Test
	void addedTabDelegatesToBuilder() {
		UIComponent tab = mock(UIComponent.class);
		UIComponent result = mock(UIComponent.class);
		when(mockBuilder.addedTab(mockComponent, tab)).thenReturn(result);
		assertSame(result, chain.addedTab(mockComponent, tab));
	}

	@Test
	void addBorderLayoutDelegatesToBuilder() {
		UIComponent border = mock(UIComponent.class);
		UIComponent borderLayout = mock(UIComponent.class);
		chain.addBorderLayout(border, borderLayout);
		verify(mockBuilder).addBorderLayout(border, borderLayout);
	}

	@Test
	void addedBorderLayoutDelegatesToBuilder() {
		UIComponent borderLayout = mock(UIComponent.class);
		UIComponent result = mock(UIComponent.class);
		when(mockBuilder.addedBorderLayout(mockComponent, borderLayout)).thenReturn(result);
		assertSame(result, chain.addedBorderLayout(mockComponent, borderLayout));
	}

	@Test
	void vboxLayoutDelegatesToBuilder() {
		VBox vbox = mock(VBox.class);
		UIComponent result = mock(UIComponent.class);
		when(mockBuilder.vboxLayout(mockComponent, vbox)).thenReturn(result);
		assertSame(result, chain.vboxLayout(mockComponent, vbox));
	}

	@Test
	void hboxLayoutDelegatesToBuilder() {
		HBox hbox = mock(HBox.class);
		UIComponent result = mock(UIComponent.class);
		when(mockBuilder.hboxLayout(mockComponent, hbox)).thenReturn(result);
		assertSame(result, chain.hboxLayout(mockComponent, hbox));
	}

	@Test
	void sidebarLayoutDelegatesToBuilder() {
		Sidebar sidebar = mock(Sidebar.class);
		UIComponent result = mock(UIComponent.class);
		when(mockBuilder.sidebarLayout(mockComponent, sidebar, true)).thenReturn(result);
		assertSame(result, chain.sidebarLayout(mockComponent, sidebar, true));
	}

	@Test
	void formLayoutDelegatesToBuilder() {
		Form form = new Form();
		UIComponent result = mock(UIComponent.class);
		when(mockBuilder.formLayout(mockComponent, form)).thenReturn(result);
		assertSame(result, chain.formLayout(mockComponent, form));
	}

	@Test
	void formRowLayoutDelegatesToBuilder() {
		FormRow row = mock(FormRow.class);
		UIComponent result = mock(UIComponent.class);
		when(mockBuilder.formRowLayout(mockComponent, row)).thenReturn(result);
		assertSame(result, chain.formRowLayout(mockComponent, row));
	}

	@Test
	void addFormRowLayoutDelegatesToBuilder() {
		UIComponent formLayout = mock(UIComponent.class);
		UIComponent rowLayout = mock(UIComponent.class);
		UIComponent result = mock(UIComponent.class);
		when(mockBuilder.addFormRowLayout(mockComponent, formLayout, rowLayout)).thenReturn(result);
		assertSame(result, chain.addFormRowLayout(mockComponent, formLayout, rowLayout));
	}

	@Test
	void addedFormRowLayoutDelegatesToBuilder() {
		UIComponent rowLayout = mock(UIComponent.class);
		UIComponent result = mock(UIComponent.class);
		when(mockBuilder.addedFormRowLayout(mockComponent, rowLayout)).thenReturn(result);
		assertSame(result, chain.addedFormRowLayout(mockComponent, rowLayout));
	}

	@Test
	void layoutFormItemLabelDelegatesToBuilder() {
		UIComponent formItemComponent = mock(UIComponent.class);
		Form form = new Form();
		FormItem formItem = mock(FormItem.class);
		FormColumn formColumn = mock(FormColumn.class);
		chain.layoutFormItemLabel(mockComponent, formItemComponent, form, formItem, formColumn, "label", true, null, false, "invisible", "help");
		verify(mockBuilder).layoutFormItemLabel(mockComponent, formItemComponent, form, formItem, formColumn, "label", true, null, false, "invisible", "help");
	}

	@Test
	void layoutFormItemWidgetDelegatesToBuilder() {
		UIComponent formItemComponent = mock(UIComponent.class);
		Form form = new Form();
		FormItem formItem = mock(FormItem.class);
		FormColumn formColumn = mock(FormColumn.class);
		chain.layoutFormItemWidget(mockComponent, formItemComponent, form, formItem, formColumn, "label", true, 1, null, false, "invisible", "help", false, 100, true, false);
		verify(mockBuilder).layoutFormItemWidget(mockComponent, formItemComponent, form, formItem, formColumn, "label", true, 1, null, false, "invisible", "help", false, 100, true, false);
	}

	@Test
	void contentSignatureLayoutDelegatesToBuilder() {
		ContentSignature signature = mock(ContentSignature.class);
		UIComponent result = mock(UIComponent.class);
		when(mockBuilder.contentSignatureLayout(mockComponent, signature)).thenReturn(result);
		assertSame(result, chain.contentSignatureLayout(mockComponent, signature));
	}

	@Test
	void addToContainerDelegatesToBuilder() {
		Container viewContainer = mock(Container.class);
		UIComponent container = mock(UIComponent.class);
		UIComponent componentToAdd = mock(UIComponent.class);
		UIComponent result = mock(UIComponent.class);
		when(mockBuilder.addToContainer(mockComponent, viewContainer, container, componentToAdd, 100, 4, 50, 1, 2, 3, 4, "invisible")).thenReturn(result);
		assertSame(result, chain.addToContainer(mockComponent, viewContainer, container, componentToAdd, 100, 4, 50, 1, 2, 3, 4, "invisible"));
	}

	@Test
	void addedToContainerDelegatesToBuilder() {
		Container viewContainer = mock(Container.class);
		UIComponent container = mock(UIComponent.class);
		UIComponent result = mock(UIComponent.class);
		when(mockBuilder.addedToContainer(mockComponent, viewContainer, container)).thenReturn(result);
		assertSame(result, chain.addedToContainer(mockComponent, viewContainer, container));
	}

	// ===== Two-builder chaining =====

	@Test
	void twoBuilderChainPassesResultThrough() {
		LayoutBuilder second = mock(LayoutBuilder.class);
		LayoutBuilderChain twoChain = new LayoutBuilderChain(mockBuilder, second);
		UIComponent midResult = mock(UIComponent.class);
		UIComponent finalResult = mock(UIComponent.class);
		when(mockBuilder.viewLayout(mockComponent)).thenReturn(midResult);
		when(second.viewLayout(midResult)).thenReturn(finalResult);
		assertSame(finalResult, twoChain.viewLayout(mockComponent));
	}
}
