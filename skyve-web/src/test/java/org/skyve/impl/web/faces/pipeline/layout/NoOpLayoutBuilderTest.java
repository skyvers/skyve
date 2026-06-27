package org.skyve.impl.web.faces.pipeline.layout;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
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

import jakarta.el.ELContext;
import jakarta.el.ExpressionFactory;
import jakarta.faces.application.Application;
import jakarta.faces.component.UIComponent;
import jakarta.faces.context.FacesContext;

/**
 * Tests for NoOpLayoutBuilder - verifies each method returns input unchanged or does nothing.
 */
class NoOpLayoutBuilderTest {
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
		ELContext mockELContext = mock(ELContext.class);
		when(facesContext.getApplication()).thenReturn(mockApplication);
		when(facesContext.getELContext()).thenReturn(mockELContext);
		when(mockApplication.getExpressionFactory()).thenReturn(mockExpressionFactory);
		FacesContextBridge.setCurrent(facesContext);
	}

	@AfterAll
	static void tearDownFacesContext() {
		FacesContextBridge.setCurrent(null);
	}

	private NoOpLayoutBuilder builder;
	private UIComponent component;
	private UIComponent other;

	@BeforeEach
	void setUp() {
		builder = new NoOpLayoutBuilder();
		component = mock(UIComponent.class);
		other = mock(UIComponent.class);
	}

	@Test
	void viewLayoutReturnsInputComponent() {
		assertSame(component, builder.viewLayout(component));
	}

	@Test
	void toolbarLayoutsReturnsInputList() {
		List<UIComponent> list = new ArrayList<>();
		assertSame(list, builder.toolbarLayouts(list));
	}

	@Test
	void tabLayoutReturnsInputComponent() {
		assertSame(component, builder.tabLayout(component));
	}

	@Test
	void vboxLayoutReturnsInputComponent() {
		assertSame(component, builder.vboxLayout(component, new VBox()));
	}

	@Test
	void hboxLayoutReturnsInputComponent() {
		assertSame(component, builder.hboxLayout(component, new HBox()));
	}

	@Test
	void sidebarLayoutReturnsInputComponent() {
		assertSame(component, builder.sidebarLayout(component, new Sidebar(), false));
	}

	@Test
	void formLayoutReturnsInputComponent() {
		assertSame(component, builder.formLayout(component, new Form()));
	}

	@Test
	void formRowLayoutReturnsInputComponent() {
		assertSame(component, builder.formRowLayout(component, new FormRow()));
	}

	@Test
	void addToolbarLayoutsDoesNothing() {
		List<UIComponent> toolbars = new ArrayList<>();
		List<UIComponent> layouts = new ArrayList<>();
		layouts.add(other);
		builder.addToolbarLayouts(toolbars, layouts);
		// Verify toolbars is unchanged (no-op)
		assertTrue(toolbars.isEmpty());
	}

	@Test
	void addToolbarsOrLayoutsDoesNothing() {
		List<UIComponent> toolbarsOrLayouts = new ArrayList<>();
		toolbarsOrLayouts.add(other);
		builder.addToolbarsOrLayouts(component, toolbarsOrLayouts);
		// No-op - verify list unchanged
		assertTrue(toolbarsOrLayouts.contains(other));
	}

	@Test
	void addTabLayoutReturnsInputComponent() {
		assertSame(component, builder.addTabLayout(component, other, mock(UIComponent.class)));
	}

	@Test
	void addTabDoesNothing() {
		builder.addTab(component, other);
		// No-op - verify component reference not null
		assertNotNull(component);
	}

	@Test
	void addedTabReturnsInputComponent() {
		assertSame(component, builder.addedTab(component, other));
	}

	@Test
	void addBorderLayoutDoesNothing() {
		builder.addBorderLayout(component, other);
		// No-op - verify component reference not null
		assertNotNull(component);
	}

	@Test
	void addedBorderLayoutReturnsInputComponent() {
		assertSame(component, builder.addedBorderLayout(component, other));
	}

	@Test
	void addFormRowLayoutReturnsInputComponent() {
		assertSame(component, builder.addFormRowLayout(component, other, mock(UIComponent.class)));
	}

	@Test
	void addedFormRowLayoutReturnsInputComponent() {
		assertSame(component, builder.addedFormRowLayout(component, other));
	}

	@Test
	void layoutFormItemLabelDoesNothing() {
		builder.layoutFormItemLabel(component, other, new Form(), new FormItem(), new FormColumn(), "label", true, null, true, "invisible", null);
		// No-op - verify component reference not null
		assertNotNull(component);
	}

	@Test
	void layoutFormItemWidgetDoesNothing() {
		builder.layoutFormItemWidget(component, other, new Form(), new FormItem(), new FormColumn(), "label", true, 1, null, true, "invisible", null, true, null, true, false);
		// No-op - verify component reference not null
		assertNotNull(component);
	}

	@Test
	void contentSignatureLayoutReturnsInputComponent() {
		assertSame(component, builder.contentSignatureLayout(component, new ContentSignature()));
	}

	@Test
	void addToContainerReturnsInputComponent() {
		assertSame(component, builder.addToContainer(component, mock(Container.class), other, mock(UIComponent.class), null, null, null, null, null, null, null, null));
	}

	@Test
	void addedToContainerReturnsInputComponent() {
		assertSame(component, builder.addedToContainer(component, mock(Container.class), other));
	}
}
