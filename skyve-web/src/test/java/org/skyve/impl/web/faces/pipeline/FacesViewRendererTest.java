package org.skyve.impl.web.faces.pipeline;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.module.ModuleImpl;
import org.skyve.impl.metadata.view.ViewImpl;
import org.skyve.impl.sail.mock.MockFacesContext;
import org.skyve.impl.web.faces.pipeline.component.ComponentBuilder;
import org.skyve.impl.web.faces.pipeline.layout.LayoutBuilder;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.View.ViewType;

import jakarta.faces.component.UIComponent;
import jakarta.faces.component.UIComponentBase;
import jakarta.faces.context.FacesContext;

@SuppressWarnings("static-method")
class FacesViewRendererTest {

	private abstract static class FacesContextBridge extends FacesContext {
		static void setCurrent(FacesContext context) {
			setCurrentInstance(context);
		}
	}

	private static final class TestComponent extends UIComponentBase {
		private final String family;

		private TestComponent(String family) {
			this.family = family;
		}

		@Override
		public String getFamily() {
			return family;
		}
	}

	@AfterEach
	void clearFacesContext() {
		FacesContextBridge.setCurrent(null);
	}

	@Test
	void renderViewAddsLayoutWhenWidgetIdIsNull() {
		UIComponent root = new TestComponent("root");
		UIComponent layout = new TestComponent("layout");
		ComponentBuilder cb = mock(ComponentBuilder.class);
		LayoutBuilder lb = mock(LayoutBuilder.class);
		ViewImpl view = createView(null);

		when(cb.view(null, false)).thenReturn(root);
		when(lb.toolbarLayouts(null)).thenReturn(new ArrayList<>());
		when(lb.viewLayout(null)).thenReturn(layout);

		FacesViewRenderer renderer = newRenderer(view, null, cb, lb);
		renderer.renderView(null, null);

		assertEquals(1, root.getChildren().size());
		assertSame(layout, root.getChildren().get(0));
	}

	@Test
	void renderViewSkipsLayoutWhenWidgetIdIsPresent() {
		UIComponent root = new TestComponent("root");
		UIComponent layout = new TestComponent("layout");
		ComponentBuilder cb = mock(ComponentBuilder.class);
		LayoutBuilder lb = mock(LayoutBuilder.class);
		ViewImpl view = createView(null);

		when(cb.view(null, false)).thenReturn(root);
		when(lb.toolbarLayouts(null)).thenReturn(new ArrayList<>());
		when(lb.viewLayout(null)).thenReturn(layout);

		FacesViewRenderer renderer = newRenderer(view, "fragment", cb, lb);
		renderer.renderView(null, null);

		assertEquals(0, root.getChildren().size());
	}

	@Test
	void renderedViewAddsToolbarsWhenActionsWidgetMatches() {
		UIComponent root = new TestComponent("root");
		UIComponent toolbarLayout = new TestComponent("toolbarLayout");
		toolbarLayout.getChildren().add(new TestComponent("child"));
		UIComponent toolbar = new TestComponent("toolbar");
		ComponentBuilder cb = mock(ComponentBuilder.class);
		LayoutBuilder lb = mock(LayoutBuilder.class);
		ViewImpl view = createView("actions");

		when(cb.view(null, false)).thenReturn(root);
		when(cb.toolbars(null, "actions")).thenReturn(List.of(toolbar));
		when(lb.toolbarLayouts(null)).thenReturn(List.of(toolbarLayout));
		when(lb.viewLayout(null)).thenReturn(null);

		FacesViewRenderer renderer = newRenderer(view, null, cb, lb);
		renderer.renderView(null, null);
		renderer.renderedView(null, null);

		verify(cb).toolbars(null, "actions");
		verify(lb).addToolbarLayouts(List.of(toolbar), List.of(toolbarLayout));
		verify(lb).addToolbarsOrLayouts(root, List.of(toolbar));
	}

	@Test
	void renderedViewSkipsToolbarsWhenWidgetIdDoesNotMatchActionsWidgetId() {
		UIComponent root = new TestComponent("root");
		ComponentBuilder cb = mock(ComponentBuilder.class);
		LayoutBuilder lb = mock(LayoutBuilder.class);
		ViewImpl view = createView("actions");

		when(cb.view(null, false)).thenReturn(root);
		when(lb.toolbarLayouts(null)).thenReturn(List.of());
		when(lb.viewLayout(null)).thenReturn(null);

		FacesViewRenderer renderer = newRenderer(view, "fragment", cb, lb);
		renderer.renderView(null, null);
		renderer.renderedView(null, null);

		verify(cb, never()).toolbars(null, "actions");
	}

	private static ViewImpl createView(String actionsWidgetId) {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");
		view.setActionsWidgetId(actionsWidgetId);
		return view;
	}

	private static FacesViewRenderer newRenderer(ViewImpl view,
						String widgetId,
						ComponentBuilder cb,
						LayoutBuilder lb) {
		FacesContextBridge.setCurrent(new MockFacesContext());
		CustomerImpl customer = new CustomerImpl();
		User user = mock(User.class);
		when(user.getCustomer()).thenReturn(customer);
		ModuleImpl module = new ModuleImpl();
		module.setName("testModule");
		DocumentImpl document = new DocumentImpl();
		document.setName("testDocument");
		document.setOwningModuleName("testModule");
		return new FacesViewRenderer(user, module, document, view, "external", widgetId, cb, lb);
	}
}