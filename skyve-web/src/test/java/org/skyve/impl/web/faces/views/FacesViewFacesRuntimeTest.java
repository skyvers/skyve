package org.skyve.impl.web.faces.views;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.List;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.view.widget.bound.input.CompleteType;
import org.skyve.impl.sail.mock.MockFacesContext;
import org.skyve.impl.web.faces.FacesUtil;
import org.skyve.impl.web.faces.pipeline.ResponsiveFormGrid;
import org.skyve.impl.web.faces.pipeline.ResponsiveFormGrid.ResponsiveGridStyle;

import jakarta.faces.component.UIComponent;
import jakarta.faces.component.UIPanel;

@SuppressWarnings("static-method")
class FacesViewFacesRuntimeTest {
	@Test
	void repeatedStyleReadsPreserveFieldWidthAcrossRequests() {
		FacesView view = new FacesView();
		ResponsiveGridStyle labelStyle = new ResponsiveGridStyle(12, 4, 4, 4);
		ResponsiveGridStyle fieldStyle = new ResponsiveGridStyle(12, 8, 8, 8);
		ResponsiveFormGrid grid = new ResponsiveFormGrid(new ResponsiveGridStyle[] {labelStyle, fieldStyle});
		// A new request must recalculate widths, including when an earlier component is hidden.
		for (int request = 0; request < 2; request++) {
			try (MockFacesContext context = MockFacesContext.get()) {
				context.getViewRoot().getAttributes().put(FacesUtil.FORM_STYLES_KEY, List.of(grid));
				view.resetResponsiveFormStyle(0);
				for (int column = request; column < 2; column++) {
					UIPanel component = new UIPanel();
					component.setId("column" + column);
					component.pushComponentToEL(context, component);
					try {
						String expected = ((column == request) ? labelStyle : fieldStyle).toString() + " leftForm";
						assertEquals(expected, view.getResponsiveFormStyle(0, "leftForm", 1));
						assertEquals(expected, view.getResponsiveFormStyle(0, "leftForm", 1));
					}
					finally {
						component.popComponentFromEL(context);
					}
				}
			}
		}
	}

	@Test
	void mockFacesContextCanBeCreatedWhenMojarraRuntimeIsPresent() {
		try (MockFacesContext context = MockFacesContext.get()) {
			assertNotNull(context.getViewRoot());
		}
	}

	@Test
	void completeAndLookupCanBeInvokedWithCurrentComponentContext() {
		try (MockFacesContext context = MockFacesContext.get()) {
			UIPanel component = new UIPanel();
			component.getAttributes().put("binding", "name");
			component.getAttributes().put("complete", CompleteType.previous);
			component.getAttributes().put("module", "admin");
			component.getAttributes().put("document", "Contact");
			component.getAttributes().put("query", "");
			component.getAttributes().put("display", "name");
			component.pushComponentToEL(context, component);
			assertNotNull(UIComponent.getCurrentComponent(context));

			FacesView view = new FacesView();
			invokeIgnoringThrowable(() -> view.complete("abc"));
			invokeIgnoringThrowable(() -> view.lookup("abc"));

			component.popComponentFromEL(context);
		}
	}

	private static void invokeIgnoringThrowable(Runnable invocation) {
		try {
			invocation.run();
		}
		catch (Exception ignored) {
			ignored.getClass();
		}
	}
}
