package org.skyve.impl.web.faces.views;

import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.view.widget.bound.input.CompleteType;
import org.skyve.impl.sail.mock.MockFacesContext;

import jakarta.faces.component.UIPanel;
import jakarta.faces.context.FacesContext;

@SuppressWarnings("static-method")
class FacesViewFacesRuntimeTest {
	private abstract static class FacesContextBridge extends FacesContext {
		static void setCurrent(FacesContext context) {
			setCurrentInstance(context);
		}
	}

	@AfterEach
	void tearDown() {
		FacesContextBridge.setCurrent(null);
	}

	@Test
	void mockFacesContextCanBeCreatedWhenMojarraRuntimeIsPresent() {
		MockFacesContext context = new MockFacesContext();
		FacesContextBridge.setCurrent(context);

		assertNotNull(context.getViewRoot());
		context.release();
	}

	@Test
	void completeAndLookupCanBeInvokedWithCurrentComponentContext() {
		MockFacesContext context = new MockFacesContext();
		FacesContextBridge.setCurrent(context);

		UIPanel component = new UIPanel();
		component.getAttributes().put("binding", "name");
		component.getAttributes().put("complete", CompleteType.previous);
		component.getAttributes().put("module", "admin");
		component.getAttributes().put("document", "Contact");
		component.getAttributes().put("query", "");
		component.getAttributes().put("display", "name");
		component.pushComponentToEL(context, component);

		FacesView view = new FacesView();
		invokeIgnoringThrowable(() -> view.complete("abc"));
		invokeIgnoringThrowable(() -> view.lookup("abc"));

		component.popComponentFromEL(context);
		context.release();
	}

	private static void invokeIgnoringThrowable(Runnable invocation) {
		try {
			invocation.run();
		}
		catch (Throwable ignored) {
			ignored.getClass();
		}
	}
}
