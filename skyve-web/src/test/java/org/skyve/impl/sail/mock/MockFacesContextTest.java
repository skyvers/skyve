package org.skyve.impl.sail.mock;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

import jakarta.faces.component.UIViewRoot;
import jakarta.faces.context.ResponseWriter;

@SuppressWarnings("static-method")
class MockFacesContextTest {
	@Test
	void defaultStateAndAccessorsBehaveAsDocumented() {
		MockFacesContext context = new MockFacesContext();

		assertNotNull(context.getApplication());
		assertNotNull(context.getELContext());
		assertNotNull(context.getViewRoot());
		assertNull(context.getClientIdsWithMessages());
		assertNull(context.getExternalContext());
		assertNull(context.getLifecycle());
		assertNull(context.getMaximumSeverity());
		assertNull(context.getMessages());
		assertNull(context.getMessages("client"));
		assertNull(context.getRenderKit());
		assertTrue(context.getRenderResponse());
		assertFalse(context.getResponseComplete());
		assertNull(context.getResponseStream());
	}

	@Test
	void viewRootAndWriterCanBeReplacedAndReleasedFlagTracksLifecycle() {
		MockFacesContext context = new MockFacesContext();
		UIViewRoot root = new UIViewRoot();
		ResponseWriter writer = org.mockito.Mockito.mock(ResponseWriter.class);

		context.setViewRoot(root);
		context.setResponseStream(null);
		context.setResponseWriter(writer);
		context.addMessage(null, null);
		context.renderResponse();
		context.responseComplete();

		assertTrue(context.getViewRoot() == root);
		assertTrue(context.getResponseWriter() == writer);
		assertFalse(context.isReleased());
		context.release();
		assertTrue(context.isReleased());
	}
}
