package org.skyve.impl.sail.mock;

import java.util.Iterator;

import jakarta.el.ELContext;
import jakarta.faces.application.Application;
import jakarta.faces.application.FacesMessage;
import jakarta.faces.application.FacesMessage.Severity;
import jakarta.faces.component.UIViewRoot;
import jakarta.faces.context.ExternalContext;
import jakarta.faces.context.FacesContext;
import jakarta.faces.context.ResponseStream;
import jakarta.faces.context.ResponseWriter;
import jakarta.faces.lifecycle.Lifecycle;
import jakarta.faces.render.RenderKit;

/**
 * Provides a mock implementation used by SAIL execution tests in the web module.
 */
public class MockFacesContext extends FacesContext {
	private Application a = new MockApplication();
	private ELContext elc = new MockELContext();
	private UIViewRoot root = new UIViewRoot();
	private ResponseWriter responseWriter = null;
	
	/**
	 * Returns the mock JSF application used for component and behavior creation.
	 */
	@Override
	public Application getApplication() {
		return a;
	}

	/**
	 * Returns a mock EL context suitable for view rendering tests.
	 */
	@Override
	public ELContext getELContext() {
		return elc;
	}
    

	/**
	 * Returns the active mock view root.
	 */
	@Override
	public UIViewRoot getViewRoot() {
		return root;
	}

	/**
	 * Sets the active mock view root.
	 */
	@Override
	public void setViewRoot(UIViewRoot root) {
		this.root = root;
	}

	/**
	 * Returns no client IDs because messages are not retained in this mock.
	 */
	@Override
	public Iterator<String> getClientIdsWithMessages() {
		return null;
	}

	/**
	 * Returns no external context because servlet integration is not required in these tests.
	 */
	@Override
	public ExternalContext getExternalContext() {
		return null;
	}

	/**
	 * Returns no lifecycle because lifecycle integration is not required in these tests.
	 */
	@Override
	public Lifecycle getLifecycle() {
		return null;
	}

	/**
	 * Returns no severity because messages are not retained in this mock.
	 */
	@Override
	public Severity getMaximumSeverity() {
		return null;
	}

	/**
	 * Returns no messages because this mock does not store faces messages.
	 */
	@Override
	public Iterator<FacesMessage> getMessages() {
		return null;
	}

	/**
	 * Returns no messages because this mock does not store faces messages.
	 */
	@Override
	public Iterator<FacesMessage> getMessages(String clientId) {
		return null;
	}

	/**
	 * Returns no render kit because render kit resolution is not required in these tests.
	 */
	@Override
	public RenderKit getRenderKit() {
		return null;
	}

	/**
	 * Indicates that rendering should proceed during mock execution.
	 */
	@Override
	public boolean getRenderResponse() {
		return true;
	}

	/**
	 * Indicates that response completion has not been requested.
	 */
	@Override
	public boolean getResponseComplete() {
		return false;
	}

	/**
	 * Returns no response stream because this mock uses {@link ResponseWriter} only when set explicitly.
	 */
	@Override
	public ResponseStream getResponseStream() {
		return null;
	}

	/**
	 * Ignores direct response stream assignment in the mock context.
	 */
	@Override
	public void setResponseStream(ResponseStream responseStream) {
		// nothing to see here
	}

	/**
	 * Returns the currently assigned response writer.
	 */
	@Override
	public ResponseWriter getResponseWriter() {
		return responseWriter;
	}

	/**
	 * Stores the response writer for subsequent rendering assertions.
	 */
	@Override
	public void setResponseWriter(ResponseWriter responseWriter) {
		this.responseWriter = responseWriter;
	}

	/**
	 * Ignores added messages in this mock implementation.
	 */
	@Override
	public void addMessage(String clientId, FacesMessage message) {
		// nothing to see here
	}

	private boolean released = false;
	
	/**
	 * Marks the context as released.
	 */
	@Override
	public void release() {
		released = true;
	}

	/**
	 * Indicates whether {@link #release()} has been called.
	 */
	@Override
	public boolean isReleased() {
		return released;
	}

	/**
	 * No-op in mock execution.
	 */
	@Override
	public void renderResponse() {
		// nothing to see here
	}

	/**
	 * No-op in mock execution.
	 */
	@Override
	public void responseComplete() {
		// nothing to see here
	}
}
