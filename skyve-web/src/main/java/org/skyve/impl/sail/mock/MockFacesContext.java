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

public class MockFacesContext extends FacesContext {
	private Application a = new MockApplication();
	private ELContext elc = new MockELContext();
	private UIViewRoot root = new UIViewRoot();
	private ResponseWriter responseWriter = null;
	
	@Override
	public Application getApplication() {
		return a;
	}

	@Override
	public ELContext getELContext() {
		return elc;
	}
    

	@Override
	public UIViewRoot getViewRoot() {
		return root;
	}

	@Override
	public void setViewRoot(UIViewRoot root) {
		this.root = root;
	}

	@Override
	public Iterator<String> getClientIdsWithMessages() {
		return null;
	}

	@Override
	public ExternalContext getExternalContext() {
		return null;
	}

	@Override
	public Lifecycle getLifecycle() {
		return null;
	}

	@Override
	public Severity getMaximumSeverity() {
		return null;
	}

	@Override
	public Iterator<FacesMessage> getMessages() {
		return null;
	}

	@Override
	public Iterator<FacesMessage> getMessages(String clientId) {
		return null;
	}

	@Override
	public RenderKit getRenderKit() {
		return null;
	}

	@Override
	public boolean getRenderResponse() {
		return true;
	}

	@Override
	public boolean getResponseComplete() {
		return false;
	}

	@Override
	public ResponseStream getResponseStream() {
		return null;
	}

	@Override
	public void setResponseStream(ResponseStream responseStream) {
		// nothing to see here
	}

	@Override
	public ResponseWriter getResponseWriter() {
		return responseWriter;
	}

	@Override
	public void setResponseWriter(ResponseWriter responseWriter) {
		this.responseWriter = responseWriter;
	}

	@Override
	public void addMessage(String clientId, FacesMessage message) {
		// nothing to see here
	}

	@Override
	public void release() {
		// nothing to see here
	}

	@Override
	public void renderResponse() {
		// nothing to see here
	}

	@Override
	public void responseComplete() {
		// nothing to see here
	}
}
