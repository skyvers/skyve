package org.skyve.impl.sail.mock;

import java.util.Iterator;

import javax.el.ELContext;
import javax.faces.application.Application;
import javax.faces.application.FacesMessage;
import javax.faces.application.FacesMessage.Severity;
import javax.faces.component.UIViewRoot;
import javax.faces.context.ExternalContext;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseStream;
import javax.faces.context.ResponseWriter;
import javax.faces.render.RenderKit;

public class MockFacesContext extends FacesContext {
	private Application a = new MockApplication();
	private ELContext elc = new MockELContext();
	private UIViewRoot root = new UIViewRoot();
	
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
		return null;
	}

	@Override
	public void setResponseWriter(ResponseWriter responseWriter) {
		// nothing to see here
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
