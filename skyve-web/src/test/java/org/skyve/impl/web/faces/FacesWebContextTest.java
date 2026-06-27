package org.skyve.impl.web.faces;

import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.isNull;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.skyve.domain.messages.MessageSeverity;

import jakarta.faces.application.FacesMessage;
import jakarta.faces.context.ExternalContext;
import jakarta.faces.context.FacesContext;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpSession;

@SuppressWarnings("static-method")
class FacesWebContextTest {
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
	void messageMapsSeveritiesToFacesMessages() {
		FacesContext facesContext = mock(FacesContext.class);
		ExternalContext externalContext = mock(ExternalContext.class);
		HttpServletRequest request = mock(HttpServletRequest.class);
		HttpSession session = mock(HttpSession.class);
		when(facesContext.getExternalContext()).thenReturn(externalContext);
		when(externalContext.getRequest()).thenReturn(request);
		when(request.getSession(false)).thenReturn(session);
		when(session.getId()).thenReturn("session-1");
		FacesContextBridge.setCurrent(facesContext);

		FacesWebContext context = new FacesWebContext();
		context.message(MessageSeverity.info, "info");
		context.message(MessageSeverity.warn, "warn");
		context.message(MessageSeverity.error, "error");
		context.message(MessageSeverity.fatal, "fatal");
		context.growl(MessageSeverity.info, "growl");

		ArgumentCaptor<FacesMessage> messageCaptor = ArgumentCaptor.forClass(FacesMessage.class);
		verify(facesContext, times(5)).addMessage(isNull(), messageCaptor.capture());

		FacesMessage info = messageCaptor.getAllValues().get(0);
		FacesMessage warn = messageCaptor.getAllValues().get(1);
		FacesMessage error = messageCaptor.getAllValues().get(2);
		FacesMessage fatal = messageCaptor.getAllValues().get(3);

		org.junit.jupiter.api.Assertions.assertSame(FacesMessage.SEVERITY_INFO, info.getSeverity());
		org.junit.jupiter.api.Assertions.assertSame(FacesMessage.SEVERITY_WARN, warn.getSeverity());
		org.junit.jupiter.api.Assertions.assertSame(FacesMessage.SEVERITY_ERROR, error.getSeverity());
		org.junit.jupiter.api.Assertions.assertSame(FacesMessage.SEVERITY_FATAL, fatal.getSeverity());
	}

	@Test
	void growlsAndMessagesListsAreNotStoredLocally() {
		FacesContext facesContext = mock(FacesContext.class);
		ExternalContext externalContext = mock(ExternalContext.class);
		HttpServletRequest request = mock(HttpServletRequest.class);
		HttpSession session = mock(HttpSession.class);
		when(facesContext.getExternalContext()).thenReturn(externalContext);
		when(externalContext.getRequest()).thenReturn(request);
		when(request.getSession(false)).thenReturn(session);
		when(session.getId()).thenReturn("session-2");
		FacesContextBridge.setCurrent(facesContext);

		FacesWebContext context = new FacesWebContext();

		assertNull(context.getGrowls());
		assertNull(context.getMessages());
	}
}
