package org.skyve.impl.web.faces.actions;

import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.web.AbstractWebContext;
import org.skyve.impl.web.faces.views.FacesView;
import org.skyve.metadata.router.UxUi;
import org.skyve.metadata.user.User;
import org.skyve.web.UserAgentType;
import org.skyve.web.WebAction;

import jakarta.faces.context.ExternalContext;
import jakarta.faces.context.FacesContext;
import jakarta.servlet.http.HttpServletRequest;

@SuppressWarnings("static-method")
class PreRenderColdHitActionTest {
	private abstract static class FacesContextBridge extends FacesContext {
		static void setCurrent(FacesContext context) {
			setCurrentInstance(context);
		}
	}

	@AfterEach
	void tearDown() throws Exception {
		FacesContextBridge.setCurrent(null);
		unbindPersistenceFromThread();
	}

	@Test
	void callbackReturnsWhenNoUserInThreadContext() throws Exception {
		FacesView facesView = mock(FacesView.class);
		when(facesView.getUserAgentType()).thenReturn(UserAgentType.desktop);
		when(facesView.getUxUi()).thenReturn(UxUi.newPrimeFaces("desktop", "ultima", "saga"));
		bindPersistenceToThread(null);

		setFacesRequestContext(mock(HttpServletRequest.class));

		PreRenderColdHitAction action = new PreRenderColdHitAction(facesView);
		assertNull(action.callback());

		verify(facesView, never()).initialise();
	}

	@Test
	void callbackInitialisesForNonEditAndNonListAction() throws Exception {
		FacesView facesView = mock(FacesView.class);
		when(facesView.getUserAgentType()).thenReturn(UserAgentType.desktop);
		when(facesView.getUxUi()).thenReturn(UxUi.newPrimeFaces("desktop", "ultima", "saga"));
		when(facesView.getWebActionParameter()).thenReturn(WebAction.c);
		when(facesView.getBizModuleParameter()).thenReturn("admin");
		when(facesView.getBizDocumentParameter()).thenReturn("Contact");
		when(facesView.getQueryNameParameter()).thenReturn("q");
		when(facesView.getBizIdParameter()).thenReturn("id");
		bindPersistenceToThread(mock(User.class));

		setFacesRequestContext(mock(HttpServletRequest.class));

		PreRenderColdHitAction action = new PreRenderColdHitAction(facesView);
		assertNull(action.callback());

		verify(facesView).initialise();
	}

	@Test
	void callbackDerivesUserAgentAndUxUiFromRequestAttributes() throws Exception {
		FacesView facesView = mock(FacesView.class);
		when(facesView.getUserAgentType()).thenReturn(null);
		when(facesView.getUxUi()).thenReturn(null);
		bindPersistenceToThread(null);

		HttpServletRequest request = mock(HttpServletRequest.class);
		UxUi uxui = UxUi.newPrimeFaces("tablet", "ultima", "saga");
		when(request.getAttribute(AbstractWebContext.USER_AGENT_TYPE_KEY)).thenReturn(UserAgentType.tablet);
		when(request.getAttribute(AbstractWebContext.UXUI)).thenReturn(uxui);
		setFacesRequestContext(request);

		PreRenderColdHitAction action = new PreRenderColdHitAction(facesView);
		assertNull(action.callback());

		verify(facesView).setUserAgentType(UserAgentType.tablet);
		verify(facesView).setUxUi(uxui);
		verify(facesView, never()).initialise();
	}

	private static void setFacesRequestContext(HttpServletRequest request) {
		FacesContext facesContext = mock(FacesContext.class);
		ExternalContext externalContext = mock(ExternalContext.class);
		when(externalContext.getRequest()).thenReturn(request);
		when(facesContext.getExternalContext()).thenReturn(externalContext);
		FacesContextBridge.setCurrent(facesContext);
	}

	@SuppressWarnings("unchecked")
	private static void bindPersistenceToThread(User user) throws Exception {
		AbstractPersistence persistence = mock(AbstractPersistence.class);
		when(persistence.getUser()).thenReturn(user);
		Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
		field.setAccessible(true);
		((ThreadLocal<AbstractPersistence>) field.get(null)).set(persistence);
	}

	@SuppressWarnings("unchecked")
	private static void unbindPersistenceFromThread() throws Exception {
		Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
		field.setAccessible(true);
		((ThreadLocal<AbstractPersistence>) field.get(null)).remove();
	}
}