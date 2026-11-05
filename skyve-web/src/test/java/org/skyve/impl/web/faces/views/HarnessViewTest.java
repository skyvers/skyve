package org.skyve.impl.web.faces.views;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

import java.util.HashMap;
import java.util.Map;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.impl.web.AbstractWebContext;
import org.skyve.metadata.repository.ProvidedRepository;
import org.skyve.metadata.router.UxUi;
import org.skyve.metadata.user.User;
import org.skyve.web.WebContext;

import jakarta.faces.context.ExternalContext;
import jakarta.faces.context.FacesContext;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpSession;

@SuppressWarnings({ "static-method", "java:S5960" }) // Assertions are intentionally test-only.
class HarnessViewTest {
	private abstract static class FacesContextBridge extends FacesContext {
		static void setCurrent(FacesContext context) {
			setCurrentInstance(context);
		}
	}

	@AfterEach
	void clearFacesContext() {
		FacesContextBridge.setCurrent(null);
	}

	@Test
	@SuppressWarnings("boxing")
	void setUxUiChangesOnlyNextRequestSessionPreference() {
		User user = mock(User.class);
		when(user.canSwitchMode()).thenReturn(true);
		Map<String, Object> sessionMap = new HashMap<>();
		sessionMap.put(WebContext.USER_SESSION_ATTRIBUTE_NAME, user);
		UxUi currentSelection = UxUi.newPrimeFaces("external", "editorial", "skyve", "blue");
		Map<String, Object> requestMap = new HashMap<>();
		requestMap.put(AbstractWebContext.UXUI_SESSION_ATTRIBUTE_NAME, currentSelection);
		installContext(sessionMap, requestMap);

		HarnessView view = new HarnessView() {
			private static final long serialVersionUID = 1L;
		};
		view.setUxUi("desktop");

		assertEquals("desktop", sessionMap.get(AbstractWebContext.UXUI_SESSION_ATTRIBUTE_NAME));
		assertSame(currentSelection, requestMap.get(AbstractWebContext.UXUI_SESSION_ATTRIBUTE_NAME));
		assertEquals(1, requestMap.size());
	}

	@Test
	void setUserRetrievesTheCustomerQualifiedPrincipalAndLeavesTheSessionAloneWhenThereIsNoSuchUser() {
		ProvidedRepository originalRepository = ProvidedRepositoryFactory.get();
		ProvidedRepository repository = mock(ProvidedRepository.class);
		FacesContext facesContext = mock(FacesContext.class);
		ExternalContext externalContext = mock(ExternalContext.class);
		HttpServletRequest request = mock(HttpServletRequest.class);
		HttpSession session = mock(HttpSession.class);
		when(facesContext.getExternalContext()).thenReturn(externalContext);
		when(externalContext.getRequest()).thenReturn(request);
		when(request.getSession(true)).thenReturn(session);
		FacesContextBridge.setCurrent(facesContext);
		HarnessView view = new HarnessView() {
			private static final long serialVersionUID = 1L;
		};

		ProvidedRepositoryFactory.set(repository);
		try {
			view.setUser("demo", "admin");
		}
		finally {
			ProvidedRepositoryFactory.set(originalRepository);
		}

		verify(repository).retrieveUser("demo/admin");
		verifyNoInteractions(session);
	}

	private static void installContext(Map<String, Object> sessionMap, Map<String, Object> requestMap) {
		FacesContext facesContext = mock(FacesContext.class);
		ExternalContext externalContext = mock(ExternalContext.class);
		when(facesContext.getExternalContext()).thenReturn(externalContext);
		when(externalContext.getSessionMap()).thenReturn(sessionMap);
		when(externalContext.getRequestMap()).thenReturn(requestMap);
		FacesContextBridge.setCurrent(facesContext);
	}
}
