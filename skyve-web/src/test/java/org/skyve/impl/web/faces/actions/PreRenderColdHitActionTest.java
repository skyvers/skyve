package org.skyve.impl.web.faces.actions;

import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.web.faces.views.FacesView;
import org.skyve.metadata.user.User;
import org.skyve.web.WebAction;

import jakarta.faces.context.FacesContext;

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
		bindPersistenceToThread(null);

		PreRenderColdHitAction action = new PreRenderColdHitAction(facesView);
		assertNull(action.callback());

		org.mockito.Mockito.verify(facesView, never()).initialise();
	}

	@Test
	void callbackInitialisesForNonEditAndNonListAction() throws Exception {
		FacesView facesView = mock(FacesView.class);
		when(facesView.getWebActionParameter()).thenReturn(WebAction.c);
		when(facesView.getBizModuleParameter()).thenReturn("admin");
		when(facesView.getBizDocumentParameter()).thenReturn("Contact");
		when(facesView.getQueryNameParameter()).thenReturn("q");
		when(facesView.getBizIdParameter()).thenReturn("id");
		bindPersistenceToThread(mock(User.class));

		PreRenderColdHitAction action = new PreRenderColdHitAction(facesView);
		assertNull(action.callback());

		org.mockito.Mockito.verify(facesView).initialise();
	}

	@Test
	void callbackDoesNotPopulateViewScopedSelectionState() throws Exception {
		FacesView facesView = mock(FacesView.class);
		bindPersistenceToThread(null);

		PreRenderColdHitAction action = new PreRenderColdHitAction(facesView);
		assertNull(action.callback());

		org.mockito.Mockito.verify(facesView, never()).initialise();
		org.mockito.Mockito.verifyNoMoreInteractions(facesView);
	}

	@SuppressWarnings({ "unchecked", "java:S112", "java:S3011" }) // Reflection installs private thread state for focused tests.
	private static void bindPersistenceToThread(User user) throws Exception {
		AbstractPersistence persistence = mock(AbstractPersistence.class);
		when(persistence.getUser()).thenReturn(user);
		Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
		field.setAccessible(true);
		((ThreadLocal<AbstractPersistence>) field.get(null)).set(persistence);
	}

	@SuppressWarnings({ "unchecked", "java:S112", "java:S3011" }) // Reflection removes private thread state after focused tests.
	private static void unbindPersistenceFromThread() throws Exception {
		Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
		field.setAccessible(true);
		((ThreadLocal<AbstractPersistence>) field.get(null)).remove();
	}
}
