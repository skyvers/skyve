package org.skyve.impl.web.faces;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import org.junit.jupiter.api.Test;

import jakarta.faces.FacesException;
import jakarta.faces.context.FacesContext;
import jakarta.faces.event.PhaseEvent;
import jakarta.faces.event.PhaseId;

@SuppressWarnings("static-method")
class SkyveFacesPhaseListenerTest {
	@Test
	void getPhaseIdReturnsAnyPhase() {
		SkyveFacesPhaseListener listener = new SkyveFacesPhaseListener();
		assertEquals(PhaseId.ANY_PHASE, listener.getPhaseId());
	}

	@Test
	void beforePhaseAcceptsNormalEvent() {
		SkyveFacesPhaseListener listener = new SkyveFacesPhaseListener();
		PhaseEvent event = mock(PhaseEvent.class);
		FacesContext facesContext = mock(FacesContext.class);

		when(event.getFacesContext()).thenReturn(facesContext);
		when(event.getPhaseId()).thenReturn(PhaseId.APPLY_REQUEST_VALUES);
		when(facesContext.getResponseComplete()).thenReturn(false);

		assertDoesNotThrow(() -> listener.beforePhase(event));
	}

	@Test
	void afterPhaseRestoreViewWrapsUnexpectedErrorsAsFacesException() {
		SkyveFacesPhaseListener listener = new SkyveFacesPhaseListener();
		PhaseEvent event = mock(PhaseEvent.class);
		FacesContext facesContext = mock(FacesContext.class);

		when(event.getPhaseId()).thenReturn(PhaseId.RESTORE_VIEW);
		when(event.getFacesContext()).thenReturn(facesContext);
		when(facesContext.getResponseComplete()).thenReturn(false);
		when(facesContext.getExternalContext()).thenReturn(null);

		assertThrows(FacesException.class, () -> listener.afterPhase(event));
	}

	@Test
	void afterPhaseUpdateModelValuesHandlesNullViewRoot() {
		SkyveFacesPhaseListener listener = new SkyveFacesPhaseListener();
		PhaseEvent event = mock(PhaseEvent.class);
		FacesContext facesContext = mock(FacesContext.class);

		when(event.getPhaseId()).thenReturn(PhaseId.UPDATE_MODEL_VALUES);
		when(event.getFacesContext()).thenReturn(facesContext);
		when(facesContext.getViewRoot()).thenReturn(null);
		when(facesContext.getResponseComplete()).thenReturn(false);

		assertDoesNotThrow(() -> listener.afterPhase(event));
	}
}
