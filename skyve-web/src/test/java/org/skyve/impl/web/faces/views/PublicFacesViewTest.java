package org.skyve.impl.web.faces.views;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;

import jakarta.faces.context.ExternalContext;
import jakarta.faces.context.FacesContext;

@SuppressWarnings("static-method")
class PublicFacesViewTest {
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
	void customerParameterGetterAndSetterRoundTrip() {
		PublicFacesView view = new PublicFacesView();

		view.setBizCustomerParameter("acme");

		assertEquals("acme", view.getBizCustomerParameter());
	}

	@Test
	void preRenderThrowsWhenCustomerParameterMissing() {
		PublicFacesView view = new PublicFacesView();
		FacesContext context = mock(FacesContext.class);
		ExternalContext externalContext = mock(ExternalContext.class);

		when(context.isPostback()).thenReturn(false);
		when(context.getExternalContext()).thenReturn(externalContext);
		when(externalContext.getUserPrincipal()).thenReturn(null);
		FacesContextBridge.setCurrent(context);

		IllegalStateException ex = assertThrows(IllegalStateException.class, view::preRender);
		assertEquals("Malformed URL - this URL must have a 'c' parameter", ex.getMessage());
	}
}
