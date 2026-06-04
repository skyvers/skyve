package org.skyve.impl.web.faces.views;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.lang.reflect.Method;
import java.util.Locale;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;

import jakarta.faces.context.ExternalContext;
import jakarta.faces.context.FacesContext;

@SuppressWarnings("static-method")
class SwaggerViewTest {
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
	void postConstructInitialisesInheritedLocalisationState() throws Exception {
		SwaggerView view = new SwaggerView();
		FacesContext context = mock(FacesContext.class);
		ExternalContext externalContext = mock(ExternalContext.class);

		when(context.getExternalContext()).thenReturn(externalContext);
		when(externalContext.getRequestLocale()).thenReturn(Locale.forLanguageTag("he"));
		FacesContextBridge.setCurrent(context);

		Method postConstruct = SwaggerView.class.getDeclaredMethod("postConstruct");
		postConstruct.setAccessible(true);
		postConstruct.invoke(view);

		assertEquals("rtl", view.getDir());
		assertEquals("he", view.getLanguageTag());
	}
}
