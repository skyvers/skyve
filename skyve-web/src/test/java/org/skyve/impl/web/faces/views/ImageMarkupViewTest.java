package org.skyve.impl.web.faces.views;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.skyve.web.WebContext;

import jakarta.faces.context.ExternalContext;
import jakarta.faces.context.FacesContext;
import jakarta.servlet.http.HttpSession;

@SuppressWarnings("static-method")
class ImageMarkupViewTest {
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
	void postConstructDeniesAccessWhenSessionIsMissing() {
		FacesContext context = mockFacesContextWithSession(null);
		FacesContextBridge.setCurrent(context);
		ImageMarkupView view = new ImageMarkupView();

		view.postConstruct();

		assertFalse(view.isCanAccess());
	}

	@Test
	void postConstructAllowsAccessWhenUserSessionAttributeIsPresent() {
		HttpSession session = mock(HttpSession.class);
		when(session.getAttribute(WebContext.USER_SESSION_ATTRIBUTE_NAME)).thenReturn(new Object());
		FacesContext context = mockFacesContextWithSession(session);
		FacesContextBridge.setCurrent(context);
		ImageMarkupView view = new ImageMarkupView();

		view.postConstruct();

		assertTrue(view.isCanAccess());
	}

	@Test
	void parameterSettersTrimBlankValuesToNull() {
		ImageMarkupView view = new ImageMarkupView();

		view.setContextParameter(" ");
		view.setBindingParameter("");
		view.setContentBindingParameter("\t");
		view.setContentIdParameter(null);

		assertNull(view.getContextParameter());
		assertNull(view.getBindingParameter());
		assertNull(view.getContentBindingParameter());
		assertNull(view.getContentIdParameter());
	}

	@Test
	void parameterSettersExposeSanitisedTextValues() {
		ImageMarkupView view = new ImageMarkupView();

		view.setContextParameter(" ctx ");
		view.setBindingParameter("bean.child");
		view.setContentBindingParameter("attachment.contentId");
		view.setContentIdParameter("content-123");

		assertEquals("ctx", view.getContextParameter());
		assertEquals("bean.child", view.getBindingParameter());
		assertEquals("attachment.contentId", view.getContentBindingParameter());
		assertEquals("content-123", view.getContentIdParameter());
	}

	@Test
	void simpleStateAccessorsReflectDefaultsAndAssignedValues() throws Exception {
		ImageMarkupView view = new ImageMarkupView();

		assertEquals(800, view.getImageWidth());
		assertEquals(600, view.getImageHeight());
		assertNull(view.getModuleDocument());
		assertNull(view.getNewContentId());
		assertNull(view.getSvg());

		view.setNewContentId("new-content");
		view.setSvg("<svg/>");
		setField(view, "moduleDocument", "admin.Document");

		assertEquals("admin.Document", view.getModuleDocument());
		assertEquals("new-content", view.getNewContentId());
		assertEquals("<svg/>", view.getSvg());
	}

	@Test
	void backgroundUrlIncludesContentDocumentAndUnsanitisedBinding() throws Exception {
		ImageMarkupView view = new ImageMarkupView();
		view.setContentIdParameter("content-123");
		view.setContentBindingParameter("attachment.contentId");
		setField(view, "moduleDocument", "admin.Document");

		String backgroundUrl = view.getBackgroundUrl();

		assertTrue(backgroundUrl.contains("content?_nm&_n=content-123"));
		assertTrue(backgroundUrl.contains("&_doc=admin.Document"));
		assertTrue(backgroundUrl.endsWith("&_b=attachment.contentId"));
	}

	private static FacesContext mockFacesContextWithSession(HttpSession session) {
		FacesContext context = mock(FacesContext.class);
		ExternalContext externalContext = mock(ExternalContext.class);
		when(context.getExternalContext()).thenReturn(externalContext);
		when(externalContext.getSession(false)).thenReturn(session);
		return context;
	}

	private static void setField(Object target, String name, Object value) throws Exception {
		Field field = ImageMarkupView.class.getDeclaredField(name);
		field.setAccessible(true);
		field.set(target, value);
	}
}
