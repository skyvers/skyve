package org.skyve.impl.web.faces.components;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.contains;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.primefaces.component.outputpanel.OutputPanel;
import org.skyve.impl.web.faces.FacesUtil;
import org.skyve.impl.web.faces.views.MenuView;

import jakarta.el.ELContext;
import jakarta.el.ELResolver;
import jakarta.el.ExpressionFactory;
import jakarta.el.ValueExpression;
import jakarta.faces.FacesException;
import jakarta.faces.application.Application;
import jakarta.faces.component.UIOutput;
import jakarta.faces.component.UIViewRoot;
import jakarta.faces.context.ExternalContext;
import jakarta.faces.context.FacesContext;
import jakarta.servlet.http.Cookie;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

// Cookie flags, shared test state, repeated values and assertions are deliberate JSF fixtures.
@SuppressWarnings({ "static-method", "java:S1192", "java:S2092", "java:S2696", "java:S3330", "java:S5960" })
class FacesUtilityComponentsTest {
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
	void resetMenuStateExposesExpectedComponentFamily() {
		ResetMenuState component = new ResetMenuState();

		assertEquals("resetMenuState", component.getFamily());
	}

	@Test
	void resetMenuStateResetsMenuBeanAndDeletesMenuCookie() throws Exception {
		ResetMenuState component = new ResetMenuState();
		FacesContext facesContext = mock(FacesContext.class);
		ELContext elContext = mock(ELContext.class);
		ELResolver elResolver = mock(ELResolver.class);
		MenuView menuView = mock(MenuView.class);
		ExternalContext externalContext = mock(ExternalContext.class);
		HttpServletRequest request = mock(HttpServletRequest.class);
		HttpServletResponse response = mock(HttpServletResponse.class);

		Cookie menuCookie = new Cookie("panelMenu-leftMenu", "abc");
		Cookie otherCookie = new Cookie("other", "xyz");

		when(facesContext.getELContext()).thenReturn(elContext);
		when(elContext.getELResolver()).thenReturn(elResolver);
		when(elResolver.getValue(elContext, null, "menu")).thenReturn(menuView);
		when(facesContext.getExternalContext()).thenReturn(externalContext);
		when(externalContext.getRequest()).thenReturn(request);
		when(externalContext.getResponse()).thenReturn(response);
		when(request.getCookies()).thenReturn(new Cookie[] {menuCookie, otherCookie});

		component.encodeBegin(facesContext);

		verify(menuView).resetState();
		ArgumentCaptor<Cookie> cookieCaptor = ArgumentCaptor.forClass(Cookie.class);
		verify(response).addCookie(cookieCaptor.capture());
		assertEquals("panelMenu-leftMenu", cookieCaptor.getValue().getName());
		assertEquals(0, cookieCaptor.getValue().getMaxAge());
	}

	@Test
	void resetMenuStateWithFacesTraceEnabledStillDeletesCookies() throws Exception {
		ResetMenuState component = new ResetMenuState();
		FacesContext facesContext = mock(FacesContext.class);
		ELContext elContext = mock(ELContext.class);
		ELResolver elResolver = mock(ELResolver.class);
		MenuView menuView = mock(MenuView.class);
		ExternalContext externalContext = mock(ExternalContext.class);
		HttpServletRequest request = mock(HttpServletRequest.class);
		HttpServletResponse response = mock(HttpServletResponse.class);

		when(facesContext.getELContext()).thenReturn(elContext);
		when(elContext.getELResolver()).thenReturn(elResolver);
		when(elResolver.getValue(elContext, null, "menu")).thenReturn(menuView);
		when(facesContext.getExternalContext()).thenReturn(externalContext);
		when(externalContext.getRequest()).thenReturn(request);
		when(externalContext.getResponse()).thenReturn(response);
		when(request.getCookies()).thenReturn(new Cookie[] {new Cookie("panelMenu-leftMenu", "abc")});

		boolean originalFacesTrace = org.skyve.impl.util.UtilImpl.FACES_TRACE;
		org.skyve.impl.util.UtilImpl.FACES_TRACE = true;
		try {
			component.encodeBegin(facesContext);
		}
		finally {
			org.skyve.impl.util.UtilImpl.FACES_TRACE = originalFacesTrace;
		}

		verify(menuView).resetState();
		verify(response).addCookie(org.mockito.ArgumentMatchers.any(Cookie.class));
	}

	@Test
	void conversationStoresManagedBeanNameOnCurrentViewRoot() throws Exception {
		Conversation component = new Conversation();
		component.getAttributes().put("managedBean", "view");
		FacesContext facesContext = mock(FacesContext.class);
		UIViewRoot viewRoot = mock(UIViewRoot.class);

		when(facesContext.getViewRoot()).thenReturn(viewRoot);
		when(viewRoot.getAttributes()).thenReturn(new java.util.HashMap<>());
		FacesContextBridge.setCurrent(facesContext);

		component.encodeBegin(facesContext);

		assertEquals("conversation", component.getFamily());
		assertEquals("view", viewRoot.getAttributes().get(FacesUtil.MANAGED_BEAN_NAME_KEY));
	}

	@Test
	void conversationWithFacesTraceEnabledStillStoresManagedBeanName() throws Exception {
		Conversation component = new Conversation();
		component.getAttributes().put("managedBean", "view");
		FacesContext facesContext = mock(FacesContext.class);
		UIViewRoot viewRoot = mock(UIViewRoot.class);

		when(facesContext.getViewRoot()).thenReturn(viewRoot);
		when(viewRoot.getAttributes()).thenReturn(new java.util.HashMap<>());
		FacesContextBridge.setCurrent(facesContext);

		boolean originalFacesTrace = org.skyve.impl.util.UtilImpl.FACES_TRACE;
		org.skyve.impl.util.UtilImpl.FACES_TRACE = true;
		try {
			component.encodeBegin(facesContext);
		}
		finally {
			org.skyve.impl.util.UtilImpl.FACES_TRACE = originalFacesTrace;
		}

		assertEquals("view", viewRoot.getAttributes().get(FacesUtil.MANAGED_BEAN_NAME_KEY));
	}

	@Test
	void csrfFormDefaultsToNonNamingContainerAndRequiresManagedBeanAttribute() {
		CSRFForm form = new CSRFForm();
		FacesContext facesContext = mock(FacesContext.class);
		Application application = mock(Application.class);
		ExpressionFactory expressionFactory = mock(ExpressionFactory.class);
		ELContext elContext = mock(ELContext.class);

		form.setId("f1");
		when(facesContext.getApplication()).thenReturn(application);
		when(application.getExpressionFactory()).thenReturn(expressionFactory);
		when(facesContext.getELContext()).thenReturn(elContext);
		FacesContextBridge.setCurrent(facesContext);

		assertFalse(form.isPrependId());
		assertThrows(FacesException.class, () -> form.encodeBegin(facesContext));
	}

	@Test
	void csrfFormBuildsHiddenTokenPanelWhenManagedBeanIsConfigured() throws Exception {
		CSRFForm form = new CSRFForm();
		FacesContext facesContext = mock(FacesContext.class);
		Application application = mock(Application.class);
		ExpressionFactory expressionFactory = mock(ExpressionFactory.class);
		ELContext elContext = mock(ELContext.class);
		ValueExpression valueExpression = mock(ValueExpression.class);

		form.setId("f2");
		form.getAttributes().put("managedBean", "view");
		when(facesContext.getApplication()).thenReturn(application);
		when(application.getExpressionFactory()).thenReturn(expressionFactory);
		when(facesContext.getELContext()).thenReturn(elContext);
		when(application.createComponent(OutputPanel.COMPONENT_TYPE)).thenReturn(new OutputPanel());
		when(application.createComponent(UIOutput.COMPONENT_TYPE)).thenReturn(new UIOutput());
		when(expressionFactory.createValueExpression(eq(elContext), contains("csrfToken"), eq(String.class))).thenReturn(valueExpression);
		FacesContextBridge.setCurrent(facesContext);

		try {
			form.encodeBegin(facesContext);
		}
		catch (RuntimeException e) {
			e.getClass();
			// super.encodeBegin() may require a renderer-backed Faces runtime; panel construction has already occurred.
		}

		OutputPanel panel = (OutputPanel) form.getChildren().get(0);
		assertEquals("f2_csrfToken", panel.getId());
		assertFalse(panel.getChildren().isEmpty());
	}

	@Test
	void csrfFormSkipsPanelCreationWhenOutputPanelAlreadyPresent() throws Exception {
		CSRFForm form = new CSRFForm();
		FacesContext facesContext = mock(FacesContext.class);
		Application application = mock(Application.class);

		form.setId("f3");
		form.getChildren().add(new OutputPanel());
		when(facesContext.getApplication()).thenReturn(application);
		FacesContextBridge.setCurrent(facesContext);

		try {
			form.encodeBegin(facesContext);
		}
		catch (RuntimeException e) {
			e.getClass();
			// super.encodeBegin() may require a renderer-backed Faces runtime.
		}

		verify(application, never()).createComponent(OutputPanel.COMPONENT_TYPE);
		verify(application, never()).createComponent(UIOutput.COMPONENT_TYPE);
		assertEquals(1, form.getChildren().size());
	}

	@Test
	void viewWrapsInvalidComponentBuilderClassAsIOException() {
		View component = new View();
		component.getAttributes().put(org.skyve.impl.web.faces.pipeline.component.ComponentBuilder.COMPONENT_BUILDER_CLASS_KEY,
										"not.a.RealBuilder");
		FacesContext facesContext = mock(FacesContext.class);

		java.io.IOException thrown = assertThrows(java.io.IOException.class, () -> component.encodeBegin(facesContext));

		assertTrue(thrown.getMessage().contains("Cannot instantiate the component builder"));
	}

	@Test
	void viewWrapsInvalidLayoutBuilderClassAsIOException() {
		View component = new View();
		component.getAttributes().put(org.skyve.impl.web.faces.pipeline.component.ComponentBuilder.COMPONENT_BUILDER_CLASS_KEY,
										org.skyve.impl.web.faces.pipeline.component.NoOpComponentBuilder.class.getName());
		component.getAttributes().put(org.skyve.impl.web.faces.pipeline.layout.LayoutBuilder.LAYOUT_BUILDER_CLASS_KEY,
										"not.a.RealLayout");
		FacesContext facesContext = mock(FacesContext.class);

		assertThrows(java.io.IOException.class, () -> component.encodeBegin(facesContext));
	}
}
