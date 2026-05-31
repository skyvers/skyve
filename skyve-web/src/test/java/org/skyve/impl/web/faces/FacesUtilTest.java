package org.skyve.impl.web.faces;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.HashMap;
import java.util.Map;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.skyve.domain.messages.DomainException;
import org.skyve.impl.sail.mock.MockFacesContext;

import jakarta.el.ELContext;
import jakarta.el.ELResolver;
import jakarta.el.ExpressionFactory;
import jakarta.el.MethodExpression;
import jakarta.el.ValueExpression;
import jakarta.faces.FacesException;
import jakarta.faces.application.Application;
import jakarta.faces.component.UIComponent;
import jakarta.faces.component.UIComponentBase;
import jakarta.faces.context.ExternalContext;
import jakarta.faces.context.FacesContext;
import jakarta.servlet.http.HttpServletRequest;

@SuppressWarnings("static-method")
class FacesUtilTest {
	private abstract static class FacesContextBridge extends FacesContext {
		static void setCurrent(FacesContext context) {
			setCurrentInstance(context);
		}
	}

	private static final class StyledComponent extends UIComponentBase {
		private String styleClass;

		public void setStyleClass(String styleClass) {
			this.styleClass = styleClass;
		}

		public String getStyleClass() {
			return styleClass;
		}

		@Override
		public String getFamily() {
			return "test";
		}
	}

	private static final class UnstyledComponent extends UIComponentBase {
		@Override
		public String getFamily() {
			return "test";
		}
	}

	@AfterEach
	void clearFacesContext() {
		FacesContextBridge.setCurrent(null);
	}

	@Test
	void xmlPartialRedirectEscapesAmpersands() {
		String xml = FacesUtil.xmlPartialRedirect("/go?a=1&b=2");

		assertTrue(xml.contains("<partial-response><redirect url=\"/go?a=1&amp;b=2\"/></partial-response>"));
	}

	@Test
	void isAjaxChecksRequestedWithHeader() {
		HttpServletRequest request = mock(HttpServletRequest.class);
		when(request.getHeader("X-Requested-With")).thenReturn("XMLHttpRequest");

		assertTrue(FacesUtil.isAjax(request));
	}

	@Test
	void getNamedReturnsResolvedBean() {
		FacesContext context = mock(FacesContext.class);
		ELContext elContext = mock(ELContext.class);
		ELResolver resolver = mock(ELResolver.class);

		when(context.getELContext()).thenReturn(elContext);
		when(elContext.getELResolver()).thenReturn(resolver);
		when(resolver.getValue(elContext, null, "view")).thenReturn("bean");
		FacesContextBridge.setCurrent(context);

		assertEquals("bean", FacesUtil.getNamed("view"));
	}

	@Test
	void getNamedThrowsWhenBeanIsMissing() {
		FacesContext context = mock(FacesContext.class);
		ELContext elContext = mock(ELContext.class);
		ELResolver resolver = mock(ELResolver.class);

		when(context.getELContext()).thenReturn(elContext);
		when(elContext.getELResolver()).thenReturn(resolver);
		when(resolver.getValue(elContext, null, "missing")).thenReturn(null);
		FacesContextBridge.setCurrent(context);

		assertThrows(FacesException.class, () -> FacesUtil.getNamed("missing"));
	}

	@Test
	void setAssignsValueViaExpressionFactory() {
		FacesContext context = mock(FacesContext.class);
		ELContext elContext = mock(ELContext.class);
		Application application = mock(Application.class);
		ExpressionFactory expressionFactory = mock(ExpressionFactory.class);
		ValueExpression valueExpression = mock(ValueExpression.class);

		when(context.getELContext()).thenReturn(elContext);
		when(context.getApplication()).thenReturn(application);
		when(application.getExpressionFactory()).thenReturn(expressionFactory);
		when(expressionFactory.createValueExpression(eq(elContext), eq("#{bean.value}"), eq(Object.class))).thenReturn(valueExpression);
		FacesContextBridge.setCurrent(context);

		FacesUtil.set("abc", "#{bean.value}");

		verify(valueExpression).setValue(elContext, "abc");
	}

	@Test
	void createMethodExpressionReturnsFactoryResult() {
		FacesContext context = mock(FacesContext.class);
		ELContext elContext = mock(ELContext.class);
		Application application = mock(Application.class);
		ExpressionFactory expressionFactory = mock(ExpressionFactory.class);
		MethodExpression methodExpression = mock(MethodExpression.class);

		when(context.getELContext()).thenReturn(elContext);
		when(context.getApplication()).thenReturn(application);
		when(application.getExpressionFactory()).thenReturn(expressionFactory);
		when(expressionFactory.createMethodExpression(eq(elContext), eq("#{bean.run}"), eq(Void.class), any(Class[].class))).thenReturn(methodExpression);
		FacesContextBridge.setCurrent(context);

		assertEquals(methodExpression,
				FacesUtil.createMethodExpression("#{bean.run}", Void.class, new Class<?>[] {String.class}));
	}

	@Test
	void createMethodExpressionThrowsFacesExceptionOnFactoryFailure() {
		FacesContext context = mock(FacesContext.class);
		Application application = mock(Application.class);

		when(context.getApplication()).thenReturn(application);
		when(application.getExpressionFactory()).thenThrow(new RuntimeException("boom"));
		FacesContextBridge.setCurrent(context);

		assertThrows(FacesException.class,
				() -> FacesUtil.createMethodExpression("#{bean.run}", Void.class, new Class<?>[0]));
	}

	@Test
	void isIgnoreAutoUpdateReadsRequestParameterMap() {
		FacesContext context = mock(FacesContext.class);
		ExternalContext externalContext = mock(ExternalContext.class);
		Map<String, String> params = new HashMap<>();

		params.put("primefaces.ignoreautoupdate", "true");
		when(context.getExternalContext()).thenReturn(externalContext);
		when(externalContext.getRequestParameterMap()).thenReturn(params);
		FacesContextBridge.setCurrent(context);

		assertTrue(FacesUtil.isIgnoreAutoUpdate());

		params.put("primefaces.ignoreautoupdate", "false");
		assertFalse(FacesUtil.isIgnoreAutoUpdate());
	}

	@Test
	void setStyleClassUsesReflectionWhenAvailable() {
		StyledComponent component = new StyledComponent();

		FacesUtil.setStyleCLass(component, "col-12");

		assertEquals("col-12", component.getStyleClass());
	}

	@Test
	void setStyleClassThrowsDomainExceptionWhenSetterMissing() {
		UIComponent component = new UnstyledComponent();

		assertThrows(DomainException.class, () -> FacesUtil.setStyleCLass(component, "x"));
	}

	@Test
	void setStyleClassAllowsNullComponent() {
		FacesUtil.setStyleCLass(null, "x");
	}

	@Test
	void isRealFacesContextDistinguishesMockAndRealContexts() {
		assertFalse(FacesUtil.isRealFacesContext());

		FacesContextBridge.setCurrent(new MockFacesContext());
		assertFalse(FacesUtil.isRealFacesContext());

		FacesContextBridge.setCurrent(mock(FacesContext.class));
		assertTrue(FacesUtil.isRealFacesContext());
	}

	@Test
	void sailFacesContextSetAndResetLifecycleIsSafe() {
		FacesContextBridge.setCurrent(null);

		FacesUtil.setSailFacesContextIfNeeded();
		assertTrue(FacesContext.getCurrentInstance() instanceof MockFacesContext);

		FacesUtil.setSailFacesContextIfNeeded();
		assertTrue(FacesContext.getCurrentInstance() instanceof MockFacesContext);

		FacesUtil.resetSailFacesContextIfNeeded();
		assertFalse(FacesContext.getCurrentInstance() instanceof MockFacesContext);

		FacesContextBridge.setCurrent(mock(FacesContext.class));
		FacesUtil.resetSailFacesContextIfNeeded();
		assertTrue(FacesContext.getCurrentInstance() != null);
	}
}
