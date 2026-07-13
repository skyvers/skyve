package org.skyve.impl.web.faces.components;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.junit.jupiter.api.Test;
import org.primefaces.component.datatable.DataTable;
import org.skyve.domain.messages.DomainException;
import org.skyve.impl.web.faces.pipeline.component.ComponentBuilder;
import org.skyve.impl.web.faces.pipeline.component.ComponentBuilderChain;
import org.skyve.impl.web.faces.pipeline.component.NoOpComponentBuilder;

import jakarta.el.ELContext;
import jakarta.el.ExpressionFactory;
import jakarta.faces.application.Application;
import jakarta.faces.context.FacesContext;

@SuppressWarnings({"static-method", "boxing"})
class ListGridStaticTest {
	private abstract static class FacesContextBridge extends FacesContext {
		static void setCurrent(FacesContext facesContext) {
			setCurrentInstance(facesContext);
		}
	}

	@Test
	void getBooleanObjectAttributeHandlesNullLiteralAndEvaluatedValues() {
		assertTrue(ListGrid.getBooleanObjectAttribute(null));
		assertTrue(ListGrid.getBooleanObjectAttribute("true"));
		assertTrue(ListGrid.getBooleanObjectAttribute(Boolean.TRUE));
		assertFalse(ListGrid.getBooleanObjectAttribute("false"));
		assertFalse(ListGrid.getBooleanObjectAttribute(Boolean.FALSE));
		assertFalse(ListGrid.getBooleanObjectAttribute("unexpected"));
	}

	@Test
	void getBooleanAttributeHandlesLiteralAndEvaluatedValues() {
		assertTrue(ListGrid.getBooleanAttribute("true"));
		assertTrue(ListGrid.getBooleanAttribute(Boolean.TRUE));
		assertFalse(ListGrid.getBooleanAttribute(null));
		assertFalse(ListGrid.getBooleanAttribute("false"));
		assertFalse(ListGrid.getBooleanAttribute(Boolean.FALSE));
		assertFalse(ListGrid.getBooleanAttribute("unexpected"));
	}

	@Test
	void newComponentBuilderAliasPathsThrowDomainExceptionWithoutFacesContext() {
		assertThrows(DomainException.class, () -> ListGrid.newComponentBuilder(null));
		assertThrows(DomainException.class, () -> ListGrid.newComponentBuilder("faces"));
		assertThrows(DomainException.class, () -> ListGrid.newComponentBuilder("vue"));
		assertThrows(DomainException.class, () -> ListGrid.newComponentBuilder("java.lang.String"));
	}

	@Test
	void newComponentBuilderInstantiatesCustomBuilderClass() {
		FacesContext facesContext = mock(FacesContext.class);
		Application application = mock(Application.class);
		ExpressionFactory expressionFactory = mock(ExpressionFactory.class);
		ELContext elContext = mock(ELContext.class);
		when(facesContext.getApplication()).thenReturn(application);
		when(application.getExpressionFactory()).thenReturn(expressionFactory);
		when(facesContext.getELContext()).thenReturn(elContext);
		FacesContextBridge.setCurrent(facesContext);
		try {
			assertInstanceOf(NoOpComponentBuilder.class, ListGrid.newComponentBuilder(NoOpComponentBuilder.class.getName()));
		}
		finally {
			FacesContextBridge.setCurrent(null);
		}
	}

	@Test
	void newComponentBuilderDefaultAppliesListViewStickyHeader() throws ReflectiveOperationException {
		FacesContext facesContext = mock(FacesContext.class);
		Application application = mock(Application.class);
		ExpressionFactory expressionFactory = mock(ExpressionFactory.class);
		ELContext elContext = mock(ELContext.class);
		when(facesContext.getApplication()).thenReturn(application);
		when(application.getExpressionFactory()).thenReturn(expressionFactory);
		when(facesContext.getELContext()).thenReturn(elContext);
		FacesContextBridge.setCurrent(facesContext);
		try {
			ComponentBuilder builder = ListGrid.newComponentBuilder(null);
			DataTable dataTable = mock(DataTable.class);
			java.lang.reflect.Method listGridMethod = null;
			for (java.lang.reflect.Method method : ComponentBuilderChain.class.getMethods()) {
				if ("listGrid".equals(method.getName()) && (method.getParameterCount() == 9)) {
					listGridMethod = method;
					break;
				}
			}
			if (listGridMethod == null) {
				throw new AssertionError("listGrid method not found");
			}

			assertSame(dataTable, listGridMethod.invoke(builder, dataTable, "mod", "doc", "model", "uxui", null, null, null, Boolean.FALSE));
			verify(dataTable).setPaginator(true);
			verify(dataTable).setStickyHeader(true);
		verify(dataTable).setStickyTopAt(".layout-topbar,#header");
		}
		finally {
			FacesContextBridge.setCurrent(null);
		}
	}

	@Test
	void newComponentBuilderThrowsDomainExceptionForUnknownClass() {
		assertThrows(DomainException.class, () -> ListGrid.newComponentBuilder("org.skyve.DoesNotExist"));
	}
}
