package org.skyve.impl.web.faces.pipeline.component;

import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.primefaces.component.datatable.DataTable;

import jakarta.el.ELContext;
import jakarta.el.ExpressionFactory;
import jakarta.faces.application.Application;
import jakarta.faces.context.FacesContext;

@SuppressWarnings("static-method")
class SkyveComponentBuilderChainTest {
	private abstract static class FacesContextBridge extends FacesContext {
		static void setCurrent(FacesContext facesContext) {
			setCurrentInstance(facesContext);
		}
	}

	@AfterEach
	void tearDownFacesContext() {
		FacesContextBridge.setCurrent(null);
	}

	@Test
	void constructorRequiresFacesContextInHeadlessUnitScope() {
		assertThrows(NullPointerException.class, SkyveComponentBuilderChain::new);
	}

	@Test
	void listGridAppliesPaginationOnly() throws ReflectiveOperationException {
		FacesContext facesContext = mock(FacesContext.class);
		Application application = mock(Application.class);
		ExpressionFactory expressionFactory = mock(ExpressionFactory.class);
		ELContext elContext = mock(ELContext.class);
		when(facesContext.getApplication()).thenReturn(application);
		when(facesContext.getELContext()).thenReturn(elContext);
		when(application.getExpressionFactory()).thenReturn(expressionFactory);
		FacesContextBridge.setCurrent(facesContext);

		SkyveComponentBuilderChain chain = new SkyveComponentBuilderChain();
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

		assertSame(dataTable, listGridMethod.invoke(chain, dataTable, "mod", "doc", "model", "uxui", null, null, null, Boolean.FALSE));
		verify(dataTable).setPaginator(true);
		verify(dataTable).setRowsPerPageTemplate("25,50,75,100");
		verify(dataTable).setPaginatorAlwaysVisible(false);
		verify(dataTable, never()).setStickyHeader(true);
	}
}
