package org.skyve.impl.web.faces.components;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import org.junit.jupiter.api.Test;
import org.skyve.domain.messages.DomainException;
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
	void newComponentBuilderThrowsDomainExceptionForUnknownClass() {
		assertThrows(DomainException.class, () -> ListGrid.newComponentBuilder("org.skyve.DoesNotExist"));
	}
}
