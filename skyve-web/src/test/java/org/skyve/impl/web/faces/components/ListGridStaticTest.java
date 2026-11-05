package org.skyve.impl.web.faces.components;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;

import org.junit.jupiter.api.Test;
import org.skyve.domain.Bean;
import org.skyve.domain.messages.DomainException;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.web.faces.pipeline.component.ComponentBuilder;
import org.skyve.impl.web.faces.pipeline.component.NoOpComponentBuilder;
import org.skyve.impl.web.faces.pipeline.component.VueListGridComponentBuilder;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.model.list.ListModel;

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
			assertInstanceOf(VueListGridComponentBuilder.class, ListGrid.newComponentBuilder("vue"));
		}
		finally {
			FacesContextBridge.setCurrent(null);
		}
	}

	@Test
	void newComponentBuilderThrowsDomainExceptionForUnknownClass() {
		assertThrows(DomainException.class, () -> ListGrid.newComponentBuilder("org.skyve.DoesNotExist"));
	}

	@Test
	void generateThrowsWhenTheComponentBuilderProducesNoListGrid() throws ReflectiveOperationException {
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		ListModel<Bean> model = mock(ListModel.class);
		when(user.getCustomer()).thenReturn(customer);
		when(customer.getModule("admin")).thenReturn(module);
		when(module.getDocument(customer, "Contact")).thenReturn(document);
		when(document.getListModel(customer, "contacts", true)).thenReturn(model);
		// The mocked builder produces no list grid component
		ComponentBuilder componentBuilder = mock(ComponentBuilder.class);
		AbstractPersistence persistence = mock(AbstractPersistence.class, CALLS_REAL_METHODS);
		persistence.setUser(user);
		persistence.setForThread();
		try {
			IllegalStateException e = assertThrows(IllegalStateException.class,
													() -> ListGrid.generate("admin",
																			"Contact",
																			null,
																			"contacts",
																			"desktop",
																			null,
																			false,
																			null,
																			false,
																			null,
																			null,
																			componentBuilder));
			assertTrue(e.getMessage().contains("produced no list grid component for contacts"), e.getMessage());
		}
		finally {
			clearThreadPersistence();
		}
	}

	@SuppressWarnings({"unchecked", "java:S3011"}) // Test cleanup must access the framework-owned thread-local
	private static void clearThreadPersistence() throws ReflectiveOperationException {
		Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
		field.setAccessible(true);
		((ThreadLocal<AbstractPersistence>) field.get(null)).remove();
	}
}
