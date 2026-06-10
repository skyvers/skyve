package org.skyve.impl.report.jasperreports;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.skyve.domain.messages.DomainException;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;
import org.skyve.metadata.user.User;
import org.skyve.metadata.user.UserAccess;
import org.skyve.persistence.DynamicPersistence;

@SuppressWarnings({"static-method", "unchecked"})
class JasperReportUtilQueryListModelTest {
	private final Class<? extends DynamicPersistence> originalDynamicImplementation = AbstractPersistence.DYNAMIC_IMPLEMENTATION_CLASS;

	@AfterEach
	void tearDown() throws Exception {
		AbstractPersistence.DYNAMIC_IMPLEMENTATION_CLASS = originalDynamicImplementation;
		unbindPersistenceFromThread();
	}

	@Test
	void getQueryListModelUsesDocumentDefaultQueryWhenMetadataQueryIsMissing() throws Exception {
		Fixture fixture = fixture("Order");
		when(fixture.module.getMetaDataQuery("Order")).thenReturn(null);
		when(fixture.module.getDocumentDefaultQuery(fixture.customer, "Order")).thenReturn(fixture.query);
		AbstractPersistence.DYNAMIC_IMPLEMENTATION_CLASS = null;
		bindPersistenceWithUser(fixture.user);

		DomainException thrown = assertThrows(DomainException.class,
				() -> JasperReportUtil.getQueryListModel(fixture.module, "Order", "desktop"));

		assertThat(thrown.getMessage(), containsString("Cannot create new list model for dynamic persistence implementation"));
		verify(fixture.module).getDocumentDefaultQuery(fixture.customer, "Order");
		verify(fixture.user).canAccess(any(UserAccess.class), eq("desktop"));
		verify(fixture.user).canReadDocument(fixture.document);
	}

	@Test
	void getQueryListModelUsesNamedMetadataQueryWhenPresent() throws Exception {
		Fixture fixture = fixture("Order");
		when(fixture.module.getMetaDataQuery("OpenOrders")).thenReturn(fixture.query);
		AbstractPersistence.DYNAMIC_IMPLEMENTATION_CLASS = null;
		bindPersistenceWithUser(fixture.user);

		DomainException thrown = assertThrows(DomainException.class,
				() -> JasperReportUtil.getQueryListModel(fixture.module, "OpenOrders", "desktop"));

		assertThat(thrown.getMessage(), containsString("Cannot create new list model for dynamic persistence implementation"));
		verify(fixture.module, never()).getDocumentDefaultQuery(any(Customer.class), any(String.class));
		verify(fixture.user).canAccess(any(UserAccess.class), eq("desktop"));
		verify(fixture.user).canReadDocument(fixture.document);
	}

	@SuppressWarnings("boxing")
	private static Fixture fixture(String documentName) {
		Customer customer = mock(Customer.class);
		User user = mock(User.class);
		Module module = mock(Module.class);
		MetaDataQueryDefinition query = mock(MetaDataQueryDefinition.class);
		Document document = mock(Document.class);

		when(user.getCustomer()).thenReturn(customer);
		when(user.canAccess(any(UserAccess.class), any(String.class))).thenReturn(true);
		when(user.canReadDocument(document)).thenReturn(true);
		when(customer.getName()).thenReturn("demo");
		when(customer.getModule("sales")).thenReturn(module);
		when(module.getName()).thenReturn("sales");
		when(module.getDocument(customer, documentName)).thenReturn(document);
		when(query.getDocumentName()).thenReturn(documentName);
		when(query.getDocumentModule(customer)).thenReturn(module);
		when(document.isDynamic()).thenReturn(true);

		return new Fixture(customer, user, module, query, document);
	}

	private static void bindPersistenceWithUser(User user) throws Exception {
		AbstractPersistence persistence = mock(AbstractPersistence.class);
		when(persistence.getUser()).thenReturn(user);
		Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
		field.setAccessible(true);
		ThreadLocal<AbstractPersistence> threadLocal = (ThreadLocal<AbstractPersistence>) field.get(null);
		threadLocal.set(persistence);
	}

	private static void unbindPersistenceFromThread() throws Exception {
		Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
		field.setAccessible(true);
		ThreadLocal<AbstractPersistence> threadLocal = (ThreadLocal<AbstractPersistence>) field.get(null);
		threadLocal.remove();
	}

	private record Fixture(Customer customer, User user, Module module, MetaDataQueryDefinition query, Document document) {
		// fixture tuple
	}
}
