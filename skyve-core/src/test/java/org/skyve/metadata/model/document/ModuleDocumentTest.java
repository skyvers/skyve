package org.skyve.metadata.model.document;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.withSettings;

import java.lang.reflect.Field;

import java.nio.file.Path;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.skyve.impl.metadata.module.ModuleImpl;
import org.skyve.impl.metadata.repository.LocalDesignRepository;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.repository.ProvidedRepository;
import org.skyve.metadata.user.User;

/**
 * Tests for the default methods of {@link ModuleDocument}.
 */
@SuppressWarnings("static-method")
class ModuleDocumentTest {
	/** A compile-time document reference as the generated per-module document enums implement it. */
	private enum TestDocument implements ModuleDocument {
		CONTACT;

		@Override
		public String moduleName() {
			return "admin";
		}

		@Override
		public String documentName() {
			return "Contact";
		}
	}

	@AfterEach
	void clearPersistenceThreadLocal() throws Exception {
		Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
		field.setAccessible(true);
		((ThreadLocal<?>) field.get(null)).remove();
	}

	@Test
	void toDocumentWithCustomerResolvesTheDocumentThroughTheModule() {
		Customer customer = mock(Customer.class);
		Document document = mockDocument(customer);

		assertSame(document, TestDocument.CONTACT.toDocument(customer));
	}

	@Test
	void toDocumentThrowsWhenTheModuleHasNoSuchDocument() {
		Customer customer = mock(Customer.class);
		ModuleImpl module = new ModuleImpl();
		module.setName("admin");
		when(customer.getModule("admin")).thenReturn(module);
		ProvidedRepository previousRepository = ProvidedRepositoryFactory.get();
		ProvidedRepositoryFactory.set(mock(ProvidedRepository.class));
		try {
			MetaDataException e = assertThrows(MetaDataException.class, () -> TestDocument.CONTACT.toDocument(customer));
			assertEquals("Document Contact does not exist in module admin", e.getMessage());
		}
		finally {
			ProvidedRepositoryFactory.set(previousRepository);
		}
	}

	/** The repository refuses a document the module does not list, which must surface as MetaDataException. */
	@Test
	void toDocumentThrowsWhenTheModuleDoesNotListTheDocument(@TempDir Path tempDir) {
		Customer customer = mock(Customer.class);
		ModuleImpl module = new ModuleImpl();
		module.setName("admin");
		when(customer.getModule("admin")).thenReturn(module);
		ProvidedRepository previousRepository = ProvidedRepositoryFactory.get();
		ProvidedRepositoryFactory.set(new LocalDesignRepository(tempDir.toString()));
		try {
			MetaDataException e = assertThrows(MetaDataException.class, () -> TestDocument.CONTACT.toDocument(customer));
			assertEquals("Document Contact does not exist in module admin", e.getMessage());
		}
		finally {
			ProvidedRepositoryFactory.set(previousRepository);
		}
	}

	@Test
	void toDocumentResolvesThroughTheCurrentUsersCustomer() {
		Customer customer = bindCurrentCustomer();
		Document document = mockDocument(customer);

		assertSame(document, TestDocument.CONTACT.toDocument());
	}

	private static Document mockDocument(Customer customer) {
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		when(customer.getModule("admin")).thenReturn(module);
		when(module.getDocument(customer, "Contact")).thenReturn(document);
		return document;
	}

	/**
	 * Binds a persistence to the thread whose user has the returned mocked customer.
	 */
	private static Customer bindCurrentCustomer() {
		Customer customer = mock(Customer.class);
		User user = mock(User.class);
		when(user.getCustomer()).thenReturn(customer);
		AbstractPersistence persistence = mock(AbstractPersistence.class, withSettings().defaultAnswer(CALLS_REAL_METHODS));
		persistence.setUser(user);
		persistence.setForThread();
		return customer;
	}
}
