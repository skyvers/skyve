package org.skyve.impl.metadata.repository;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertSame;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.model.document.field.Enumeration;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.repository.ProvidedRepository;
import org.skyve.metadata.view.View;
import org.skyve.metadata.view.View.ViewType;

import modules.admin.Contact.ContactExtension;
import modules.admin.domain.Contact;
import modules.admin.domain.Contact.ContactType;
import util.AbstractH2TestDispose;

/**
 * Evicts the application's cached metadata, as the Control Panel's Evict From Cache action does, and
 * checks that the metadata still resolves afterwards. A full eviction disposes every persistence,
 * so these tests dispose theirs afterwards.
 */
@SuppressWarnings("static-method")
class MetaDataEvictionH2Test extends AbstractH2TestDispose {
	private static final String FACTORY_KEY = ProvidedRepository.MODULES_NAMESPACE + Contact.MODULE_NAME + '/' +
												Contact.DOCUMENT_NAME + '/' + Contact.DOCUMENT_NAME + "Factory";

	@Test
	void customerEvictionLeavesTheMetaDataResolvable() {
		ProvidedRepository repository = ProvidedRepositoryFactory.getNullSafe();
		assertMetaDataResolves(repository);

		repository.evictCachedMetaData(repository.getCustomer(CUSTOMER));

		assertMetaDataResolves(repository);
	}

	@Test
	void fullEvictionLeavesTheMetaDataResolvable() {
		ProvidedRepository repository = ProvidedRepositoryFactory.getNullSafe();
		assertMetaDataResolves(repository);

		repository.evictCachedMetaData(null);

		assertMetaDataResolves(repository);
	}

	/**
	 * A document caches its bean classes, which go with it when eviction loads the document again.
	 */
	@Test
	void customerEvictionLoadsTheDocumentAgainWithoutItsCachedBeanClasses() throws Exception {
		ProvidedRepository repository = ProvidedRepositoryFactory.getNullSafe();
		Customer customer = repository.getCustomer(CUSTOMER);
		assertNotNull(customer, "customer");
		DocumentImpl before = contact(repository, customer);
		Class<?> beanClass = before.getBeanClass(customer);

		repository.evictCachedMetaData(customer);

		Customer evictedCustomer = repository.getCustomer(CUSTOMER);
		assertNotNull(evictedCustomer, "customer");
		DocumentImpl after = contact(repository, evictedCustomer);
		assertAll(() -> assertSame(ContactExtension.class, beanClass),
					() -> assertNotSame(before, after),
					() -> assertSame(ContactExtension.class, after.getBeanClass(evictedCustomer)));
	}

	/**
	 * An enumeration caches its enum classes, which go with its document when eviction loads it again.
	 */
	@Test
	void customerEvictionLoadsTheEnumerationsAgainWithoutTheirCachedEnumClasses() {
		ProvidedRepository repository = ProvidedRepositoryFactory.getNullSafe();
		Customer customer = repository.getCustomer(CUSTOMER);
		assertNotNull(customer, "customer");
		Enumeration before = (Enumeration) contact(repository, customer).getAttribute(Contact.contactTypePropertyName);
		assertNotNull(before, "before");
		Class<?> enumClass = before.getImplementingType(customer);

		repository.evictCachedMetaData(customer);

		Customer evictedCustomer = repository.getCustomer(CUSTOMER);
		assertNotNull(evictedCustomer, "customer");
		Enumeration after = (Enumeration) contact(repository, evictedCustomer).getAttribute(Contact.contactTypePropertyName);
		assertNotNull(after, "after");
		assertAll(() -> assertSame(ContactType.class, enumClass),
					() -> assertNotSame(before, after),
					() -> assertSame(ContactType.class, after.getImplementingType(evictedCustomer)));
	}

	private static DocumentImpl contact(ProvidedRepository repository, Customer customer) {
		Module module = repository.getModule(customer, Contact.MODULE_NAME);
		assertNotNull(module, "module");
		return (DocumentImpl) repository.getDocument(customer, module, Contact.DOCUMENT_NAME);
	}

	/**
	 * Resolves the customer, a module, document and view of it, and the vtable key of the
	 * document's factory class.
	 */
	private static void assertMetaDataResolves(ProvidedRepository repository) {
		Customer customer = repository.getCustomer(CUSTOMER);
		assertNotNull(customer, "customer");
		Module module = repository.getModule(customer, Contact.MODULE_NAME);
		assertNotNull(module, "module");
		Document document = repository.getDocument(customer, module, Contact.DOCUMENT_NAME);
		assertNotNull(document, "document");
		View view = repository.getView(null, customer, document, ViewType.edit.toString());
		assertAll(() -> assertNotNull(view, "view"),
					() -> assertEquals(FACTORY_KEY, repository.vtable(CUSTOMER, FACTORY_KEY), "vtable"));
	}
}
