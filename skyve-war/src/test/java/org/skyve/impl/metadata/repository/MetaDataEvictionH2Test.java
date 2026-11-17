package org.skyve.impl.metadata.repository;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.Test;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.repository.ProvidedRepository;
import org.skyve.metadata.view.View;
import org.skyve.metadata.view.View.ViewType;

import modules.admin.domain.Contact;
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
