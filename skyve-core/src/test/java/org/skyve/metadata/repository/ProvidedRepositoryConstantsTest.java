package org.skyve.metadata.repository;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class ProvidedRepositoryConstantsTest {
	@Test
	void constantsExposeExpectedRepositoryNamespaces() {
		assertEquals("router", ProvidedRepository.ROUTER_NAME);
		assertEquals("router/", ProvidedRepository.ROUTER_NAMESPACE);
		assertEquals("customers", ProvidedRepository.CUSTOMERS_NAME);
		assertEquals("customers/", ProvidedRepository.CUSTOMERS_NAMESPACE);
		assertEquals("resources/", ProvidedRepository.RESOURCES_NAMESPACE);
		assertEquals("modules", ProvidedRepository.MODULES_NAME);
		assertEquals("modules/", ProvidedRepository.MODULES_NAMESPACE);
		assertEquals("converters/", ProvidedRepository.CONVERTERS_NAMESPACE);
		assertEquals("views", ProvidedRepository.VIEWS_NAME);
		assertEquals("views/", ProvidedRepository.VIEWS_NAMESPACE);
		assertEquals("models", ProvidedRepository.MODELS_NAME);
		assertEquals("models/", ProvidedRepository.MODELS_NAMESPACE);
		assertEquals("actions", ProvidedRepository.ACTIONS_NAME);
		assertEquals("actions/", ProvidedRepository.ACTIONS_NAMESPACE);
		assertEquals("images", ProvidedRepository.IMAGES_NAME);
		assertEquals("images/", ProvidedRepository.IMAGES_NAMESPACE);
		assertEquals("reports", ProvidedRepository.REPORTS_NAME);
		assertEquals("reports/", ProvidedRepository.REPORTS_NAMESPACE);
		assertEquals("domain", ProvidedRepository.DOMAIN_NAME);
		assertEquals("domain/", ProvidedRepository.DOMAIN_NAMESPACE);

		assertTrue(ProvidedRepository.ACTIONS_NAMESPACE.endsWith("/"));
		assertTrue(ProvidedRepository.MODELS_NAMESPACE.endsWith("/"));
		assertTrue(ProvidedRepository.REPORTS_NAMESPACE.endsWith("/"));
	}

	@Test
	void constantsExposeExpectedSuffixes() {
		assertEquals("Bizlet", ProvidedRepository.BIZLET_SUFFIX);
		assertEquals("Jasper", ProvidedRepository.JASPER_SUFFIX);
		assertEquals("Freemarker", ProvidedRepository.FREEMARKER_SUFFIX);
		assertEquals("MetaData", ProvidedRepository.META_DATA_SUFFIX);
	}
}
