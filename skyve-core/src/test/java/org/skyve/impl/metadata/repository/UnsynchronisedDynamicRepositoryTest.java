package org.skyve.impl.metadata.repository;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class UnsynchronisedDynamicRepositoryTest {

	private UnsynchronisedDynamicRepository repo;

	@BeforeEach
	void setUp() {
		repo = new UnsynchronisedDynamicRepository();
	}

	@Test
	void populateKeysDoesNotThrow() {
		assertDoesNotThrow(repo::populateKeys);
	}

	@Test
	void loadRouterReturnsNull() {
		assertThat(repo.loadRouter(), is(nullValue()));
	}

	@Test
	void routerLastModifiedMillisReturnsMinValue() {
		assertEquals(Long.MIN_VALUE, repo.routerLastModifiedMillis());
	}

	@Test
	void loadCustomerReturnsNull() {
		assertThat(repo.loadCustomer("any"), is(nullValue()));
	}

	@Test
	void customerLastModifiedMillisReturnsMinValue() {
		assertEquals(Long.MIN_VALUE, repo.customerLastModifiedMillis("any"));
	}

	@Test
	void loadModuleReturnsNull() {
		assertThat(repo.loadModule("any", "anyModule"), is(nullValue()));
	}

	@Test
	void moduleLastModifiedMillisReturnsMinValue() {
		assertEquals(Long.MIN_VALUE, repo.moduleLastModifiedMillis("any", "anyModule"));
	}

	@Test
	void loadDocumentReturnsNull() {
		assertThat(repo.loadDocument("any", "anyModule", "anyDoc"), is(nullValue()));
	}

	@Test
	void documentLastModifiedMillisReturnsMinValue() {
		assertEquals(Long.MIN_VALUE, repo.documentLastModifiedMillis("any", "anyModule", "anyDoc"));
	}

	@Test
	void loadViewReturnsNull() {
		assertThat(repo.loadView("any", "anyModule", "anyDoc", "desktop", "edit"), is(nullValue()));
	}

	@Test
	void viewLastModifiedMillisReturnsMinValue() {
		assertEquals(Long.MIN_VALUE, repo.viewLastModifiedMillis("any", "anyModule", "anyDoc", "desktop", "edit"));
	}

	@Test
	void loadMetaDataActionReturnsNull() {
		assertThat(repo.loadMetaDataAction("any", "anyModule", "anyDoc", "Save"), is(nullValue()));
	}

	@Test
	void metaDataActionLastModifiedMillisReturnsMinValue() {
		assertEquals(Long.MIN_VALUE, repo.metaDataActionLastModifiedMillis("any", "anyModule", "anyDoc", "Save"));
	}

	@Test
	void loadMetaDataBizletReturnsNull() {
		assertThat(repo.loadMetaDataBizlet("any", "anyModule", "anyDoc"), is(nullValue()));
	}

	@Test
	void metaDataBizletLastModifiedMillisReturnsMinValue() {
		assertEquals(Long.MIN_VALUE, repo.metaDataBizletLastModifiedMillis("any", "anyModule", "anyDoc"));
	}

	@Test
	void getAllCustomerNamesReturnsEmptyList() {
		assertNotNull(repo.getAllCustomerNames());
		assertTrue(repo.getAllCustomerNames().isEmpty());
	}

	@Test
	void getAllVanillaModuleNamesReturnsEmptyList() {
		assertNotNull(repo.getAllVanillaModuleNames());
		assertTrue(repo.getAllVanillaModuleNames().isEmpty());
	}

	@Test
	void getUseScaffoldedViewsReturnsFalse() {
		assertFalse(repo.getUseScaffoldedViews());
	}
}
