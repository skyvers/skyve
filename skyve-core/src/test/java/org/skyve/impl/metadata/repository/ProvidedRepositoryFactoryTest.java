package org.skyve.impl.metadata.repository;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;

public class ProvidedRepositoryFactoryTest {

	@Test
	@SuppressWarnings("static-method")
	public void setCustomerAndUserFromPrincipalWithSlash() {
		var user = ProvidedRepositoryFactory.setCustomerAndUserFromPrincipal("acme/mike");
		assertNotNull(user);
		assertThat(user.getCustomerName(), is("acme"));
		assertThat(user.getName(), is("mike"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void setCustomerAndUserFromPrincipalWithoutSlash() {
		var user = ProvidedRepositoryFactory.setCustomerAndUserFromPrincipal("mike");
		assertNotNull(user);
		assertThat(user.getName(), is("mike"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void setCustomerAndUserFromPrincipalNullReturnsNull() {
		var user = ProvidedRepositoryFactory.setCustomerAndUserFromPrincipal(null);
		assertNull(user);
	}
}
