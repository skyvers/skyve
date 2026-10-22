package org.skyve.impl.metadata.repository;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import org.skyve.CORE;
import org.skyve.metadata.repository.ProvidedRepository;

class ProvidedRepositoryFactoryTest {

	@Test
	@SuppressWarnings("static-method")
	void setCustomerAndUserFromPrincipalWithSlash() {
		var user = ProvidedRepositoryFactory.setCustomerAndUserFromPrincipal("acme/mike");
		assertNotNull(user);
		assertThat(user.getCustomerName(), is("acme"));
		assertThat(user.getName(), is("mike"));
	}

	@Test
	@SuppressWarnings("static-method")
	void setCustomerAndUserFromPrincipalWithoutSlash() {
		var user = ProvidedRepositoryFactory.setCustomerAndUserFromPrincipal("mike");
		assertNotNull(user);
		assertThat(user.getName(), is("mike"));
	}

	@Test
	@SuppressWarnings("static-method")
	void setCustomerAndUserFromPrincipalNullReturnsNull() {
		var user = ProvidedRepositoryFactory.setCustomerAndUserFromPrincipal(null);
		assertNull(user);
	}

	@Test
	@SuppressWarnings("static-method")
	void withoutARepositoryGetIsNullAndTheNullSafeAccessorsThrow() {
		ProvidedRepository previous = ProvidedRepositoryFactory.get();
		ProvidedRepositoryFactory.clear();
		try {
			assertFalse(ProvidedRepositoryFactory.isConfigured());
			assertNull(ProvidedRepositoryFactory.get());
			assertThrows(IllegalStateException.class, ProvidedRepositoryFactory::getNullSafe);
			assertThrows(IllegalStateException.class, CORE::getRepository);
		}
		finally {
			ProvidedRepositoryFactory.set(previous);
		}
	}

	@Test
	@SuppressWarnings("static-method")
	void withARepositoryEveryAccessorReturnsIt() {
		ProvidedRepository previous = ProvidedRepositoryFactory.get();
		ProvidedRepository repository = Mockito.mock(ProvidedRepository.class);
		ProvidedRepositoryFactory.set(repository);
		try {
			assertTrue(ProvidedRepositoryFactory.isConfigured());
			assertSame(repository, ProvidedRepositoryFactory.get());
			assertSame(repository, ProvidedRepositoryFactory.getNullSafe());
			assertSame(repository, CORE.getRepository());
		}
		finally {
			ProvidedRepositoryFactory.set(previous);
		}
	}
}
