package org.skyve.impl.metadata.repository;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.function.ThrowingSupplier;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.repository.ProvidedRepository;
import org.skyve.metadata.user.User;

@SuppressWarnings("static-method")
class DefaultRepositoryTest {
	private String previousAppsJarDirectory;

	@BeforeEach
	void setUpBasePath() throws Exception {
		previousAppsJarDirectory = UtilImpl.APPS_JAR_DIRECTORY;
		UtilImpl.APPS_JAR_DIRECTORY = System.getProperty("user.dir");
		resetAbsoluteBasePath();
	}

	@AfterEach
	void tearDownBasePath() throws Exception {
		UtilImpl.APPS_JAR_DIRECTORY = previousAppsJarDirectory;
		resetAbsoluteBasePath();
	}

	private static void resetAbsoluteBasePath() throws Exception {
		Field field = UtilImpl.class.getDeclaredField("absoluteBasePath");
		field.setAccessible(true);
		field.set(null, null);
	}

	@Test
	void getSessionRepositoryReturnsNullWhenNoDelegateHasBeenSet() {
		DefaultRepository repository = new DefaultRepository();
		User user = mock(User.class);
		when(user.getSessionId()).thenReturn("S1");

		assertNull(repository.getSessionRepository(user));
	}

	@Test
	void setAndGetSessionRepositoryForUserUsesSameDelegate() {
		DefaultRepository repository = new DefaultRepository();
		User user = mock(User.class);
		ProvidedRepository delegate = mock(ProvidedRepository.class);
		when(user.getSessionId()).thenReturn("S1");

		repository.setSessionRepository(user, delegate);

		assertSame(delegate, repository.getSessionRepository(user));
	}

	@Test
	void removeSessionRepositoryForUserClearsOnlyThatUsersDelegate() {
		DefaultRepository repository = new DefaultRepository();
		User userA = mock(User.class);
		User userB = mock(User.class);
		ProvidedRepository delegateA = mock(ProvidedRepository.class);
		ProvidedRepository delegateB = mock(ProvidedRepository.class);
		when(userA.getSessionId()).thenReturn("S1");
		when(userB.getSessionId()).thenReturn("S2");

		repository.setSessionRepository(userA, delegateA);
		repository.setSessionRepository(userB, delegateB);

		repository.removeSessionRepository(userA);

		assertNull(repository.getSessionRepository(userA));
		assertSame(delegateB, repository.getSessionRepository(userB));
	}

	@Test
	void setSessionRepositoryWithoutSessionIdThrows() {
		DefaultRepository repository = new DefaultRepository();
		User user = mock(User.class);
		ProvidedRepository delegate = mock(ProvidedRepository.class);
		when(user.getSessionId()).thenReturn(null);
		when(user.getName()).thenReturn("tester");

		IllegalStateException e = assertThrows(IllegalStateException.class, () -> repository.setSessionRepository(user, delegate));
		assertEquals("User tester does not belong to a session", e.getMessage());
	}

	@Test
	void removeSessionRepositoryWithoutSessionIdThrows() {
		DefaultRepository repository = new DefaultRepository();
		User user = mock(User.class);
		when(user.getSessionId()).thenReturn(null);
		when(user.getName()).thenReturn("tester");

		IllegalStateException e = assertThrows(IllegalStateException.class, () -> repository.removeSessionRepository(user));
		assertEquals("User tester does not belong to a session", e.getMessage());
	}

	@Test
	void getSessionRepositoryCurrentThreadVariantDoesNotThrowWithoutPersistence() {
		DefaultRepository repository = new DefaultRepository();

		ProvidedRepository sessionRepo = assertDoesNotThrow((ThrowingSupplier<ProvidedRepository>) repository::getSessionRepository);
		assertNull(sessionRepo);
	}

	@Test
	void setSessionRepositoryCurrentThreadVariantThrowsWhenNoPersistenceIsBound() {
		DefaultRepository repository = new DefaultRepository();
		ProvidedRepository delegate = mock(ProvidedRepository.class);

		IllegalStateException e = assertThrows(IllegalStateException.class, () -> repository.setSessionRepository(delegate));
		assertEquals("No persistence on this thread", e.getMessage());
	}

	@Test
	void removeSessionRepositoryCurrentThreadVariantThrowsWhenNoPersistenceIsBound() {
		DefaultRepository repository = new DefaultRepository();

		IllegalStateException e = assertThrows(IllegalStateException.class, repository::removeSessionRepository);
		assertEquals("No persistence on this thread", e.getMessage());
	}
}
