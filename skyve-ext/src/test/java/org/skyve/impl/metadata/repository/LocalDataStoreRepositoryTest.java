package org.skyve.impl.metadata.repository;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Comparator;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.dataaccess.sql.SQLDataAccess;
import org.skyve.domain.Bean;
import org.skyve.domain.messages.NoResultsException;
import org.skyve.impl.util.UtilImpl;
import org.skyve.persistence.SQL;

class LocalDataStoreRepositoryTest {
	private String savedCustomer;
	private Path repositoryPath;

	@BeforeEach
	void beforeEach() throws Exception {
		savedCustomer = UtilImpl.CUSTOMER;
		repositoryPath = Files.createTempDirectory("local-data-store-repository-test");
	}

	@AfterEach
	void afterEach() throws Exception {
		UtilImpl.CUSTOMER = savedCustomer;
		if (repositoryPath != null) {
			try (var paths = Files.walk(repositoryPath)) {
				paths.sorted(Comparator.reverseOrder()).forEach(path -> path.toFile().delete());
			}
		}
	}

	@Test
	void retrievePublicUserNameReturnsNullWhenNoResultsExceptionOccurs() {
		UtilImpl.CUSTOMER = null;
		SQLDataAccess dataAccess = mock(SQLDataAccess.class);
		SQL sql = mock(SQL.class);
		when(dataAccess.newSQL(anyString())).thenReturn(sql);
		when(sql.putParameter(eq(Bean.CUSTOMER_NAME), eq("demo"), eq(false))).thenReturn(sql);
		when(sql.retrieveScalar(String.class)).thenThrow(new NoResultsException());

		TestableLocalDataStoreRepository repository = new TestableLocalDataStoreRepository(repositoryPath.toString(), dataAccess);
		String result = assertDoesNotThrow(() -> repository.retrievePublicUserName("demo"));

		assertNull(result);
		verify(sql).putParameter(Bean.CUSTOMER_NAME, "demo", false);
		assertNull(repository.lastLoggedException);
	}

	@Test
	void retrievePublicUserNameReturnsResultWhenFound() {
		UtilImpl.CUSTOMER = null;
		SQLDataAccess dataAccess = mock(SQLDataAccess.class);
		SQL sql = mock(SQL.class);
		when(dataAccess.newSQL(anyString())).thenReturn(sql);
		when(sql.putParameter(eq(Bean.CUSTOMER_NAME), eq("demo"), eq(false))).thenReturn(sql);
		when(sql.retrieveScalar(String.class)).thenReturn("publicUser");

		TestableLocalDataStoreRepository repository = new TestableLocalDataStoreRepository(repositoryPath.toString(), dataAccess);
		String result = repository.retrievePublicUserName("demo");

		assertEquals("publicUser", result);
		verify(sql).putParameter(Bean.CUSTOMER_NAME, "demo", false);
		assertNull(repository.lastLoggedException);
	}

	@Test
	void retrievePublicUserNameLogsWarningsForUnexpectedExceptions() {
		UtilImpl.CUSTOMER = null;
		SQLDataAccess dataAccess = mock(SQLDataAccess.class);
		SQL sql = mock(SQL.class);
		IllegalStateException boom = new IllegalStateException("boom");
		when(dataAccess.newSQL(anyString())).thenReturn(sql);
		when(sql.putParameter(eq(Bean.CUSTOMER_NAME), eq("demo"), eq(false))).thenReturn(sql);
		when(sql.retrieveScalar(String.class)).thenThrow(boom);

		TestableLocalDataStoreRepository repository = new TestableLocalDataStoreRepository(repositoryPath.toString(), dataAccess);
		String result = repository.retrievePublicUserName("demo");

		assertNull(result);
		assertEquals(boom, repository.lastLoggedException);
		assertEquals("demo", repository.lastLoggedCustomer);
	}

	private static final class TestableLocalDataStoreRepository extends LocalDataStoreRepository {
		private final SQLDataAccess dataAccess;
		private String lastLoggedCustomer;
		private Exception lastLoggedException;

		private TestableLocalDataStoreRepository(String absolutePath, SQLDataAccess dataAccess) {
			super(absolutePath);
			this.dataAccess = dataAccess;
		}

		@Override
		SQLDataAccess newSQLDataAccess() {
			return dataAccess;
		}

		@Override
		void logPublicUserLookupFailure(String customerName, Exception e) {
			lastLoggedCustomer = customerName;
			lastLoggedException = e;
		}
	}
}
