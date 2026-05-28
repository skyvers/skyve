package org.skyve.impl.metadata.repository;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.when;

import java.nio.file.Path;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.mockito.MockedStatic;
import org.skyve.EXT;
import org.skyve.dataaccess.sql.SQLDataAccess;
import org.skyve.domain.Bean;
import org.skyve.impl.util.UtilImpl;
import org.skyve.persistence.SQL;

@Disabled("Until byte buddy can be uplifted to allow mockito-inline lib to work")
class LocalDataStoreRepositoryTest {
	private String savedCustomer;
	private MockedStatic<EXT> mockedExt;
	@TempDir
	private Path repositoryPath;

	@BeforeEach
	void beforeEach() {
		savedCustomer = UtilImpl.CUSTOMER;
		mockedExt = mockStatic(EXT.class);
	}

	@AfterEach
	void afterEach() {
		UtilImpl.CUSTOMER = savedCustomer;
		if (mockedExt != null) {
			mockedExt.close();
		}
	}

	@Test
	void retrievePublicUserNameReturnsNullWhenNoPublicUserConfigured() {
		UtilImpl.CUSTOMER = null;
		SQLDataAccess dataAccess = mock(SQLDataAccess.class);
		SQL sql = mock(SQL.class);
		when(dataAccess.newSQL(anyString())).thenReturn(sql);
		when(sql.putParameter(eq(Bean.CUSTOMER_NAME), eq("demo"), eq(false))).thenReturn(sql);
		when(sql.scalarResult(String.class)).thenReturn(null);
		mockedExt.when(EXT::newSQLDataAccess).thenReturn(dataAccess);

		LocalDataStoreRepository repository = new LocalDataStoreRepository(repositoryPath.toString());
		String result = assertDoesNotThrow(() -> repository.retrievePublicUserName("demo"));

		assertNull(result);
	}

	@Test
	void retrievePublicUserNameReturnsResultWhenFound() {
		UtilImpl.CUSTOMER = null;
		SQLDataAccess dataAccess = mock(SQLDataAccess.class);
		SQL sql = mock(SQL.class);
		when(dataAccess.newSQL(anyString())).thenReturn(sql);
		when(sql.putParameter(eq(Bean.CUSTOMER_NAME), eq("demo"), eq(false))).thenReturn(sql);
		when(sql.scalarResult(String.class)).thenReturn("publicUser");
		mockedExt.when(EXT::newSQLDataAccess).thenReturn(dataAccess);

		LocalDataStoreRepository repository = new LocalDataStoreRepository(repositoryPath.toString());
		String result = repository.retrievePublicUserName("demo");

		assertEquals("publicUser", result);
	}

	@Test
	void retrievePublicUserNameReturnsNullForUnexpectedExceptions() {
		UtilImpl.CUSTOMER = null;
		SQLDataAccess dataAccess = mock(SQLDataAccess.class);
		SQL sql = mock(SQL.class);
		when(dataAccess.newSQL(anyString())).thenReturn(sql);
		when(sql.putParameter(eq(Bean.CUSTOMER_NAME), eq("demo"), eq(false))).thenReturn(sql);
		when(sql.scalarResult(String.class)).thenThrow(new IllegalStateException("boom"));
		mockedExt.when(EXT::newSQLDataAccess).thenReturn(dataAccess);

		LocalDataStoreRepository repository = new LocalDataStoreRepository(repositoryPath.toString());
		String result = repository.retrievePublicUserName("demo");

		assertNull(result);
	}
}
