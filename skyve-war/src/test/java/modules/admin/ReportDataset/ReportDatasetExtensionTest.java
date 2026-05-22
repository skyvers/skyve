package modules.admin.ReportDataset;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.doReturn;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.mockito.Spy;
import org.skyve.domain.app.admin.ReportDataset.DatasetType;
import org.skyve.domain.messages.DomainException;

import modules.admin.ReportDataset.ReportDatasetExtension.SubstitutedQueryResult;
import modules.admin.ReportParameter.ReportParameterExtension;

class ReportDatasetExtensionTest {

	@Spy
	private ReportDatasetExtension bean;

	@SuppressWarnings("resource")
	private AutoCloseable closeable;

	@BeforeEach
	void openMocks() {
		@SuppressWarnings("resource")
		AutoCloseable mocks = MockitoAnnotations.openMocks(this);
		closeable = mocks;
	}

	@AfterEach
	void releaseMocks() throws Exception {
		closeable.close();
	}

	@Test
	void testGetSubstitutedQueryOneParameter() {
		// setup the test data
		String query = "SELECT * FROM ADM_SecurityUser where createdDateTime < {DATE}";

		// setup mocks
		Mockito.when(bean.getQuery()).thenReturn(query);

		// call the method under test
		SubstitutedQueryResult result = bean.getSubstitutedQuery();

		// verify the result
		assertThat(result, is(notNullValue()));
		assertThat(result.getQuery(), containsString(":d_"));
		assertThat(result.getQuery(), containsString("_1"));

		assertEquals(1, result.getParameters().size());
	}

	@Test
	void testGetSubstitutedQueryTwoParameters() {
		// setup the test data
		String query = "SELECT * FROM ADM_SecurityUser where createdDateTime BETWEEN {DATE} AND {DATE+1d}";

		// setup mocks
		Mockito.when(bean.getQuery()).thenReturn(query);

		// call the method under test
		SubstitutedQueryResult result = bean.getSubstitutedQuery();

		// verify the result
		assertThat(result, is(notNullValue()));
		assertThat(result.getQuery(), containsString(":d_"));
		assertThat(result.getQuery(), containsString("_1"));
		assertThat(result.getQuery(), containsString("_2"));

		assertEquals(2, result.getParameters().size());
	}

	@Test
	void testContainsParameterReturnsTrueWhenParameterInBizQLQuery() {
		doReturn(DatasetType.bizQL).when(bean).getDatasetType();
		doReturn("SELECT b FROM admin$User b WHERE b.name = :userName").when(bean).getQuery();

		ReportParameterExtension param = new ReportParameterExtension();
		param.setName("userName");

		assertTrue(bean.containsParameter(param));
	}

	@Test
	void testContainsParameterReturnsFalseWhenParameterNotInBizQLQuery() {
		doReturn(DatasetType.bizQL).when(bean).getDatasetType();
		doReturn("SELECT b FROM admin$User b WHERE b.active = true").when(bean).getQuery();

		ReportParameterExtension param = new ReportParameterExtension();
		param.setName("userName");

		assertFalse(bean.containsParameter(param));
	}

	@Test
	void testContainsParameterThrowsWhenParameterNullForBizQL() {
		doReturn(DatasetType.bizQL).when(bean).getDatasetType();
		doReturn("SELECT b FROM admin$User b").when(bean).getQuery();

		assertThrows(DomainException.class, () -> bean.containsParameter(null));
	}

	@Test
	void testContainsParameterReturnsTrueForNonQueryType() {
		doReturn(DatasetType.classValue).when(bean).getDatasetType();

		assertTrue(bean.containsParameter(null));
	}

	@Test
	void testContainsParameterReturnsTrueForSQLQueryContainingParameter() {
		doReturn(DatasetType.SQL).when(bean).getDatasetType();
		doReturn("SELECT * FROM ADM_User WHERE userName = :user").when(bean).getQuery();

		ReportParameterExtension param = new ReportParameterExtension();
		param.setName("user");

		assertTrue(bean.containsParameter(param));
	}

	@Test
	void testAddMissingParametersDoesNothingWhenNotTypeQuery() {
		doReturn(DatasetType.classValue).when(bean).getDatasetType();
		assertDoesNotThrow(() -> bean.addMissingParameters());
	}

	@Test
	void testAddMissingParametersReturnsWhenQueryIsNull() {
		doReturn(DatasetType.bizQL).when(bean).getDatasetType();
		doReturn(null).when(bean).getQuery();
		assertDoesNotThrow(() -> bean.addMissingParameters());
	}

	@Test
	void testAddMissingParametersReturnsWhenParentIsNull() {
		doReturn(DatasetType.bizQL).when(bean).getDatasetType();
		doReturn("SELECT b FROM admin$User b WHERE b.name = :param").when(bean).getQuery();
		doReturn(null).when(bean).getParent();
		assertDoesNotThrow(() -> bean.addMissingParameters());
	}

	@Test
	void testGetSubstitutedQueryForNonBizQLTypeReturnsOriginalQuery() {
		doReturn(DatasetType.SQL).when(bean).getDatasetType();
		doReturn("SELECT * FROM ADM_User").when(bean).getQuery();

		SubstitutedQueryResult result = bean.getSubstitutedQuery();

		assertThat(result, is(notNullValue()));
		assertThat(result.getQuery(), is("SELECT * FROM ADM_User"));
		assertTrue(result.getParameters().isEmpty());
	}

	@Test
	void testGetSubstitutedQueryForBizQLWithNoDateReturnsOriginalQuery() {
		doReturn(DatasetType.bizQL).when(bean).getDatasetType();
		String query = "SELECT b FROM admin$User b WHERE b.active = true";
		doReturn(query).when(bean).getQuery();

		SubstitutedQueryResult result = bean.getSubstitutedQuery();

		assertThat(result, is(notNullValue()));
		assertThat(result.getQuery(), is(query));
		assertTrue(result.getParameters().isEmpty());
	}

	@Test
	void testGetSubstitutedQueryWithMonthModifierReplacesDate() {
		doReturn(DatasetType.bizQL).when(bean).getDatasetType();
		doReturn("SELECT b FROM admin$Audit b WHERE b.timestamp > {DATE-1m}").when(bean).getQuery();

		SubstitutedQueryResult result = bean.getSubstitutedQuery();

		assertThat(result, is(notNullValue()));
		assertThat(result.getQuery(), containsString(":d_"));
		assertEquals(1, result.getParameters().size());
	}

	@Test
	void testGetSubstitutedQueryWithYearModifierReplacesDate() {
		doReturn(DatasetType.bizQL).when(bean).getDatasetType();
		doReturn("SELECT b FROM admin$Audit b WHERE b.timestamp > {DATE+1y}").when(bean).getQuery();

		SubstitutedQueryResult result = bean.getSubstitutedQuery();

		assertThat(result, is(notNullValue()));
		assertThat(result.getQuery(), containsString(":d_"));
		assertEquals(1, result.getParameters().size());
	}

	@Test
	void testExecuteQueryThrowsForNonBizQLType() {
		doReturn(DatasetType.SQL).when(bean).getDatasetType();

		assertThrows(IllegalArgumentException.class, () -> bean.executeQuery());
	}

	@Test
	void testExecuteSQLQueryThrowsForNonSQLType() {
		doReturn(DatasetType.bizQL).when(bean).getDatasetType();

		assertThrows(IllegalArgumentException.class, () -> bean.executeSQLQuery());
	}

	@Test
	void testExecuteTestQueryThrowsForNonBizQLType() {
		doReturn(DatasetType.SQL).when(bean).getDatasetType();

		assertThrows(IllegalArgumentException.class, () -> bean.executeTestQuery());
	}

	@Test
	void testExecuteTestSQLQueryThrowsForNonSQLType() {
		doReturn(DatasetType.bizQL).when(bean).getDatasetType();

		assertThrows(IllegalArgumentException.class, () -> bean.executeTestSQLQuery());
	}

	@Test
	void testSubstitutedQueryResultGetters() {
		doReturn(DatasetType.SQL).when(bean).getDatasetType();
		doReturn("SELECT * FROM ADM_User").when(bean).getQuery();

		SubstitutedQueryResult result = bean.getSubstitutedQuery();

		assertThat(result.getQuery(), is("SELECT * FROM ADM_User"));
		assertThat(result.getParameters(), is(notNullValue()));
		assertTrue(result.getParameters().isEmpty());
	}
}
