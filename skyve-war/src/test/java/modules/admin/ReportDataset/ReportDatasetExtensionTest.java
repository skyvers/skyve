package modules.admin.ReportDataset;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.mockito.Spy;

import modules.admin.ReportDataset.ReportDatasetExtension.SubstitutedQueryResult;

public class ReportDatasetExtensionTest {

	@Spy
	private ReportDatasetExtension bean;

	private AutoCloseable closeable;

	@BeforeEach
	public void openMocks() {
		closeable = MockitoAnnotations.openMocks(this);
	}

	@AfterEach
	public void releaseMocks() throws Exception {
		closeable.close();
	}

	@Test
	@SuppressWarnings("boxing")
	public void testGetSubstitutedQueryOneParameter() {
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

		assertThat(result.getParameters().size(), is(1));
	}

	@Test
	@SuppressWarnings("boxing")
	public void testGetSubstitutedQueryTwoParameters() {
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

		assertThat(result.getParameters().size(), is(2));
	}
}
