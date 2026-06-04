package modules.admin.ReportDataset.actions;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;
import org.skyve.domain.app.admin.ReportDataset.DatasetType;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.messages.ValidationException;
import org.skyve.metadata.controller.ServerSideActionResult;

import modules.admin.ReportDataset.ReportDatasetExtension;
import modules.admin.domain.ReportTemplate;
import util.AbstractH2Test;

@SuppressWarnings("static-method")
class TestQueryTest extends AbstractH2Test {

	/**
	 * When datasetType is null the validate method does nothing (its condition
	 * is only entered for bizQL or SQL) and none of the if/else branches match,
	 * so execute simply returns the bean without touching CORE or CDI.
	 */
	@Test
	void executeWithNullDatasetTypeReturnsBean() throws Exception {
		ReportDatasetExtension bean = new ReportDatasetExtension();
		// override default bizQL so that CORE is not called
		bean.setDatasetType(null);

		TestQuery action = new TestQuery();
		ServerSideActionResult<ReportDatasetExtension> result = action.execute(bean, null);

		assertNotNull(result);
		assertSame(bean, result.getBean());
		assertNull(bean.getResults()); // setResults(null) was the first call
	}

	@Test
	void executeWithUndeclaredWholeQueryParameterThrowsValidationException() throws Exception {
		ReportDatasetExtension bean = ReportDatasetExtension.newInstance();
		bean.setParent(ReportTemplate.newInstance());
		bean.setDatasetType(DatasetType.bizQL);
		bean.setQuery(":missingParameter");

		ValidationException exception = assertThrows(ValidationException.class, () -> new TestQuery().execute(bean, null));

		assertTrue(exception.getMessages()
				.stream()
				.anyMatch(message -> message.getText().contains("parameter has been defined")));
		assertNull(bean.getResults());
	}

	@Test
	void executeWithInvalidSqlStoresStackTraceInResults() throws Exception {
		ReportDatasetExtension bean = ReportDatasetExtension.newInstance();
		bean.setParent(ReportTemplate.newInstance());
		bean.setDatasetType(DatasetType.SQL);
		bean.setQuery("select * from definitely_missing_table");

		ServerSideActionResult<ReportDatasetExtension> result = new TestQuery().execute(bean, null);

		assertSame(bean, result.getBean());
		assertNotNull(bean.getResults());
		assertTrue(bean.getResults().contains("definitely_missing_table"));
	}

	@Test
	void executeWithMissingClassDatasetThrowsDomainException() throws Exception {
		ReportDatasetExtension bean = ReportDatasetExtension.newInstance();
		bean.setParent(ReportTemplate.newInstance());
		bean.setDatasetType(DatasetType.classValue);
		bean.setQuery("example.DoesNotExist");

		DomainException exception = assertThrows(DomainException.class, () -> new TestQuery().execute(bean, null));

		assertTrue(exception.getMessage().contains("Unable to create an instance of example.DoesNotExist"));
	}
}
