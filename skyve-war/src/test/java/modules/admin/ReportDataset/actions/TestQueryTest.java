package modules.admin.ReportDataset.actions;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;

import org.junit.jupiter.api.Test;
import org.skyve.metadata.controller.ServerSideActionResult;

import modules.admin.ReportDataset.ReportDatasetExtension;

@SuppressWarnings("static-method")
class TestQueryTest {

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
}
