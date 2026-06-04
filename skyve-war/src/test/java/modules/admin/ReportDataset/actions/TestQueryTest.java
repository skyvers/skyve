package modules.admin.ReportDataset.actions;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;
import org.skyve.CORE;
import org.skyve.domain.app.admin.ReportDataset.DatasetType;
import org.skyve.domain.app.admin.ReportParameter.Type;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.messages.ValidationException;
import org.skyve.domain.types.DateOnly;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import modules.admin.ReportDataset.ReportDatasetExtension;
import modules.admin.ReportParameter.ReportParameterExtension;
import modules.admin.ReportTemplate.ReportTemplateExtension;
import modules.admin.domain.ReportDataset;
import modules.admin.domain.ReportTemplate;
import modules.test.domain.AllAttributesPersistent;
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
	void executeWithUndeclaredWholeQueryParameterThrowsValidationException() {
		ReportDatasetExtension bean = ReportDataset.newInstance();
		bean.setParent(ReportTemplate.newInstance());
		bean.setDatasetType(DatasetType.bizQL);
		bean.setQuery(":missingParameter");
		TestQuery action = new TestQuery();

		ValidationException exception = assertThrows(ValidationException.class, () -> action.execute(bean, null));

		assertTrue(exception.getMessages()
				.stream()
				.anyMatch(message -> message.getText().contains("parameter has been defined")));
		assertNull(bean.getResults());
	}

	@Test
	void executeWithInvalidSqlStoresStackTraceInResults() throws Exception {
		ReportDatasetExtension bean = ReportDataset.newInstance();
		bean.setParent(ReportTemplate.newInstance());
		bean.setDatasetType(DatasetType.SQL);
		bean.setQuery("select * from definitely_missing_table");

		ServerSideActionResult<ReportDatasetExtension> result = new TestQuery().execute(bean, null);

		assertSame(bean, result.getBean());
		assertNotNull(bean.getResults());
		assertTrue(bean.getResults().contains("definitely_missing_table"));
	}

	@Test
	void executeWithInvalidSqlBindsAllSupportedParameterTypesBeforeStoringStackTrace() throws Exception {
		ReportDatasetExtension bean = ReportDataset.newInstance();
		ReportTemplateExtension parent = ReportTemplate.newInstance();
		parent.getParameters().add(parameter("dateParam", Type.date, new DateOnly(), null, null));
		parent.getParameters().add(parameter("integerParam", Type.integer, null, Long.valueOf(12L), null));
		parent.getParameters().add(parameter("longParam", Type.longInteger, null, Long.valueOf(123L), null));
		parent.getParameters().add(parameter("textParam", Type.text, null, null, "alpha"));
		parent.getParameters().add(parameter("unusedParam", Type.text, null, null, "ignored"));
		bean.setParent(parent);
		bean.setDatasetType(DatasetType.SQL);
		bean.setQuery("select * from definitely_missing_table where date_col = :dateParam and int_col = :integerParam "
				+ "and long_col = :longParam and text_col = :textParam");

		ServerSideActionResult<ReportDatasetExtension> result = new TestQuery().execute(bean, null);

		assertSame(bean, result.getBean());
		assertNotNull(bean.getResults());
		assertTrue(bean.getResults().contains("definitely_missing_table"));
	}

	@Test
	void executeWithValidSqlStoresFormattedRowsInResults() throws Exception {
		ReportDatasetExtension bean = ReportDataset.newInstance();
		bean.setParent(ReportTemplate.newInstance());
		bean.setDatasetType(DatasetType.SQL);
		bean.setQuery("select 1 as number_value, 'alpha' as text_value");

		ServerSideActionResult<ReportDatasetExtension> result = new TestQuery().execute(bean, null);

		assertSame(bean, result.getBean());
		assertNotNull(bean.getResults());
		assertTrue(bean.getResults().contains("alpha"));
	}

	@Test
	void executeWithValidBizQLStoresBeanBizKeyInResults() throws Exception {
		AllAttributesPersistent saved = new DataBuilder().fixture(FixtureType.crud)
				.build(AllAttributesPersistent.MODULE_NAME, AllAttributesPersistent.DOCUMENT_NAME);
		saved.setText("test-query-bizql");
		saved = CORE.getPersistence().save(saved);
		ReportDatasetExtension bean = ReportDataset.newInstance();
		bean.setParent(ReportTemplate.newInstance());
		bean.setDatasetType(DatasetType.bizQL);
		bean.setQuery("select bean from {test.AllAttributesPersistent} as bean where bean.bizId = '" + saved.getBizId() + "'");

		ServerSideActionResult<ReportDatasetExtension> result = new TestQuery().execute(bean, null);

		assertSame(bean, result.getBean());
		assertNotNull(bean.getResults());
		assertTrue(bean.getResults().contains(saved.getBizKey()));
	}

	@Test
	void executeWithMissingClassDatasetThrowsDomainException() {
		ReportDatasetExtension bean = ReportDataset.newInstance();
		bean.setParent(ReportTemplate.newInstance());
		bean.setDatasetType(DatasetType.classValue);
		bean.setQuery("example.DoesNotExist");
		TestQuery action = new TestQuery();

		DomainException exception = assertThrows(DomainException.class, () -> action.execute(bean, null));

		assertTrue(exception.getMessage().contains("Unable to create an instance of example.DoesNotExist"));
	}

	private static ReportParameterExtension parameter(String name,
			Type type,
			DateOnly dateTestValue,
			Long numericalTestValue,
			String textTestValue) {
		ReportParameterExtension parameter = new ReportParameterExtension();
		parameter.setName(name);
		parameter.setType(type);
		parameter.setDateTestValue(dateTestValue);
		parameter.setNumericalTestValue(numericalTestValue);
		parameter.setTextTestValue(textTestValue);
		return parameter;
	}
}
