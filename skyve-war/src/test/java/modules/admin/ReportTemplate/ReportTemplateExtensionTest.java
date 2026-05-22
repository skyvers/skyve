package modules.admin.ReportTemplate;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockitoAnnotations;
import org.mockito.Spy;
import org.skyve.domain.app.admin.ReportDataset.DatasetType;
import org.skyve.domain.messages.ValidationException;

import modules.admin.ReportDataset.ReportDatasetExtension;
import modules.admin.ReportParameter.ReportParameterExtension;
import modules.admin.domain.ReportTemplate.ReportType;

class ReportTemplateExtensionTest {

	@Spy
	private ReportTemplateExtension bean;

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
	void testClearSchedulesClearsAllFields() {
		bean.setScheduled(Boolean.TRUE);
		bean.setCronExpression("0 0 * * * ?");

		bean.clearSchedules();

		assertThat(bean.getScheduled(), is(Boolean.FALSE));
		assertNull(bean.getCronExpression());
		assertNull(bean.getStartTime());
		assertNull(bean.getEndTime());
		assertNull(bean.getRunAs());
		assertTrue(bean.getUsersToEmail().isEmpty());
	}

	@Test
	void testGetScheduleDescriptionReturnsDescriptionForValidCron() {
		bean.setCronExpression("0 0 12 * * ?");
		String description = bean.getScheduleDescription();
		assertNotNull(description);
		assertFalse(description.isEmpty());
	}

	@Test
	void testGetScheduleDescriptionReturnsNullForInvalidCron() {
		bean.setCronExpression("not-a-valid-cron");
		String description = bean.getScheduleDescription();
		assertNull(description);
	}

	@Test
	void testGetScheduleDescriptionReturnsNullForNullCron() {
		bean.setCronExpression(null);
		String description = bean.getScheduleDescription();
		assertNull(description);
	}

	@Test
	void testHasRequiredParametersReturnsTrueWhenRequiredParamPresent() {
		ReportParameterExtension requiredParam = new ReportParameterExtension();
		requiredParam.setName("startDate");
		requiredParam.setRequired(Boolean.TRUE);
		bean.getParameters().add(requiredParam);

		assertTrue(bean.hasRequiredParameters());
	}

	@Test
	void testHasRequiredParametersReturnsFalseWhenNoRequiredParams() {
		ReportParameterExtension optionalParam = new ReportParameterExtension();
		optionalParam.setName("optionalParam");
		optionalParam.setRequired(Boolean.FALSE);
		bean.getParameters().add(optionalParam);

		assertFalse(bean.hasRequiredParameters());
	}

	@Test
	void testHasRequiredParametersReturnsFalseWhenEmpty() {
		assertFalse(bean.hasRequiredParameters());
	}

	@Test
	void testValidateReportParametersSkipsForJasperType() {
		bean.setReportType(ReportType.jasper);
		ReportParameterExtension param = new ReportParameterExtension();
		param.setName("unused");
		bean.getParameters().add(param);

		ValidationException e = new ValidationException();
		// should not throw and should not add any errors
		assertDoesNotThrow(() -> bean.validateReportParameters(e));
		assertTrue(e.getMessages().isEmpty());
	}

	@Test
	void testValidateReportParametersAddsErrorWhenParamNotInAnyDataset() {
		bean.setReportType(ReportType.freemarker);

		// add a param that is NOT used in any dataset
		ReportParameterExtension param = new ReportParameterExtension();
		param.setName("unusedParam");
		bean.getParameters().add(param);

		// add a bizQL dataset that does NOT contain the param
		ReportDatasetExtension dataset = new ReportDatasetExtension();
		dataset.setDatasetType(DatasetType.bizQL);
		dataset.setQuery("SELECT b FROM admin$User b");
		bean.getDatasets().add(dataset);

		ValidationException e = new ValidationException();
		bean.validateReportParameters(e);

		assertFalse(e.getMessages().isEmpty());
	}

	@Test
	void testValidateReportParametersNoErrorWhenParamFoundInBizQLDataset() {
		bean.setReportType(ReportType.freemarker);

		// add a param used in the dataset
		ReportParameterExtension param = new ReportParameterExtension();
		param.setName("startDate");
		bean.getParameters().add(param);

		// add a bizQL dataset that contains the param
		ReportDatasetExtension dataset = new ReportDatasetExtension();
		dataset.setDatasetType(DatasetType.bizQL);
		dataset.setQuery("SELECT b FROM admin$User b WHERE b.createdDateTime > :startDate");
		bean.getDatasets().add(dataset);

		ValidationException e = new ValidationException();
		bean.validateReportParameters(e);

		assertTrue(e.getMessages().isEmpty());
	}

	@Test
	void testValidateReportParametersNoErrorWhenClassValueDatasetPresent() {
		bean.setReportType(ReportType.freemarker);

		// param in a classValue dataset — classValue always injects all params
		ReportParameterExtension param = new ReportParameterExtension();
		param.setName("anyParam");
		bean.getParameters().add(param);

		ReportDatasetExtension dataset = new ReportDatasetExtension();
		dataset.setDatasetType(DatasetType.classValue);
		bean.getDatasets().add(dataset);

		ValidationException e = new ValidationException();
		bean.validateReportParameters(e);

		assertTrue(e.getMessages().isEmpty());
	}

	@Test
	void testValidateReportParametersSkipsConstantDatasets() {
		bean.setReportType(ReportType.freemarker);

		// add a param
		ReportParameterExtension param = new ReportParameterExtension();
		param.setName("reportParam");
		bean.getParameters().add(param);

		// add only a constant dataset (should skip it)
		ReportDatasetExtension constantDataset = new ReportDatasetExtension();
		constantDataset.setDatasetType(DatasetType.constant);
		bean.getDatasets().add(constantDataset);

		// add a bizQL dataset that contains the param
		ReportDatasetExtension bizQLDataset = new ReportDatasetExtension();
		bizQLDataset.setDatasetType(DatasetType.bizQL);
		bizQLDataset.setQuery("SELECT b FROM admin$User b WHERE b.name = :reportParam");
		bean.getDatasets().add(bizQLDataset);

		ValidationException e = new ValidationException();
		bean.validateReportParameters(e);

		assertTrue(e.getMessages().isEmpty());
	}

	@Test
	void testToJobSchedulePopulatesFields() {
		bean.setBizId("test-uuid");
		bean.setName("Monthly Report");
		bean.setCronExpression("0 0 12 1 * ?");

		org.skyve.job.JobSchedule result = bean.toJobSchedule();

		assertThat(result, is(notNullValue()));
		assertThat(result.getUuid(), is("test-uuid"));
		assertThat(result.getJobName(), is("Monthly Report"));
		assertThat(result.getCronExpression(), is("0 0 12 1 * ?"));
		assertThat(result.getStartTime(), is(nullValue()));
		assertThat(result.getEndTime(), is(nullValue()));
	}
}
