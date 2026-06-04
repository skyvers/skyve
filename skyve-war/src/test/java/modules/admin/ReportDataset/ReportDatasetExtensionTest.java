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

import java.lang.reflect.Field;
import java.time.LocalDate;
import java.util.List;

import org.apache.commons.beanutils.DynaBean;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.mockito.Spy;
import org.skyve.CORE;
import org.skyve.domain.app.admin.ReportDataset.DatasetType;
import org.skyve.domain.app.admin.ReportParameter.Type;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.types.DateOnly;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import modules.admin.ReportDataset.ReportDatasetExtension.SubstitutedQueryResult;
import modules.admin.ReportParameter.ReportParameterExtension;
import modules.admin.ReportTemplate.ReportTemplateExtension;
import modules.admin.domain.ReportDataset;
import modules.admin.domain.ReportTemplate;
import modules.test.domain.AllAttributesPersistent;
import util.AbstractH2Test;

@SuppressWarnings("static-method")
class ReportDatasetExtensionTest extends AbstractH2Test {

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
	void testAddMissingParametersCreatesAllNamedParametersWhenNoneExist() throws Exception {
		ReportDatasetExtension dataset = new ReportDatasetExtension();
		ReportTemplateExtension parent = new ReportTemplateExtension();
		dataset.setParent(parent);
		dataset.setDatasetType(DatasetType.bizQL);
		dataset.setQuery("SELECT b FROM admin$User b WHERE b.userName = :userName AND b.createdDate = :createdDate");
		injectReportDatasetService(dataset);

		dataset.addMissingParameters();

		assertEquals(2, parent.getParameters().size());
		assertTrue(parent.getParameters().stream().anyMatch(p -> "userName".equals(p.getName())));
		assertTrue(parent.getParameters()
				.stream()
				.anyMatch(p -> "createdDate".equals(p.getName()) && Type.date == p.getType()));
	}

	@Test
	void testAddMissingParametersOnlyAddsNewParameters() throws Exception {
		ReportDatasetExtension dataset = new ReportDatasetExtension();
		ReportTemplateExtension parent = new ReportTemplateExtension();
		ReportParameterExtension existing = new ReportParameterExtension();
		existing.setName("userName");
		parent.getParameters().add(existing);
		dataset.setParent(parent);
		dataset.setDatasetType(DatasetType.SQL);
		dataset.setQuery("SELECT * FROM ADM_SecurityUser WHERE userName = :userName AND status = :status");
		injectReportDatasetService(dataset);

		dataset.addMissingParameters();

		assertEquals(2, parent.getParameters().size());
		assertEquals(1, parent.getParameters().stream().filter(p -> "userName".equals(p.getName())).count());
		assertTrue(parent.getParameters().stream().anyMatch(p -> "status".equals(p.getName())));
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
	void testGetSubstitutedQueryWithDuplicateDateExpressionReplacesBothWithOneParameter() {
		doReturn(DatasetType.bizQL).when(bean).getDatasetType();
		doReturn("SELECT b FROM admin$Audit b WHERE b.createdDate = {DATE} OR b.updatedDate = {DATE}").when(bean).getQuery();

		SubstitutedQueryResult result = bean.getSubstitutedQuery();

		assertThat(result, is(notNullValue()));
		assertThat(result.getQuery(), containsString(":d_"));
		assertFalse(result.getQuery().contains("{DATE}"));
		assertEquals(1, result.getParameters().size());
	}

	@Test
	void testGetSubstitutedQueryWithInvalidDateExpressionThrows() {
		doReturn(DatasetType.bizQL).when(bean).getDatasetType();
		doReturn("SELECT b FROM admin$Audit b WHERE b.timestamp > {DATE+abc}").when(bean).getQuery();

		IllegalArgumentException thrown = assertThrows(IllegalArgumentException.class, () -> bean.getSubstitutedQuery());

		assertThat(thrown.getMessage(), containsString("Invalid date expression"));
	}

	@Test
	void testGetSubstitutedQueryWithUnsupportedUppercaseModifierThrows() {
		doReturn(DatasetType.bizQL).when(bean).getDatasetType();
		doReturn("SELECT b FROM admin$Audit b WHERE b.timestamp > {DATE+1D}").when(bean).getQuery();

		IllegalStateException thrown = assertThrows(IllegalStateException.class, () -> bean.getSubstitutedQuery());

		assertThat(thrown.getMessage(), containsString("D is not catered for"));
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
	void testExecuteSQLQueryBindsInputAndDefaultValues() throws Exception {
		ReportDatasetExtension dataset = ReportDataset.newInstance();
		ReportTemplateExtension parent = ReportTemplate.newInstance();
		parent.getParameters().add(parameterWithInput("dateParam", Type.date, "01-Feb-2026", null, null, null, null, null));
		parent.getParameters().add(parameterWithInput("integerParam", Type.integer, null, Long.valueOf(3L), null, null, null, null));
		parent.getParameters().add(parameterWithInput("longParam", Type.longInteger, null, null, "7", null, null, null));
		parent.getParameters().add(parameterWithInput("textParam", Type.text, null, null, null, null, null, "input"));
		parent.getParameters().add(parameterWithInput("unusedParam", Type.text, null, null, null, null, null, "ignored"));
		dataset.setParent(parent);
		dataset.setDatasetType(DatasetType.SQL);
		dataset.setQuery("select :dateParam as date_value, :integerParam as integer_value, :longParam as long_value, "
				+ ":textParam as text_value");

		List<DynaBean> results = dataset.executeSQLQuery();

		assertEquals(1, results.size());
	}

	@Test
	void testExecuteQueryBindsInputAndDefaultValues() throws Exception {
		DateOnly savedDate = new DateOnly(LocalDate.of(2026, 2, 1));
		AllAttributesPersistent saved = saveAllAttributes("report-bizql-input", 11, 17L, savedDate);
		ReportDatasetExtension dataset = ReportDataset.newInstance();
		ReportTemplateExtension parent = ReportTemplate.newInstance();
		parent.getParameters().add(parameterWithInput("dateParam", Type.date,
				CORE.getCustomer().getDefaultDateConverter().toDisplayValue(savedDate), null, null, null, null, null));
		parent.getParameters().add(parameterWithInput("integerParam", Type.integer, null, Long.valueOf(11L), null, null, null, null));
		parent.getParameters().add(parameterWithInput("longParam", Type.longInteger, null, null, "17", null, null, null));
		parent.getParameters().add(parameterWithInput("textParam", Type.text, null, null, null, null, null, "report-bizql-input"));
		parent.getParameters().add(parameterWithInput("unusedParam", Type.text, null, null, null, null, null, "ignored"));
		dataset.setParent(parent);
		dataset.setDatasetType(DatasetType.bizQL);
		dataset.setQuery("select bean from {test.AllAttributesPersistent} as bean "
				+ "where bean.text = :textParam and bean.normalInteger = :integerParam "
				+ "and bean.longInteger = :longParam and bean.date = :dateParam");

		List<?> results = dataset.executeQuery();

		assertEquals(1, results.size());
		assertEquals(saved.getBizId(), ((AllAttributesPersistent) results.get(0)).getBizId());
	}

	@Test
	void testExecuteTestSQLQueryBindsAllTestValueTypes() throws Exception {
		ReportDatasetExtension dataset = ReportDataset.newInstance();
		ReportTemplateExtension parent = ReportTemplate.newInstance();
		parent.getParameters().add(parameterWithInput("dateParam", Type.date, null, null, null, new DateOnly(), null, null));
		parent.getParameters().add(parameterWithInput("integerParam", Type.integer, null, null, null, null, Long.valueOf(4L), null));
		parent.getParameters().add(parameterWithInput("longParam", Type.longInteger, null, null, null, null, Long.valueOf(8L), null));
		parent.getParameters().add(parameterWithInput("textParam", Type.text, null, null, null, null, null, "test"));
		parent.getParameters().add(parameterWithInput("unusedParam", Type.text, null, null, null, null, null, "ignored"));
		dataset.setParent(parent);
		dataset.setDatasetType(DatasetType.SQL);
		dataset.setQuery("select :dateParam as date_value, :integerParam as integer_value, :longParam as long_value, "
				+ ":textParam as text_value");

		List<DynaBean> results = dataset.executeTestSQLQuery();

		assertEquals(1, results.size());
	}

	@Test
	void testExecuteTestQueryBindsAllTestValueTypes() {
		DateOnly savedDate = new DateOnly(LocalDate.of(2026, 3, 2));
		AllAttributesPersistent saved = saveAllAttributes("report-bizql-test", 13, 19L, savedDate);
		ReportDatasetExtension dataset = ReportDataset.newInstance();
		ReportTemplateExtension parent = ReportTemplate.newInstance();
		parent.getParameters().add(parameterWithInput("dateParam", Type.date, null, null, null, savedDate, null, null));
		parent.getParameters().add(parameterWithInput("integerParam", Type.integer, null, null, null, null, Long.valueOf(13L), null));
		parent.getParameters().add(parameterWithInput("longParam", Type.longInteger, null, null, null, null, Long.valueOf(19L), null));
		parent.getParameters().add(parameterWithInput("textParam", Type.text, null, null, null, null, null, "report-bizql-test"));
		parent.getParameters().add(parameterWithInput("unusedParam", Type.text, null, null, null, null, null, "ignored"));
		dataset.setParent(parent);
		dataset.setDatasetType(DatasetType.bizQL);
		dataset.setQuery("select bean from {test.AllAttributesPersistent} as bean "
				+ "where bean.text = :textParam and bean.normalInteger = :integerParam "
				+ "and bean.longInteger = :longParam and bean.date = :dateParam");

		List<?> results = dataset.executeTestQuery();

		assertEquals(1, results.size());
		assertEquals(saved.getBizId(), ((AllAttributesPersistent) results.get(0)).getBizId());
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

	@Test
	void testSubstitutedQueryResultConstructorHandlesNullParameters() {
		SubstitutedQueryResult result = bean.new SubstitutedQueryResult("SELECT 1", null);

		assertThat(result.getQuery(), is("SELECT 1"));
		assertThat(result.getParameters(), is(notNullValue()));
		assertTrue(result.getParameters().isEmpty());
	}

	@Test
	void testSubstitutedQueryResultConstructorKeepsSuppliedParameters() {
		java.util.Map<String, DateOnly> parameters = java.util.Map.of("dateParam", new DateOnly());

		SubstitutedQueryResult result = bean.new SubstitutedQueryResult("SELECT :dateParam", parameters);

		assertThat(result.getQuery(), is("SELECT :dateParam"));
		assertThat(result.getParameters(), is(parameters));
	}

	private static void injectReportDatasetService(ReportDatasetExtension dataset) throws Exception {
		Field field = ReportDatasetExtension.class.getDeclaredField("reportDatasetService");
		field.setAccessible(true);
		field.set(dataset, new TestReportDatasetService());
	}

	private static ReportParameterExtension parameterWithInput(String name,
			Type type,
			String reportInputValue,
			Long numericalDefaultValue,
			String longReportInputValue,
			DateOnly dateTestValue,
			Long numericalTestValue,
			String textValue) {
		ReportParameterExtension parameter = new ReportParameterExtension();
		parameter.setName(name);
		parameter.setType(type);
		parameter.setReportInputValue(longReportInputValue != null ? longReportInputValue : reportInputValue);
		parameter.setNumericalDefaultValue(numericalDefaultValue);
		parameter.setDateTestValue(dateTestValue);
		parameter.setNumericalTestValue(numericalTestValue);
		parameter.setTextTestValue(textValue);
		parameter.setTextDefaultValue(textValue);
		return parameter;
	}

	private static AllAttributesPersistent saveAllAttributes(String text, int integer, long longInteger, DateOnly date) {
		AllAttributesPersistent bean = new DataBuilder().fixture(FixtureType.crud)
				.build(AllAttributesPersistent.MODULE_NAME, AllAttributesPersistent.DOCUMENT_NAME);
		bean.setText(text);
		bean.setNormalInteger(Integer.valueOf(integer));
		bean.setLongInteger(Long.valueOf(longInteger));
		bean.setDate(date);
		return CORE.getPersistence().save(bean);
	}

	private static final class TestReportDatasetService extends ReportDatasetService {
		@Override
		protected ReportParameterExtension createNewParameter(String parameterName) {
			ReportParameterExtension parameter = new ReportParameterExtension();
			parameter.setName(parameterName);
			parameter.setDescription(parameterName);
			if (parameterName.endsWith("Date")) {
				parameter.setType(Type.date);
			}
			return parameter;
		}
	}
}
