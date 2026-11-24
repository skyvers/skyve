package modules.admin.Configuration;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;
import java.util.function.BooleanSupplier;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockitoAnnotations;
import org.mockito.Spy;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.persistence.DocumentFilter;
import org.skyve.persistence.DocumentQuery;

import modules.admin.domain.AdminDocument;
import modules.admin.domain.JobSchedule;

class ConfigurationExtensionTest {
	@Spy
	private ConfigurationExtension bean;

	@SuppressWarnings("resource")
	private AutoCloseable closeable;
	
	@BeforeEach
	@SuppressWarnings("resource")
	void setup() {
		closeable = MockitoAnnotations.openMocks(this);
	}

	@AfterEach
	void tearDown() throws Exception {
		if (closeable != null) {
			closeable.close();
		}
	}
	
	@Test
	void testGetPasswordRuleDescriptionLenghtOnly() {
		when(bean.getPasswordRequireLowercase()).thenReturn(Boolean.FALSE);
		when(bean.getPasswordRequireNumeric()).thenReturn(Boolean.FALSE);
		when(bean.getPasswordRequireSpecial()).thenReturn(Boolean.FALSE);
		when(bean.getPasswordRequireUppercase()).thenReturn(Boolean.FALSE);

		// call the method under test
		String result = bean.getPasswordRuleDescription();

		// verify the result
		assertThat(result, is("Passwords must be 12 characters or more."));
	}

	@Test
	void testGetPasswordRuleDescriptionMustContainNumeric() {
		// setup mocks
		when(bean.getPasswordRequireLowercase()).thenReturn(Boolean.FALSE);
		when(bean.getPasswordRequireNumeric()).thenReturn(Boolean.TRUE);
		when(bean.getPasswordRequireSpecial()).thenReturn(Boolean.FALSE);
		when(bean.getPasswordRequireUppercase()).thenReturn(Boolean.FALSE);
		
		// call the method under test
		String result = bean.getPasswordRuleDescription();

		// verify the result
		assertThat(result, is("Passwords must be 12 characters or more and contain at least one numeric character."));
	}

	@Test
	void testGetPasswordRuleDescriptionMustContainLowercase() {
		// setup mocks
		when(bean.getPasswordRequireLowercase()).thenReturn(Boolean.TRUE);
		when(bean.getPasswordRequireNumeric()).thenReturn(Boolean.FALSE);
		when(bean.getPasswordRequireSpecial()).thenReturn(Boolean.FALSE);
		when(bean.getPasswordRequireUppercase()).thenReturn(Boolean.FALSE);

		// call the method under test
		String result = bean.getPasswordRuleDescription();

		// verify the result
		assertThat(result, is("Passwords must be 12 characters or more and contain at least one lowercase character."));
	}

	@Test
	void testGetPasswordRuleDescriptionMustContainSpecial() {
		// setup mocks
		when(bean.getPasswordRequireLowercase()).thenReturn(Boolean.FALSE);
		when(bean.getPasswordRequireNumeric()).thenReturn(Boolean.FALSE);
		when(bean.getPasswordRequireSpecial()).thenReturn(Boolean.TRUE);
		when(bean.getPasswordRequireUppercase()).thenReturn(Boolean.FALSE);

		// call the method under test
		String result = bean.getPasswordRuleDescription();

		// verify the result
		assertThat(result, is("Passwords must be 12 characters or more and contain at least one special character."));
	}

	@Test
	void testGetPasswordRuleDescriptionMustContainUppercase() {
		// setup mocks
		when(bean.getPasswordRequireLowercase()).thenReturn(Boolean.FALSE);
		when(bean.getPasswordRequireNumeric()).thenReturn(Boolean.FALSE);
		when(bean.getPasswordRequireSpecial()).thenReturn(Boolean.FALSE);
		when(bean.getPasswordRequireUppercase()).thenReturn(Boolean.TRUE);

		// call the method under test
		String result = bean.getPasswordRuleDescription();

		// verify the result
		assertThat(result, is("Passwords must be 12 characters or more and contain at least one uppercase character."));
	}

	@Test
	void testGetPasswordRuleDescriptionTwoRules() {
		// setup mocks
		when(bean.getPasswordRequireLowercase()).thenReturn(Boolean.TRUE);
		when(bean.getPasswordRequireNumeric()).thenReturn(Boolean.FALSE);
		when(bean.getPasswordRequireSpecial()).thenReturn(Boolean.FALSE);
		when(bean.getPasswordRequireUppercase()).thenReturn(Boolean.TRUE);

		// call the method under test
		String result = bean.getPasswordRuleDescription();

		// verify the result
		assertThat(result, is("Passwords must be 12 characters or more and contain lowercase and uppercase characters."));
	}

	@Test
	void testGetPasswordRuleDescriptionThreeRules() {
		// setup mocks
		when(bean.getPasswordRequireLowercase()).thenReturn(Boolean.TRUE);
		when(bean.getPasswordRequireNumeric()).thenReturn(Boolean.TRUE);
		when(bean.getPasswordRequireSpecial()).thenReturn(Boolean.FALSE);
		when(bean.getPasswordRequireUppercase()).thenReturn(Boolean.TRUE);

		// call the method under test
		String result = bean.getPasswordRuleDescription();

		// verify the result
		assertThat(result,
				is("Passwords must be 12 characters or more and contain lowercase, uppercase and numeric characters."));
	}

	@Test
	void testGetPasswordRuleDescriptionFourRules() {
		// setup mocks
		when(bean.getPasswordRequireLowercase()).thenReturn(Boolean.TRUE);
		when(bean.getPasswordRequireNumeric()).thenReturn(Boolean.TRUE);
		when(bean.getPasswordRequireSpecial()).thenReturn(Boolean.TRUE);
		when(bean.getPasswordRequireUppercase()).thenReturn(Boolean.TRUE);

		// call the method under test
		String result = bean.getPasswordRuleDescription();

		// verify the result
		assertThat(result,
				is("Passwords must be 12 characters or more and contain lowercase, uppercase, numeric and special characters."));
	}

	@Test
	void testMeetsComplexityStringTooShortReturnsFalse() {
		// setup test data
		String cleartext = "short";

		// call the method under test
		Boolean result = Boolean.valueOf(bean.meetsComplexity(cleartext));

		// verify the result
		assertThat(result, is(Boolean.FALSE));
	}

	@Test
	void testMeetsComplexityStringNotTooShortReturnsTrue() {
		// setup test data
		String cleartext = "not too short";

		when(bean.getPasswordRequireLowercase()).thenReturn(Boolean.FALSE);
		when(bean.getPasswordRequireNumeric()).thenReturn(Boolean.FALSE);
		when(bean.getPasswordRequireSpecial()).thenReturn(Boolean.FALSE);
		when(bean.getPasswordRequireUppercase()).thenReturn(Boolean.FALSE);

		// call the method under test
		Boolean result = Boolean.valueOf(bean.meetsComplexity(cleartext));

		// verify the result
		assertThat(result, is(Boolean.TRUE));
	}

	@Test
	void testMeetsComplexityStringMustContainNumericInvalid() {
		// setup test data
		String cleartext = "does not contain numeric";

		// setup mocks
		when(bean.getPasswordRequireLowercase()).thenReturn(Boolean.FALSE);
		when(bean.getPasswordRequireNumeric()).thenReturn(Boolean.TRUE);
		when(bean.getPasswordRequireSpecial()).thenReturn(Boolean.FALSE);
		when(bean.getPasswordRequireUppercase()).thenReturn(Boolean.FALSE);

		// call the method under test
		Boolean result = Boolean.valueOf(bean.meetsComplexity(cleartext));

		// verify the result
		assertThat(result, is(Boolean.FALSE));
	}

	@Test
	void testMeetsComplexityStringMustContainNumericValid() {
		// setup test data
		String cleartext1 = "contains one numera1";
		String cleartext2 = "3ontains one numeral";
		String cleartext3 = "123456789101234567890";

		// setup mocks
		when(bean.getPasswordRequireLowercase()).thenReturn(Boolean.FALSE);
		when(bean.getPasswordRequireNumeric()).thenReturn(Boolean.TRUE);
		when(bean.getPasswordRequireSpecial()).thenReturn(Boolean.FALSE);
		when(bean.getPasswordRequireUppercase()).thenReturn(Boolean.FALSE);

		// call the method under test
		Boolean result1 = Boolean.valueOf(bean.meetsComplexity(cleartext1));
		Boolean result2 = Boolean.valueOf(bean.meetsComplexity(cleartext2));
		Boolean result3 = Boolean.valueOf(bean.meetsComplexity(cleartext3));

		// verify the result
		assertThat(result1, is(Boolean.TRUE));
		assertThat(result2, is(Boolean.TRUE));
		assertThat(result3, is(Boolean.TRUE));
	}

	@Test
	void testMeetsComplexityStringMustContainLowercaseInvalid() {
		// setup test data
		String cleartext = "DOES NOT CONTAIN LOWERCASE";

		// setup mocks
		when(bean.getPasswordRequireLowercase()).thenReturn(Boolean.TRUE);
		when(bean.getPasswordRequireNumeric()).thenReturn(Boolean.FALSE);
		when(bean.getPasswordRequireSpecial()).thenReturn(Boolean.FALSE);
		when(bean.getPasswordRequireUppercase()).thenReturn(Boolean.FALSE);

		// call the method under test
		Boolean result = Boolean.valueOf(bean.meetsComplexity(cleartext));

		// verify the result
		assertThat(result, is(Boolean.FALSE));
	}

	@Test
	void testMeetsComplexityStringMustContainLowercaseValid() {
		// setup test data
		String cleartext1 = "CONTAINS ONE LOWERCASe";
		String cleartext2 = "cONTAINS ONE LOWERCASE";
		String cleartext3 = "containsonlylowercase";

		// setup mocks
		when(bean.getPasswordRequireLowercase()).thenReturn(Boolean.TRUE);
		when(bean.getPasswordRequireNumeric()).thenReturn(Boolean.FALSE);
		when(bean.getPasswordRequireSpecial()).thenReturn(Boolean.FALSE);
		when(bean.getPasswordRequireUppercase()).thenReturn(Boolean.FALSE);

		// call the method under test
		Boolean result1 = Boolean.valueOf(bean.meetsComplexity(cleartext1));
		Boolean result2 = Boolean.valueOf(bean.meetsComplexity(cleartext2));
		Boolean result3 = Boolean.valueOf(bean.meetsComplexity(cleartext3));

		// verify the result
		assertThat(result1, is(Boolean.TRUE));
		assertThat(result2, is(Boolean.TRUE));
		assertThat(result3, is(Boolean.TRUE));
	}

	@Test
	void testMeetsComplexityStringMustContainSpecialInvalid() {
		// setup test data
		String cleartext = "doesNotContainSp3cial";

		// setup mocks
		when(bean.getPasswordRequireLowercase()).thenReturn(Boolean.FALSE);
		when(bean.getPasswordRequireNumeric()).thenReturn(Boolean.FALSE);
		when(bean.getPasswordRequireSpecial()).thenReturn(Boolean.TRUE);
		when(bean.getPasswordRequireUppercase()).thenReturn(Boolean.FALSE);

		// call the method under test
		Boolean result = Boolean.valueOf(bean.meetsComplexity(cleartext));

		// verify the result
		assertThat(result, is(Boolean.FALSE));
	}

	@Test
	void testMeetsComplexityStringMustContainSpecialValid() {
		// setup test data
		String cleartext1 = "contains onespecial";
		String cleartext2 = "%ontainsonespecial";
		String cleartext3 = "!@#$%^&*+-./";

		// setup mocks
		when(bean.getPasswordRequireLowercase()).thenReturn(Boolean.FALSE);
		when(bean.getPasswordRequireNumeric()).thenReturn(Boolean.FALSE);
		when(bean.getPasswordRequireSpecial()).thenReturn(Boolean.TRUE);
		when(bean.getPasswordRequireUppercase()).thenReturn(Boolean.FALSE);

		// call the method under test
		Boolean result1 = Boolean.valueOf(bean.meetsComplexity(cleartext1));
		Boolean result2 = Boolean.valueOf(bean.meetsComplexity(cleartext2));
		Boolean result3 = Boolean.valueOf(bean.meetsComplexity(cleartext3));

		// verify the result
		assertThat(result1, is(Boolean.TRUE));
		assertThat(result2, is(Boolean.TRUE));
		assertThat(result3, is(Boolean.TRUE));
	}

	@Test
	void testMeetsComplexityStringMustContainUppercaseInvalid() {
		// setup test data
		String cleartext = "does not contain uppercase";

		// setup mocks
		when(bean.getPasswordRequireLowercase()).thenReturn(Boolean.FALSE);
		when(bean.getPasswordRequireNumeric()).thenReturn(Boolean.FALSE);
		when(bean.getPasswordRequireSpecial()).thenReturn(Boolean.FALSE);
		when(bean.getPasswordRequireUppercase()).thenReturn(Boolean.TRUE);

		// call the method under test
		Boolean result = Boolean.valueOf(bean.meetsComplexity(cleartext));

		// verify the result
		assertThat(result, is(Boolean.FALSE));
	}

	@Test
	void testMeetsComplexityStringMustContainUppercaseValid() {
		// setup test data
		String cleartext1 = "contains one uppercasE";
		String cleartext2 = "Contains one uppercase";
		String cleartext3 = "CONTAINSONLYUPPERCASE";

		// setup mocks
		when(bean.getPasswordRequireLowercase()).thenReturn(Boolean.FALSE);
		when(bean.getPasswordRequireNumeric()).thenReturn(Boolean.FALSE);
		when(bean.getPasswordRequireSpecial()).thenReturn(Boolean.FALSE);
		when(bean.getPasswordRequireUppercase()).thenReturn(Boolean.TRUE);

		// call the method under test
		Boolean result1 = Boolean.valueOf(bean.meetsComplexity(cleartext1));
		Boolean result2 = Boolean.valueOf(bean.meetsComplexity(cleartext2));
		Boolean result3 = Boolean.valueOf(bean.meetsComplexity(cleartext3));

		// verify the result
		assertThat(result1, is(Boolean.TRUE));
		assertThat(result2, is(Boolean.TRUE));
		assertThat(result3, is(Boolean.TRUE));
	}

	@Test
	@SuppressWarnings("static-method")
	void validBackupScheduleLooksForAnEnabledScheduleOfTheBackupJob() throws Exception {
		assertLooksForSchedule("admin.jBackup", ConfigurationExtension::validBackupSchedule);
	}

	@Test
	@SuppressWarnings("static-method")
	void validAvailableDiskSpaceAlarmScheduleLooksForAnEnabledScheduleOfTheAlarmJob() throws Exception {
		assertLooksForSchedule("admin.jAvailableDiskSpaceAlarm", ConfigurationExtension::validAvailableDiskSpaceAlarmSchedule);
	}

	/** The check is true only when a job schedule with the job's module-qualified name is found. */
	@SuppressWarnings("unchecked")
	private static void assertLooksForSchedule(String scheduledJobName, BooleanSupplier check) throws Exception {
		AbstractPersistence persistence = mock(AbstractPersistence.class);
		DocumentQuery query = mock(DocumentQuery.class);
		DocumentFilter filter = mock(DocumentFilter.class);
		when(persistence.newDocumentQuery(AdminDocument.JOB_SCHEDULE)).thenReturn(query);
		when(query.getFilter()).thenReturn(filter);
		Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
		field.setAccessible(true);
		ThreadLocal<AbstractPersistence> threadLocal = (ThreadLocal<AbstractPersistence>) field.get(null);
		threadLocal.set(persistence);
		try {
			assertFalse(check.getAsBoolean());
			doReturn(mock(JobSchedule.class)).when(query).beanResult();
			assertTrue(check.getAsBoolean());
			verify(filter, times(2)).addEquals(JobSchedule.jobNamePropertyName, scheduledJobName);
		}
		finally {
			threadLocal.remove();
		}
	}
}
