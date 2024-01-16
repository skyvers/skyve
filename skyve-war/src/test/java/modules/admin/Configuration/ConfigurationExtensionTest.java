package modules.admin.Configuration;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.mockito.Mockito.when;

import org.junit.Before;
import org.junit.Test;
import org.mockito.MockitoAnnotations;
import org.mockito.Spy;

public class ConfigurationExtensionTest {

	@Spy
	private ConfigurationExtension bean;

	@Before
	public void setup() throws Exception {
		MockitoAnnotations.initMocks(this);
	}

	@Test
	public void testGetPasswordRuleDescriptionLenghtOnly() {
		// call the method under test
		String result = bean.getPasswordRuleDescription();

		// verify the result
		assertThat(result, is("Passwords must be 10 characters or more."));
	}

	@Test
	public void testGetPasswordRuleDescriptionMustContainNumeric() {
		// setup mocks
		when(bean.getPasswordRequireNumeric()).thenReturn(Boolean.TRUE);

		// call the method under test
		String result = bean.getPasswordRuleDescription();

		// verify the result
		assertThat(result, is("Passwords must be 10 characters or more and contain at least one numeric character."));
	}

	@Test
	public void testGetPasswordRuleDescriptionMustContainLowercase() {
		// setup mocks
		when(bean.getPasswordRequireLowercase()).thenReturn(Boolean.TRUE);

		// call the method under test
		String result = bean.getPasswordRuleDescription();

		// verify the result
		assertThat(result, is("Passwords must be 10 characters or more and contain at least one lowercase character."));
	}

	@Test
	public void testGetPasswordRuleDescriptionMustContainSpecial() {
		// setup mocks
		when(bean.getPasswordRequireSpecial()).thenReturn(Boolean.TRUE);

		// call the method under test
		String result = bean.getPasswordRuleDescription();

		// verify the result
		assertThat(result, is("Passwords must be 10 characters or more and contain at least one special character."));
	}

	@Test
	public void testGetPasswordRuleDescriptionMustContainUppercase() {
		// setup mocks
		when(bean.getPasswordRequireUppercase()).thenReturn(Boolean.TRUE);

		// call the method under test
		String result = bean.getPasswordRuleDescription();

		// verify the result
		assertThat(result, is("Passwords must be 10 characters or more and contain at least one uppercase character."));
	}

	@Test
	public void testGetPasswordRuleDescriptionTwoRules() {
		// setup mocks
		when(bean.getPasswordRequireLowercase()).thenReturn(Boolean.TRUE);
		when(bean.getPasswordRequireUppercase()).thenReturn(Boolean.TRUE);

		// call the method under test
		String result = bean.getPasswordRuleDescription();

		// verify the result
		assertThat(result, is("Passwords must be 10 characters or more and contain lowercase and uppercase characters."));
	}

	@Test
	public void testGetPasswordRuleDescriptionThreeRules() {
		// setup mocks
		when(bean.getPasswordRequireLowercase()).thenReturn(Boolean.TRUE);
		when(bean.getPasswordRequireNumeric()).thenReturn(Boolean.TRUE);
		when(bean.getPasswordRequireUppercase()).thenReturn(Boolean.TRUE);

		// call the method under test
		String result = bean.getPasswordRuleDescription();

		// verify the result
		assertThat(result,
				is("Passwords must be 10 characters or more and contain lowercase, uppercase and numeric characters."));
	}

	@Test
	public void testGetPasswordRuleDescriptionFourRules() {
		// setup mocks
		when(bean.getPasswordRequireLowercase()).thenReturn(Boolean.TRUE);
		when(bean.getPasswordRequireNumeric()).thenReturn(Boolean.TRUE);
		when(bean.getPasswordRequireSpecial()).thenReturn(Boolean.TRUE);
		when(bean.getPasswordRequireUppercase()).thenReturn(Boolean.TRUE);

		// call the method under test
		String result = bean.getPasswordRuleDescription();

		// verify the result
		assertThat(result,
				is("Passwords must be 10 characters or more and contain lowercase, uppercase, numeric and special characters."));
	}

	@Test
	public void testMeetsComplexityStringTooShortReturnsFalse() {
		// setup test data
		String cleartext = "short";

		// call the method under test
		Boolean result = Boolean.valueOf(bean.meetsComplexity(cleartext));

		// verify the result
		assertThat(result, is(Boolean.FALSE));
	}

	@Test
	public void testMeetsComplexityStringNotTooShortReturnsTrue() {
		// setup test data
		String cleartext = "not too short";

		// call the method under test
		Boolean result = Boolean.valueOf(bean.meetsComplexity(cleartext));

		// verify the result
		assertThat(result, is(Boolean.TRUE));
	}

	@Test
	public void testMeetsComplexityStringMustContainNumericInvalid() {
		// setup test data
		String cleartext = "does not contain numeric";

		// setup mocks
		when(bean.getPasswordRequireNumeric()).thenReturn(Boolean.TRUE);

		// call the method under test
		Boolean result = Boolean.valueOf(bean.meetsComplexity(cleartext));

		// verify the result
		assertThat(result, is(Boolean.FALSE));
	}

	@Test
	public void testMeetsComplexityStringMustContainNumericValid() {
		// setup test data
		String cleartext1 = "contains one numera1";
		String cleartext2 = "3ontains one numeral";
		String cleartext3 = "12345678910";

		// setup mocks
		when(bean.getPasswordRequireNumeric()).thenReturn(Boolean.TRUE);

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
	public void testMeetsComplexityStringMustContainLowercaseInvalid() {
		// setup test data
		String cleartext = "DOES NOT CONTAIN LOWERCASE";

		// setup mocks
		when(bean.getPasswordRequireLowercase()).thenReturn(Boolean.TRUE);

		// call the method under test
		Boolean result = Boolean.valueOf(bean.meetsComplexity(cleartext));

		// verify the result
		assertThat(result, is(Boolean.FALSE));
	}

	@Test
	public void testMeetsComplexityStringMustContainLowercaseValid() {
		// setup test data
		String cleartext1 = "CONTAINS ONE LOWERCASe";
		String cleartext2 = "cONTAINS ONE LOWERCASE";
		String cleartext3 = "containsonlylowercase";

		// setup mocks
		when(bean.getPasswordRequireLowercase()).thenReturn(Boolean.TRUE);

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
	public void testMeetsComplexityStringMustContainSpecialInvalid() {
		// setup test data
		String cleartext = "doesNotContainSp3cial";

		// setup mocks
		when(bean.getPasswordRequireSpecial()).thenReturn(Boolean.TRUE);

		// call the method under test
		Boolean result = Boolean.valueOf(bean.meetsComplexity(cleartext));

		// verify the result
		assertThat(result, is(Boolean.FALSE));
	}

	@Test
	public void testMeetsComplexityStringMustContainSpecialValid() {
		// setup test data
		String cleartext1 = "contains onespecial";
		String cleartext2 = "%ontainsonespecial";
		String cleartext3 = "!@#$%^&*+-./";

		// setup mocks
		when(bean.getPasswordRequireSpecial()).thenReturn(Boolean.TRUE);

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
	public void testMeetsComplexityStringMustContainUppercaseInvalid() {
		// setup test data
		String cleartext = "does not contain uppercase";

		// setup mocks
		when(bean.getPasswordRequireUppercase()).thenReturn(Boolean.TRUE);

		// call the method under test
		Boolean result = Boolean.valueOf(bean.meetsComplexity(cleartext));

		// verify the result
		assertThat(result, is(Boolean.FALSE));
	}

	@Test
	public void testMeetsComplexityStringMustContainUppercaseValid() {
		// setup test data
		String cleartext1 = "contains one uppercasE";
		String cleartext2 = "Contains one uppercase";
		String cleartext3 = "CONTAINSONLYUPPERCASE";

		// setup mocks
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
}
