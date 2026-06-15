package modules.admin.Configuration;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.util.List;

import org.junit.Before;
import org.junit.Test;
import org.skyve.domain.messages.ValidationException;
import org.skyve.impl.util.UtilImpl;

import jakarta.inject.Inject;
import modules.admin.User.UserService;
import modules.admin.domain.Configuration;
import modules.admin.domain.Configuration.TwoFactorType;
import util.AbstractH2TestForJUnit4;

public class ConfigurationBizletTest extends AbstractH2TestForJUnit4 {

	private ConfigurationBizlet bizlet;
	private ConfigurationExtension configuration;
	@Inject
	private transient UserService userService;

	@Before
	public void setup() {
		UtilImpl.SMTP_SENDER = "test@test.com";
		bizlet = new ConfigurationBizlet();
		configuration = Configuration.newInstance();
	}

	@Test
	public void testNewInstance() throws Exception {
		// validate the test data
		assertThat(userService.currentAdminUserProxy(), is(notNullValue()));

		// call the method under test
		ConfigurationExtension result = bizlet.newInstance(configuration);

		// verify the result
		assertThat(result, is(notNullValue()));
		assertThat(result.getEmailFrom(), is(notNullValue()));
		assertThat(result.getStartup(), is(notNullValue()));
	}

	@Test
	public void testCompleteReturnsEmptyListForUnknownAttribute() throws Exception {
		List<String> result = bizlet.complete("unknownAttribute", "val", configuration);
		assertNotNull(result);
		assertTrue(result.isEmpty());
	}

	@Test
	public void testCompleteTwoFactorEmailBodyWithNullValueReturnsTfaCode() throws Exception {
		List<String> result = bizlet.complete(Configuration.twoFactorEmailBodyPropertyName, null, configuration);
		assertNotNull(result);
		assertEquals(1, result.size());
		assertEquals("{tfaCode}", result.get(0));
	}

	@Test
	public void testCompleteTwoFactorEmailBodyWithTfaCodePrefixReturnsTfaCode() throws Exception {
		List<String> result = bizlet.complete(Configuration.twoFactorEmailBodyPropertyName, "{tfaC", configuration);
		assertNotNull(result);
		assertEquals(1, result.size());
		assertEquals("{tfaCode}", result.get(0));
	}

	@Test
	public void testCompleteTwoFactorEmailBodyWithNonMatchingValueReturnsEmpty() throws Exception {
		List<String> result = bizlet.complete(Configuration.twoFactorEmailBodyPropertyName, "hello", configuration);
		assertNotNull(result);
		assertTrue(result.isEmpty());
	}

	@Test
	public void testCompletePasswordResetEmailSubjectReturnsNotNull() throws Exception {
		List<String> result = bizlet.complete(Configuration.passwordResetEmailSubjectPropertyName, null, configuration);
		assertNotNull(result);
	}

	@Test
	public void testCompletePasswordResetEmailBodyReturnsResetUrl() throws Exception {
		List<String> result = bizlet.complete(Configuration.passwordResetEmailBodyPropertyName, null, configuration);
		assertNotNull(result);
		assertTrue(result.contains("{#resetPasswordUrl}"));
	}

	@Test
	public void testValidateWithNullFieldsProducesNoErrors() throws Exception {
		configuration.setPasswordResetEmailSubject(null);
		configuration.setPasswordResetEmailBody(null);
		configuration.setTwoFactorEmailBody(null);
		ValidationException e = new ValidationException();
		bizlet.validate(configuration, e);
		assertTrue(e.getMessages().isEmpty());
	}

	@Test
	public void testValidateWithValidPasswordResetSubjectProducesNoErrors() throws Exception {
		configuration.setPasswordResetEmailSubject("Reset your password");
		ValidationException e = new ValidationException();
		bizlet.validate(configuration, e);
		assertTrue(e.getMessages().isEmpty());
	}

	@Test
	public void testValidateWithTfaCodeOnlyInBodyProducesNoErrors() throws Exception {
		configuration.setTwoFactorEmailBody("Your code is: {tfaCode}");
		ValidationException e = new ValidationException();
		bizlet.validate(configuration, e);
		assertTrue(e.getMessages().isEmpty());
	}

	@Test
	public void testValidateWithInvalidSkyveExpressionInTfaBodyProducesError() throws Exception {
		configuration.setTwoFactorEmailBody("{currentUser.name}");
		ValidationException e = new ValidationException();
		bizlet.validate(configuration, e);
		assertEquals(1, e.getMessages().size());
	}

	// ===== preRerender() — twoFactorType/email branch (no H2 needed for bean modifications) =====

	@Test
	public void testPreRerenderTwoFactorTypeEmailSetsDefaultsWhenNull() throws Exception {
		configuration.setTwoFactorType(TwoFactorType.email);
		configuration.setTwoFactorEmailBody(null);
		configuration.setTwoFactorEmailSubject(null);
		configuration.setTwofactorPushCodeTimeOutSeconds(null);

		bizlet.preRerender(Configuration.twoFactorTypePropertyName, configuration, null);

		assertNotNull(configuration.getTwoFactorEmailBody());
		assertTrue(configuration.getTwoFactorEmailBody().contains("{tfaCode}"));
		assertEquals("Email verification security code", configuration.getTwoFactorEmailSubject());
		assertEquals(Integer.valueOf(5 * 60), configuration.getTwofactorPushCodeTimeOutSeconds());
	}

	@Test
	public void testPreRerenderTwoFactorTypeEmailDoesNotOverwriteExistingBody() throws Exception {
		configuration.setTwoFactorType(TwoFactorType.email);
		configuration.setTwoFactorEmailBody("Custom body");
		configuration.setTwoFactorEmailSubject("Custom subject");
		configuration.setTwofactorPushCodeTimeOutSeconds(Integer.valueOf(120));

		bizlet.preRerender(Configuration.twoFactorTypePropertyName, configuration, null);

		assertEquals("Custom body", configuration.getTwoFactorEmailBody());
		assertEquals("Custom subject", configuration.getTwoFactorEmailSubject());
		assertEquals(Integer.valueOf(120), configuration.getTwofactorPushCodeTimeOutSeconds());
	}

	@Test
	public void testPreRerenderTwoFactorTypeOffDoesNotSetDefaults() throws Exception {
		configuration.setTwoFactorType(TwoFactorType.off);
		configuration.setTwoFactorEmailBody(null);

		bizlet.preRerender(Configuration.twoFactorTypePropertyName, configuration, null);

		assertNull(configuration.getTwoFactorEmailBody());
	}

	@Test
	public void testPreRerenderUnknownSourceDoesNotModifyBean() throws Exception {
		configuration.setTwoFactorType(TwoFactorType.email);
		configuration.setTwoFactorEmailBody("existing body");

		bizlet.preRerender("someOtherSource", configuration, null);

		assertEquals("existing body", configuration.getTwoFactorEmailBody());
	}

}
