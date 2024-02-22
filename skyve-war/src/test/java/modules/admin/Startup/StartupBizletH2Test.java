package modules.admin.Startup;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.domain.messages.ValidationException;
import org.skyve.util.BeanValidator;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import jakarta.inject.Inject;
import modules.admin.domain.Startup;
import util.AbstractH2Test;

public class StartupBizletH2Test extends AbstractH2Test {

	@Inject
	private StartupBizlet bizlet;

	private DataBuilder db;
	private StartupExtension bean;

	@BeforeEach
	public void startup() throws Exception {
		db = new DataBuilder().fixture(FixtureType.crud);

		bean = Startup.newInstance();
	}

	@Test
	@SuppressWarnings("boxing")
	public void testValidateBackupDirectoryNameMinimumLength() throws Exception {
		// setup the test data
		bean.setBackupDirectoryName("aa");
		ValidationException e = new ValidationException();

		// call the method under test
		bizlet.validate(bean, e);

		// verify the result
		assertThat(e.getMessages().size(), is(1));
		assertThat(e.getMessages().get(0).getText(), containsString("3 characters"));
	}

	@Test
	@SuppressWarnings("boxing")
	public void testValidateBackupDirectoryNameExceedsMaximumLength() throws Exception {
		// setup the test data
		bean.setBackupDirectoryName("1234567890123456789012345678901234567890123456789012345678901234567890");
		ValidationException e = new ValidationException();

		// call the method under test
		bizlet.validate(bean, e);

		// verify the result
		assertThat(e.getMessages().size(), is(1));
		assertThat(e.getMessages().get(0).getText(), containsString("63 characters"));
	}

	@Test
	@SuppressWarnings("boxing")
	public void testValidateBackupDirectoryNameAgainstMetadataRegexInvalidCharacters() throws Exception {
		// setup the test data
		bean = db.build(Startup.MODULE_NAME, Startup.DOCUMENT_NAME);
		bean.setBackupDirectoryName("container@name");

		// call the method under test
		ValidationException result = Assertions.assertThrows(ValidationException.class, () -> {
			BeanValidator.validateBeanAgainstDocument(bean);
			BeanValidator.validateBeanAgainstBizlet(bean);
		});

		// verify the result
		assertThat(result.getMessages().size(), is(1));
		assertThat(result.getMessages().get(0).getText(), containsString("valid DNS name"));
	}

	@Test
	@SuppressWarnings("boxing")
	public void testValidateBackupDirectoryNameAgainstMetadataRegexUpperCaseLetters() throws Exception {
		// setup the test data
		bean = db.build(Startup.MODULE_NAME, Startup.DOCUMENT_NAME);
		bean.setBackupDirectoryName("MyContainer");

		// call the method under test
		ValidationException result = Assertions.assertThrows(ValidationException.class, () -> {
			BeanValidator.validateBeanAgainstDocument(bean);
			BeanValidator.validateBeanAgainstBizlet(bean);
		});

		// verify the result
		assertThat(result.getMessages().size(), is(1));
		assertThat(result.getMessages().get(0).getText(), containsString("valid DNS name"));
	}

	@Test
	@SuppressWarnings("boxing")
	public void testValidateBackupDirectoryNameAgainstMetadataRegexEndingWithDash() throws Exception {
		// setup the test data
		bean = db.build(Startup.MODULE_NAME, Startup.DOCUMENT_NAME);
		bean.setBackupDirectoryName("container-");

		// call the method under test
		ValidationException result = Assertions.assertThrows(ValidationException.class, () -> {
			BeanValidator.validateBeanAgainstDocument(bean);
			BeanValidator.validateBeanAgainstBizlet(bean);
		});

		// verify the result
		assertThat(result.getMessages().size(), is(1));
		assertThat(result.getMessages().get(0).getText(), containsString("valid DNS name"));
	}

	@Test
	@SuppressWarnings("boxing")
	public void testValidateBackupDirectoryNameAgainstMetadataRegexStartingWithDash() throws Exception {
		// setup the test data
		bean = db.build(Startup.MODULE_NAME, Startup.DOCUMENT_NAME);
		bean.setBackupDirectoryName("-container");

		// call the method under test
		ValidationException result = Assertions.assertThrows(ValidationException.class, () -> {
			BeanValidator.validateBeanAgainstDocument(bean);
			BeanValidator.validateBeanAgainstBizlet(bean);
		});

		// verify the result
		assertThat(result.getMessages().size(), is(1));
		assertThat(result.getMessages().get(0).getText(), containsString("valid DNS name"));
	}

	@Test
	@SuppressWarnings("boxing")
	public void testValidateBackupDirectoryNameAgainstMetadataRegexConsecutiveDashes() throws Exception {
		// setup the test data
		bean = db.build(Startup.MODULE_NAME, Startup.DOCUMENT_NAME);
		bean.setBackupDirectoryName("my--container");

		// call the method under test
		ValidationException result = Assertions.assertThrows(ValidationException.class, () -> {
			BeanValidator.validateBeanAgainstDocument(bean);
			BeanValidator.validateBeanAgainstBizlet(bean);
		});

		// verify the result
		assertThat(result.getMessages().size(), is(1));
		assertThat(result.getMessages().get(0).getText(), containsString("valid DNS name"));
	}

	@Test
	public void testValidateBackupDirectoryNameAgainstMetadataRegexValidName() throws Exception {
		// setup the test data
		bean = db.build(Startup.MODULE_NAME, Startup.DOCUMENT_NAME);
		bean.setBackupDirectoryName("my-container-1");

		// call the method under test
		Assertions.assertDoesNotThrow(() -> {
			BeanValidator.validateBeanAgainstDocument(bean);
			BeanValidator.validateBeanAgainstBizlet(bean);
		});
	}
}
