package org.skyve.impl.web;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.Assert.assertThrows;

import java.io.OutputStream;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.junit.After;
import org.junit.Test;
import org.skyve.impl.mail.MailServiceStaticSingleton;
import org.skyve.impl.mail.NoOpMailService;
import org.skyve.impl.util.UtilImpl;
import org.skyve.util.Mail;
import org.skyve.util.MailService;

public class SkyveContextListenerTest {
	private final MailService originalMailService = MailServiceStaticSingleton.get();
	private final String originalMailServiceClass = UtilImpl.SKYVE_MAIL_SERVICE_CLASS;
	private final String originalSmtp = UtilImpl.SMTP;
	private final int originalSmtpPort = UtilImpl.SMTP_PORT;
	private final String originalSmtpUid = UtilImpl.SMTP_UID;
	private final String originalSmtpPwd = UtilImpl.SMTP_PWD;
	private final Map<String, String> originalSmtpProperties = UtilImpl.SMTP_PROPERTIES;
	private final Map<String, String> originalSmtpHeaders = UtilImpl.SMTP_HEADERS;
	private final String originalSmtpSender = UtilImpl.SMTP_SENDER;
	private final String originalSmtpTestRecipient = UtilImpl.SMTP_TEST_RECIPIENT;
	private final boolean originalSmtpTestBogusSend = UtilImpl.SMTP_TEST_BOGUS_SEND;

	@After
	public void after() {
		MailServiceStaticSingleton.set(originalMailService);
		UtilImpl.SKYVE_MAIL_SERVICE_CLASS = originalMailServiceClass;
		UtilImpl.SMTP = originalSmtp;
		UtilImpl.SMTP_PORT = originalSmtpPort;
		UtilImpl.SMTP_UID = originalSmtpUid;
		UtilImpl.SMTP_PWD = originalSmtpPwd;
		UtilImpl.SMTP_PROPERTIES = originalSmtpProperties;
		UtilImpl.SMTP_HEADERS = originalSmtpHeaders;
		UtilImpl.SMTP_SENDER = originalSmtpSender;
		UtilImpl.SMTP_TEST_RECIPIENT = originalSmtpTestRecipient;
		UtilImpl.SMTP_TEST_BOGUS_SEND = originalSmtpTestBogusSend;
	}

	@Test
	@SuppressWarnings("static-method")
	public void testCleanupContentDirectoryHandlesEmptyStrings() throws Exception {
		// setup the test data
		final String test1 = null, test2 = "";

		// perform the method under test
		String result1 = SkyveContextListener.cleanupDirectory(test1);
		String result2 = SkyveContextListener.cleanupDirectory(test2);

		// verify the results
		assertThat(result1, is(test1));
		assertThat(result2, is(test2));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testCleanupContentDirectoryAppendsSlash() throws Exception {
		// setup the test data
		final String test1 = "C:/workspace/content",
				test2 = "C:/workspace/content/",
				test3 = "C:\\workspace\\content",
				test4 = "C:\\workspace\\content\\";
		final String expected = "C:/workspace/content/";

		// perform the method under test
		String result1 = SkyveContextListener.cleanupDirectory(test1);
		String result2 = SkyveContextListener.cleanupDirectory(test2);
		String result3 = SkyveContextListener.cleanupDirectory(test3);
		String result4 = SkyveContextListener.cleanupDirectory(test4);

		// verify the results
		assertThat(result1, is(expected));
		assertThat(result2, is(expected));
		assertThat(result3, is(expected));
		assertThat(result4, is(expected));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testCleanupModuleDirectoryHandlesEmptyStrings() throws Exception {
		// setup the test data
		final String test1 = null, test2 = "";

		// perform the method under test
		String result1 = UtilImpl.cleanupModuleDirectory(test1);
		String result2 = UtilImpl.cleanupModuleDirectory(test2);

		// verify the results
		assertThat(result1, is(test1));
		assertThat(result2, is(test2));
	}
	
	@Test
	@SuppressWarnings("static-method")
	public void testCleanupModuleDirectoryAppendsModules() throws Exception {
		// setup the test data
		final String test1 = "/src/main/java/",
				test2 = "/src/main/java",
				test3 = "\\src\\main\\java\\",
				test4 = "\\src\\main\\java";

		// perform the method under test
		String result1 = UtilImpl.cleanupModuleDirectory(test1);
		String result2 = UtilImpl.cleanupModuleDirectory(test2);
		String result3 = UtilImpl.cleanupModuleDirectory(test3);
		String result4 = UtilImpl.cleanupModuleDirectory(test4);

		// verify the results
		assertThat(result1, is(test1 + "modules/"));
		assertThat(result2, is(test2 + "/modules/"));
		assertThat(result3, is("\\src\\main\\java/modules/"));
		assertThat(result4, is(test4 + "/modules/"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testCleanupModuleDirectoryWithModulesAppendsSlash() throws Exception {
		// setup the test data
		final String test1 = "/src/main/java/modules",
				test2 = "/src/main/java/modules/",
				test3 = "\\src\\main\\java\\modules",
				test4 = "\\src\\main\\java\\modules\\";

		// perform the method under test
		String result1 = UtilImpl.cleanupModuleDirectory(test1);
		String result2 = UtilImpl.cleanupModuleDirectory(test2);
		String result3 = UtilImpl.cleanupModuleDirectory(test3);
		String result4 = UtilImpl.cleanupModuleDirectory(test4);

		// verify the results
		assertThat(result1, is(test1 + "/"));
		assertThat(result2, is(test2));
		assertThat(result3, is(test3 + "/"));
		assertThat(result4, is("\\src\\main\\java\\modules" + "/"));
	}

	@Test
	public void testConfigureMailServiceAndSmtpAllowsMissingSmtpForNonSMTPMailService() {
		Map<String, Object> properties = new HashMap<>();
		Map<String, Object> factories = new HashMap<>();
		factories.put("mailServiceClass", TestMailService.class.getName());

		SkyveContextListener.configureMailServiceAndSmtp(properties, factories);

		assertThat(MailServiceStaticSingleton.get(), instanceOf(TestMailService.class));
		assertThat(UtilImpl.SMTP, is((String) null));
		assertThat(UtilImpl.SMTP_PORT, is(0));
		assertThat(UtilImpl.SMTP_SENDER, is((String) null));
	}

	@Test
	public void testConfigureMailServiceAndSmtpRequiresSmtpForDefaultSMTPService() {
		Map<String, Object> properties = new HashMap<>();
		Map<String, Object> factories = new HashMap<>();

		assertThrows(IllegalStateException.class, () -> SkyveContextListener.configureMailServiceAndSmtp(properties, factories));
	}

	@Test
	public void testConfigureMailServiceAndSmtpReadsConfiguredSmtp() {
		Map<String, Object> properties = new HashMap<>();
		Map<String, Object> smtp = new HashMap<>();
		smtp.put("server", "localhost");
		smtp.put("port", Integer.valueOf(25));
		smtp.put("sender", "mailer@skyve.org");
		smtp.put("testBogusSend", Boolean.FALSE);
		properties.put("smtp", smtp);

		Map<String, Object> factories = new HashMap<>();
		factories.put("mailServiceClass", NoOpMailService.class.getName());

		SkyveContextListener.configureMailServiceAndSmtp(properties, factories);

		assertThat(UtilImpl.SMTP, is("localhost"));
		assertThat(UtilImpl.SMTP_PORT, is(25));
		assertThat(UtilImpl.SMTP_SENDER, is("mailer@skyve.org"));
	}

	public static class TestMailService implements MailService {
		@Override
		public void writeMail(Mail mail, OutputStream out) {
			// no-op
		}

		@Override
		public void sendMail(Mail mail) {
			// no-op
		}

		@Override
		public void sendBulkMail(List<Mail> mails) {
			// no-op
		}
	}
}
