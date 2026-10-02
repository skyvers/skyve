package org.skyve.impl.backup;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.not;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.lang.reflect.Method;
import java.io.File;
import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.IntStream;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.skyve.domain.Bean;
import org.skyve.domain.app.admin.DataMaintenance;
import org.skyve.domain.app.admin.DataMaintenance.DataSensitivity;
import org.skyve.impl.mail.MailServiceStaticSingleton;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.model.Attribute.Sensitivity;
import org.skyve.util.Mail;
import org.skyve.util.MailService;

@SuppressWarnings("static-method")
class BackupJobTest {
	private final CaptureMailService capture = new CaptureMailService();

	private MailService originalMailService;
	private String originalArchiveName;
	private String originalEnvironmentIdentifier;
	private String originalSupportEmailAddress;
	private String originalSmtpSender;
	private String originalSmtpTestRecipient;
	private boolean originalSmtpTestBogusSend;

	@BeforeEach
	void beforeEach() {
		MailServiceStaticSingleton.setDefault();
		originalMailService = MailServiceStaticSingleton.get();
		originalArchiveName = UtilImpl.ARCHIVE_NAME;
		originalEnvironmentIdentifier = UtilImpl.ENVIRONMENT_IDENTIFIER;
		originalSupportEmailAddress = UtilImpl.SUPPORT_EMAIL_ADDRESS;
		originalSmtpSender = UtilImpl.SMTP_SENDER;
		originalSmtpTestRecipient = UtilImpl.SMTP_TEST_RECIPIENT;
		originalSmtpTestBogusSend = UtilImpl.SMTP_TEST_BOGUS_SEND;

		MailServiceStaticSingleton.set(capture);
		UtilImpl.ARCHIVE_NAME = "SkyveTest";
		UtilImpl.ENVIRONMENT_IDENTIFIER = "TEST";
		UtilImpl.SUPPORT_EMAIL_ADDRESS = "support@skyve.org";
		UtilImpl.SMTP_SENDER = "noreply@skyve.org";
		UtilImpl.SMTP_TEST_RECIPIENT = null;
		UtilImpl.SMTP_TEST_BOGUS_SEND = false;
	}

	@AfterEach
	void afterEach() {
		MailServiceStaticSingleton.set(originalMailService);
		UtilImpl.ARCHIVE_NAME = originalArchiveName;
		UtilImpl.ENVIRONMENT_IDENTIFIER = originalEnvironmentIdentifier;
		UtilImpl.SUPPORT_EMAIL_ADDRESS = originalSupportEmailAddress;
		UtilImpl.SMTP_SENDER = originalSmtpSender;
		UtilImpl.SMTP_TEST_RECIPIENT = originalSmtpTestRecipient;
		UtilImpl.SMTP_TEST_BOGUS_SEND = originalSmtpTestBogusSend;
	}

	@Test
	void testEmailProblemUsesExtMailService() {
		List<String> jobLog = new ArrayList<>();

		BackupJob.emailProblem(jobLog, "simulated failure");

		assertEquals(1, capture.sendCount);
		assertTrue(capture.lastSend.getRecipientEmailAddresses().contains("support@skyve.org"));
		assertThat(capture.lastSend.getSenderEmailAddress(), is("noreply@skyve.org"));
		assertThat(capture.lastSend.getSubject(), is("[SkyveTest - TEST] Backup Problem"));
		assertThat(capture.lastSend.getBody(), containsString("a problem:- simulated failure"));
		assertThat(capture.lastSend.getBody(), containsString("See the backup job log (admin -> Jobs) for details."));
	}

	@Test
	void logProblemsLogsNothingWhenThereAreNoProblems(@TempDir Path tempDir) throws Exception {
		List<String> jobLog = new ArrayList<>();

		assertEquals(0, BackupJob.logProblems(writeProblems(tempDir, 0), jobLog));
		assertTrue(jobLog.isEmpty());
	}

	@Test
	void logProblemsLogsSummaryThenEachProblem(@TempDir Path tempDir) throws Exception {
		List<String> jobLog = new ArrayList<>();

		assertEquals(3, BackupJob.logProblems(writeProblems(tempDir, 3), jobLog));
		assertEquals(List.of("3 problems recorded in problems.txt", "problem 1", "problem 2", "problem 3"), jobLog);
	}

	@Test
	void logProblemsCapsLoggedProblems(@TempDir Path tempDir) throws Exception {
		List<String> jobLog = new ArrayList<>();

		assertEquals(150, BackupJob.logProblems(writeProblems(tempDir, 150), jobLog));
		assertEquals(102, jobLog.size());
		assertEquals("150 problems recorded in problems.txt", jobLog.get(0));
		assertEquals("problem 100", jobLog.get(100));
		assertEquals("... and 50 more", jobLog.get(101));
	}

	@Test
	void newBackupJobHasNoGeneratedZip() {
		BackupJob job = new BackupJob();

		assertEquals(null, job.getBackupZip());
	}

		@Test
		void testEmailProblemWithNullProblemSendsGenericMessage() {
                List<String> jobLog = new ArrayList<>();

                BackupJob.emailProblem(jobLog, null);

                assertEquals(1, capture.sendCount);
                assertThat(capture.lastSend.getBody(), containsString("problems."));
                assertEquals(0, jobLog.size(), "No log entry when support email is set");
        }

		@Test
		void testEmailProblemWithNoSupportEmailLogsToJobLog() {
                UtilImpl.SUPPORT_EMAIL_ADDRESS = null;
                List<String> jobLog = new ArrayList<>();

                BackupJob.emailProblem(jobLog, "disk full");

                assertEquals(0, capture.sendCount, "No email should be sent when no support address");
                assertEquals(1, jobLog.size(), "Problem should be logged when no support address");
                assertThat(jobLog.get(0), containsString("disk full"));
        }

		@Test
		void testEmailProblemWithNullEnvironmentIdentifierOmitsEnvSuffix() {
                UtilImpl.ENVIRONMENT_IDENTIFIER = null;
                List<String> jobLog = new ArrayList<>();

                BackupJob.emailProblem(jobLog, "test problem");

                assertEquals(1, capture.sendCount);
                assertThat(capture.lastSend.getSubject(), is("[SkyveTest] Backup Problem"));
			assertThat(capture.lastSend.getSubject(), not(containsString(" - ")));
	}

	@Test
	void backupOptionsDefaultWhenBeanIsNotDataMaintenance() throws Exception {
		Bean bean = mock(Bean.class);

		assertEquals(0, invokeGetSensitivityLevel(bean));
		assertTrue(invokeGetIncludeContent(bean));
		assertTrue(invokeGetIncludeAuditLog(bean));
	}

	@Test
	void backupOptionsReadDataMaintenanceSelections() throws Exception {
		DataMaintenance bean = mock(DataMaintenance.class);
		when(bean.getDataSensitivity()).thenReturn(DataSensitivity.restricted);
		when(bean.getIncludeContent()).thenReturn(Boolean.TRUE);
		when(bean.getIncludeAuditLog()).thenReturn(Boolean.FALSE);

		assertEquals(Sensitivity.restricted.ordinal(), invokeGetSensitivityLevel(bean));
		assertTrue(invokeGetIncludeContent(bean));
		assertFalse(invokeGetIncludeAuditLog(bean));
	}

	@Test
	void backupOptionsTreatNullDataMaintenanceSelectionsAsDisabledOrNone() throws Exception {
		DataMaintenance bean = mock(DataMaintenance.class);

		assertEquals(0, invokeGetSensitivityLevel(bean));
		assertFalse(invokeGetIncludeContent(bean));
		assertFalse(invokeGetIncludeAuditLog(bean));
	}

	@Test
	void formatSizeUsesMegabytesBelowOneGigabyteAndGigabytesFromThere() throws Exception {
		// Expected values are formatted the same way so the test holds in any default locale
		assertEquals(String.format("%,.1f MB", Double.valueOf(1023)), invokeFormatSize(1023L * 1024 * 1024));
		assertEquals(String.format("%,.1f GB", Double.valueOf(1)), invokeFormatSize(1024L * 1024 * 1024));
		assertEquals(String.format("%,.1f GB", Double.valueOf(1.5)), invokeFormatSize(1536L * 1024 * 1024));
	}

	private static String invokeFormatSize(long bytes) throws Exception {
		Method method = BackupJob.class.getDeclaredMethod("formatSize", long.class);
		method.setAccessible(true);
		return (String) method.invoke(null, Long.valueOf(bytes));
	}

	private static File writeProblems(Path dir, int count) throws Exception {
		Path problemsTxt = dir.resolve("problems.txt");
		Files.write(problemsTxt, IntStream.rangeClosed(1, count).mapToObj(i -> "problem " + i).toList());
		return problemsTxt.toFile();
	}

	private static int invokeGetSensitivityLevel(Bean bean) throws Exception {
		Method method = BackupJob.class.getDeclaredMethod("getSensitivityLevel", Bean.class);
		method.setAccessible(true);
		return ((Integer) method.invoke(null, bean)).intValue();
	}

	private static boolean invokeGetIncludeContent(Bean bean) throws Exception {
		Method method = BackupJob.class.getDeclaredMethod("getIncludeContent", Bean.class);
		method.setAccessible(true);
		return ((Boolean) method.invoke(null, bean)).booleanValue();
	}

	private static boolean invokeGetIncludeAuditLog(Bean bean) throws Exception {
		Method method = BackupJob.class.getDeclaredMethod("getIncludeAuditLog", Bean.class);
		method.setAccessible(true);
		return ((Boolean) method.invoke(null, bean)).booleanValue();
	}

	private static class CaptureMailService implements MailService {
		int sendCount = 0;
		Mail lastSend = null;

		@Override
		public void sendMail(@jakarta.annotation.Nonnull Mail mail) {
			lastSend = mail;
			sendCount++;
		}

		@Override
		public void sendBulkMail(@jakarta.annotation.Nonnull List<Mail> mails) {
			// no-op
		}

		@Override
		public void writeMail(@jakarta.annotation.Nonnull Mail mail, @jakarta.annotation.Nonnull OutputStream out) {
			// no-op
		}
	}
}
