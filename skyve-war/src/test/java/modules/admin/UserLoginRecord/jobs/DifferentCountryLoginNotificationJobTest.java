package modules.admin.UserLoginRecord.jobs;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;

import org.junit.jupiter.api.Test;

import modules.admin.Contact.ContactExtension;
import modules.admin.Startup.StartupExtension;
import modules.admin.UserLoginRecord.UserLoginRecordExtension;
import modules.admin.domain.Contact;
import modules.admin.domain.Startup;
import modules.admin.domain.UserLoginRecord;

@SuppressWarnings("static-method")
class DifferentCountryLoginNotificationJobTest {
	@Test
	void persistJobExecutionOnSuccessReturnsFalse() {
		assertFalse(new DifferentCountryLoginNotificationJob().persistJobExecutionOnSuccess());
	}

	@Test
	void executeWithNoCountryOnlyCompletes() throws Exception {
		TestableDifferentCountryLoginNotificationJob job = new TestableDifferentCountryLoginNotificationJob();
		job.setBean(loginRecord(null));

		job.execute();

		assertEquals(100, job.getPercentComplete());
		assertEquals(0, job.getLog().size());
		assertEquals(0, job.sentCount);
	}

	@Test
	void executeWithMissingSupportEmailLogsWarningAndDoesNotSend() throws Exception {
		TestableDifferentCountryLoginNotificationJob job = new TestableDifferentCountryLoginNotificationJob();
		job.startup.setEnvironmentSupportEmail(null);
		job.setBean(loginRecord("New Zealand"));

		job.execute();

		assertEquals(100, job.getPercentComplete());
		assertEquals(0, job.sentCount);
		assertThat(job.getLog().get(0), containsString("There is no environment support email specified"));
	}

	@Test
	void executeSendsNotificationWhenSupportEmailConfigured() throws Exception {
		TestableDifferentCountryLoginNotificationJob job = new TestableDifferentCountryLoginNotificationJob();
		job.setBean(loginRecord("New Zealand"));

		job.execute();

		assertEquals(100, job.getPercentComplete());
		assertEquals(1, job.sentCount);
		assertThat(job.getLog().get(0), containsString("Successfully sent email warning"));
		assertThat(job.getLog().get(0), containsString("person@example.com"));
	}

	@Test
	void executeLogsFailureWhenNotificationSendThrows() throws Exception {
		TestableDifferentCountryLoginNotificationJob job = new TestableDifferentCountryLoginNotificationJob();
		job.failSend = true;
		job.setBean(loginRecord("New Zealand"));

		job.execute();

		assertEquals(100, job.getPercentComplete());
		assertEquals(1, job.sentCount);
		assertThat(job.getLog().get(0), containsString("Failed to send security alert email"));
		assertThat(job.getLog().get(0), containsString("mail unavailable"));
	}

	private static UserLoginRecord loginRecord(String countryName) {
		TestLoginRecord loginRecord = new TestLoginRecord(countryName);
		loginRecord.setCountryCode("NZ");
		loginRecord.setIpAddress("203.0.113.10");
		return loginRecord;
	}

	private static class TestLoginRecord extends UserLoginRecordExtension {
		private static final long serialVersionUID = 4901134837596984889L;

		private final String countryName;

		private TestLoginRecord(String countryName) {
			this.countryName = countryName;
		}

		@Override
		public String getCountryName() {
			return countryName;
		}
	}

	private static class TestableDifferentCountryLoginNotificationJob extends DifferentCountryLoginNotificationJob {
		private final Contact contact = new ContactExtension();
		private final Startup startup = new StartupExtension();
		private int sentCount;
		private boolean failSend;

		private TestableDifferentCountryLoginNotificationJob() {
			contact.setName("Person Example");
			contact.setEmail1("person@example.com");
			startup.setEnvironmentSupportEmail("support@example.com");
		}

		@Override
		protected Contact getCurrentUserContact() {
			return contact;
		}

		@Override
		protected Startup newStartup() {
			return startup;
		}

		@Override
		protected void sendNotification(Contact contactToEmail, UserLoginRecord loginRecord, Startup startupForEmail) {
			sentCount++;
			if (failSend) {
				throw new IllegalStateException("mail unavailable");
			}
		}
	}
}
