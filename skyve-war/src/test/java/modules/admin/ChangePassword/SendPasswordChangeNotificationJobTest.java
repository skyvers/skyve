package modules.admin.ChangePassword;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;

import org.junit.jupiter.api.Test;

import modules.admin.Configuration.ConfigurationExtension;
import modules.admin.Contact.ContactExtension;
import modules.admin.Startup.StartupExtension;
import modules.admin.User.UserExtension;
import modules.admin.domain.Configuration;
import modules.admin.domain.Startup;
import modules.admin.domain.User;
import util.AbstractH2Test;

@SuppressWarnings("static-method")
class SendPasswordChangeNotificationJobTest extends AbstractH2Test {
	@Test
	void persistJobExecutionOnSuccessReturnsFalse() {
		assertFalse(new SendPasswordChangeNotificationJob().persistJobExecutionOnSuccess());
	}

	@Test
	void executeWithoutConfiguredEmailOrUserLogsFailureAndStopsAtZeroPercent() throws Exception {
		SendPasswordChangeNotificationJob job = new SendPasswordChangeNotificationJob();

		job.execute();

		assertEquals(0, job.getPercentComplete());
		assertEquals(1, job.getLog().size());
		assertThat(job.getLog().get(0), containsString("Failed to send password change notification"));
	}

	@Test
	void executeWithConfiguredEmailButNoUserLogsFailure() throws Exception {
		TestableSendPasswordChangeNotificationJob job = new TestableSendPasswordChangeNotificationJob();

		job.execute();

		assertEquals(0, job.getPercentComplete());
		assertEquals(1, job.getLog().size());
		assertThat(job.getLog().get(0), containsString("No user has been parsed"));
		assertEquals(0, job.sentCount);
	}

	@Test
	void executeWithMissingSupportEmailLogsFailure() throws Exception {
		TestableSendPasswordChangeNotificationJob job = new TestableSendPasswordChangeNotificationJob();
		job.startup.setEnvironmentSupportEmail(null);
		job.setBean(user("Person Example"));

		job.execute();

		assertEquals(0, job.getPercentComplete());
		assertEquals(1, job.getLog().size());
		assertThat(job.getLog().get(0), containsString("There is no environment support email specified"));
		assertEquals(0, job.sentCount);
	}

	@Test
	void executeSendsDefaultNotificationWhenGeoIpIsNotBlocking() throws Exception {
		TestableSendPasswordChangeNotificationJob job = new TestableSendPasswordChangeNotificationJob();
		job.geoIPBlocking = false;
		job.setBean(user("Person Example"));

		job.execute();

		assertEquals(100, job.getPercentComplete());
		assertEquals(1, job.sentCount);
		assertEquals("default", job.sentTemplate);
		assertThat(job.getLog().get(0), containsString("Successfully sent password change notification to Person Example"));
	}

	@Test
	void executeSendsGeoIpNotificationWhenGeoIpIsBlocking() throws Exception {
		TestableSendPasswordChangeNotificationJob job = new TestableSendPasswordChangeNotificationJob();
		job.geoIPBlocking = true;
		job.setBean(user("Person Example"));

		job.execute();

		assertEquals(100, job.getPercentComplete());
		assertEquals(1, job.sentCount);
		assertEquals("geoip", job.sentTemplate);
	}

	@Test
	void executeLogsFailureWhenNotificationSendThrows() throws Exception {
		TestableSendPasswordChangeNotificationJob job = new TestableSendPasswordChangeNotificationJob();
		job.failSend = true;
		job.setBean(user("Person Example"));

		job.execute();

		assertEquals(100, job.getPercentComplete());
		assertEquals(1, job.sentCount);
		assertThat(job.getLog().get(0), containsString("Failed to send password change notification to Person Example"));
	}

	private static User user(String name) {
		UserExtension user = new UserExtension();
		ContactExtension contact = new ContactExtension();
		contact.setName(name);
		contact.setEmail1("person@example.com");
		user.setContact(contact);
		return user;
	}

	private static class TestableSendPasswordChangeNotificationJob extends SendPasswordChangeNotificationJob {
		private final Configuration configuration = new ConfiguredEmailConfiguration();
		private final StartupExtension startup = new StartupExtension();
		private boolean geoIPBlocking;
		private boolean failSend;
		private int sentCount;
		private String sentTemplate;

		private TestableSendPasswordChangeNotificationJob() {
			startup.setEnvironmentSupportEmail("support@example.com");
		}

		@Override
		protected Configuration newConfiguration() {
			return configuration;
		}

		@Override
		protected Startup newStartup() {
			return startup;
		}

		@Override
		protected boolean isGeoIPBlocking() {
			return geoIPBlocking;
		}

		@Override
		protected void sendGeoIPNotification(User user, Startup startupBean) {
			send("geoip");
		}

		@Override
		protected void sendNotification(User user, Startup startupBean) {
			send("default");
		}

		private void send(String template) {
			sentCount++;
			sentTemplate = template;
			if (failSend) {
				throw new IllegalStateException("mail unavailable");
			}
		}
	}

	private static class ConfiguredEmailConfiguration extends ConfigurationExtension {
		private static final long serialVersionUID = 7481497729799857311L;

		@Override
		public boolean isEmailConfigured() {
			return true;
		}
	}
}
