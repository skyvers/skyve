package modules.admin.DataMaintenance;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.File;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.domain.app.admin.DataMaintenance.DataSensitivity;
import org.skyve.impl.backup.BackupJob;
import org.skyve.impl.backup.RestoreJob;
import org.skyve.persistence.Persistence;
import org.skyve.util.PushMessage;
import org.skyve.util.PushMessage.PushMessageReceiver;

import modules.admin.User.UserExtension;
import modules.admin.domain.Contact;
import modules.admin.domain.Contact.ContactType;
import modules.admin.domain.DataMaintenance;
import modules.admin.domain.DataMaintenance.ContentRestoreOption;
import modules.admin.domain.DataMaintenance.RestorePreProcess;
import modules.admin.domain.User;
import util.AbstractH2TestDispose;

/**
 * Backs up and restores the data store, to check what a restore leaves behind: the shared (second-level) cache, and
 * the user names and passwords a redacted backup holds. The backups and restores commit, so each test disposes of
 * the database.
 */
class RestoreJobH2Test extends AbstractH2TestDispose {
	private final List<File> backups = new ArrayList<>();
	/** The restore's completion growls, as "severity: message". */
	private final List<String> restoreGrowls = new ArrayList<>();
	/** Receives the growls the jobs push to every user, as a signed in user's browser would. */
	private final PushMessageReceiver growlReceiver = new PushMessageReceiver() {
		@Override
		public String forUserId() {
			return "RestoreJobH2Test";
		}

		@Override
		public void sendMessage(PushMessage message) {
			for (Map<String, Object> item : message.getItems()) {
				Object text = item.get("message");
				if ("g".equals(item.get("type")) && String.valueOf(text).startsWith("System Restore complete")) {
					restoreGrowls.add(item.get("severity") + ": " + text);
				}
			}
		}
	};

	@BeforeEach
	void receiveGrowls() {
		PushMessage.RECEIVERS.add(growlReceiver);
	}

	@AfterEach
	void deleteBackups() throws Exception {
		PushMessage.RECEIVERS.remove(growlReceiver);
		for (File backup : backups) {
			Files.deleteIfExists(backup.toPath());
		}
	}

	/**
	 * A restore writes the tables with JDBC. When it left Hibernate's shared cache as it was, the next save of a
	 * cached bean (Update My Details saves the user's Contact) failed with an optimistic lock, as the cache had a later
	 * version than the restored row.
	 */
	@Test
	void aSaveAfterARestoreIsNotAnOptimisticLockFailure() throws Exception {
		Persistence p = CORE.getPersistence();
		Contact contact = Contact.newInstance();
		contact.setName("Restore cache");
		contact.setContactType(ContactType.person);
		contact = p.save(contact);
		p.commit(false);
		p.begin();

		Integer backedUpVersion = contact.getBizVersion();
		File backup = backup(DataSensitivity.none);

		// Change the contact after the backup, so the cache has a later version than the backup's row
		contact.setMobile("0400 000 001");
		contact = p.save(contact);
		p.commit(false);
		p.begin();
		p.evictAllCached();
		Contact cached = p.retrieve(Contact.MODULE_NAME, Contact.DOCUMENT_NAME, contact.getBizId());
		assertNotNull(cached);
		assertTrue(p.sharedCacheBean(cached), "the contact is in the shared cache before the restore");

		RestoreJob job = restore(backup);

		p.begin(); // the restore's reindex commits
		p.evictAllCached(); // a new request starts with an empty session
		Contact restored = p.retrieve(Contact.MODULE_NAME, Contact.DOCUMENT_NAME, contact.getBizId());
		assertNotNull(restored);
		assertEquals(backedUpVersion, restored.getBizVersion(), "the restored version, not the cached one");
		assertNull(restored.getMobile(), "the restored mobile, not the cached one");
		restored.setMobile("0400 000 002");
		p.save(restored); // threw OptimisticLockException
		p.commit(false);
		p.begin();

		// A backup with no redaction keeps the passwords, and says nothing about them
		assertFalse(job.getLog().stream().anyMatch(line -> line.contains("redacted the passwords")), job.getLog().toString());
		assertTrue(EXT.checkPassword(PASSWORD, adminUser(p).getPassword()));
		assertEquals(List.of("info: System Restore complete."), restoreGrowls);
	}

	/**
	 * A backup with a data sensitivity above none redacts every password (sensitivity secret), and one from internal
	 * to personal redacts the user names too (sensitivity personal). Its restore must say both, and the redacted
	 * passwords must fail a sign in as bad credentials: they threw IllegalArgumentException at sign in.
	 */
	@Test
	void aRedactedBackupRestoresWithAWarningAndPasswordsThatMatchNothing() throws Exception {
		Persistence p = CORE.getPersistence();
		p.commit(false); // commit the admin user, so the backup has a user name and password to redact
		p.begin();

		RestoreJob job = restore(backup(DataSensitivity.personal));

		assertWarning(job,
						" redacted the passwords of [1-9]\\d* user\\(s\\) and the user names of [1-9]\\d* user\\(s\\)\\. \\S");
		p.evictAllCached();
		UserExtension restored = adminUser(p);
		String restoredPassword = restored.getPassword();
		assertNotNull(restoredPassword);
		assertFalse(restoredPassword.contains("}"), "the backup redacted the password: " + restoredPassword);
		assertFalse(EXT.checkPassword(PASSWORD, restoredPassword));
		assertNotEquals(USER, restored.getUserName(), "the backup redacted the user name");
		assertGrowl("the passwords of [1-9]\\d* user\\(s\\) and the user names of [1-9]\\d* user\\(s\\), " +
						"which must be set again");
	}

	/**
	 * A backup with data sensitivity secret redacts the passwords only, so its restore's warning says nothing about
	 * user names.
	 */
	@Test
	void aSecretBackupRestoresWithAWarningAboutPasswordsOnly() throws Exception {
		Persistence p = CORE.getPersistence();
		p.commit(false); // commit the admin user, so the backup has a password to redact
		p.begin();

		RestoreJob job = restore(backup(DataSensitivity.secret));

		assertWarning(job,
						" redacted the passwords of [1-9]\\d* user\\(s\\)\\. They cannot sign in until their passwords are reset:");
		assertFalse(job.getLog().stream().anyMatch(line -> line.contains("user names")), job.getLog().toString());
		p.evictAllCached();
		UserExtension restored = adminUser(p);
		assertEquals(USER, restored.getUserName(), "the backup kept the user name");
		String restoredPassword = restored.getPassword();
		assertNotNull(restoredPassword);
		assertFalse(EXT.checkPassword(PASSWORD, restoredPassword));
		assertGrowl("the passwords of [1-9]\\d* user\\(s\\), which must be reset");
	}

	/**
	 * Asserts that the restore pushed one warning growl about the redacted backup.
	 *
	 * @param redacted what the growl must say the backup redacted, as a regular expression
	 */
	private void assertGrowl(String redacted) {
		assertEquals(1, restoreGrowls.size(), restoreGrowls.toString());
		String growl = restoreGrowls.get(0);
		assertTrue(Pattern.matches("warn: System Restore complete, but the backup redacted " + redacted +
										": see the restore job's log\\.", growl),
					growl);
	}

	/**
	 * Asserts that the restore job's log has the redacted backup's warning.
	 *
	 * @param job the restore job that ran
	 * @param regex what the warning must contain
	 */
	private static void assertWarning(RestoreJob job, String regex) {
		List<String> log = job.getLog();
		Pattern redacted = Pattern.compile(regex);
		assertTrue(log.stream().anyMatch(line -> line.startsWith("WARNING: This backup ") && redacted.matcher(line).find()),
					log.toString());
	}

	private File backup(DataSensitivity sensitivity) throws Exception {
		BackupJob job = new BackupJob();
		DataMaintenance bean = DataMaintenance.newInstance();
		bean.setDataSensitivity(sensitivity);
		job.setBean(bean);
		job.execute();
		File result = job.getBackupZip();
		assertNotNull(result);
		backups.add(result);
		return result;
	}

	private static RestoreJob restore(File backup) throws Exception {
		RestoreJob job = new RestoreJob();
		DataMaintenance bean = DataMaintenance.newInstance();
		bean.setSelectedBackupName(backup.getName());
		bean.setContentRestoreOption(ContentRestoreOption.error);
		bean.setRestorePreProcess(RestorePreProcess.deleteExistingTableDataUsingMetadata);
		job.setBean(bean);
		job.execute();
		return job;
	}

	private static UserExtension adminUser(Persistence p) {
		UserExtension result = p.retrieve(User.MODULE_NAME, User.DOCUMENT_NAME, USER);
		assertNotNull(result);
		return result;
	}
}
