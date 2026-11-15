package modules.admin.DataMaintenance;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.File;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.domain.app.admin.DataMaintenance.DataSensitivity;
import org.skyve.impl.backup.BackupJob;
import org.skyve.impl.backup.RestoreJob;
import org.skyve.persistence.Persistence;

import modules.admin.User.UserExtension;
import modules.admin.domain.Contact;
import modules.admin.domain.Contact.ContactType;
import modules.admin.domain.DataMaintenance;
import modules.admin.domain.DataMaintenance.ContentRestoreOption;
import modules.admin.domain.DataMaintenance.RestorePreProcess;
import modules.admin.domain.User;
import util.AbstractH2Test;

/**
 * Backs up and restores the data store, to check what a restore leaves behind: the shared (second-level) cache, and
 * the user passwords a redacted backup holds.
 */
class RestoreJobH2Test extends AbstractH2Test {
	private final List<File> backups = new ArrayList<>();

	@AfterEach
	void deleteBackups() throws Exception {
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
	}

	/**
	 * A backup with a data sensitivity above none redacts every password (sensitivity secret). Its restore must say so,
	 * and the redacted passwords must fail a sign in as bad credentials: they threw IllegalArgumentException at sign in.
	 */
	@Test
	void aRedactedBackupRestoresWithAWarningAndPasswordsThatMatchNothing() throws Exception {
		Persistence p = CORE.getPersistence();
		p.commit(false); // commit the admin user, so the backup has a password to redact
		p.begin();

		RestoreJob job = restore(backup(DataSensitivity.personal));

		List<String> log = job.getLog();
		assertTrue(log.stream().anyMatch(line -> line.matches("WARNING: This backup .+ redacted the passwords of [1-9]\\d* user\\(s\\)\\. .+")),
					log.toString());
		p.evictAllCached();
		String restoredPassword = adminUser(p).getPassword();
		assertNotNull(restoredPassword);
		assertFalse(restoredPassword.contains("}"), "the backup redacted the password: " + restoredPassword);
		assertFalse(EXT.checkPassword(PASSWORD, restoredPassword));
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
