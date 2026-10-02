package org.skyve.impl.backup;

import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.UUID;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.content.AttachmentContent;
import org.skyve.impl.content.AbstractContentManager;
import org.skyve.impl.content.NoOpContentManager;
import org.skyve.impl.util.UtilImpl;

import modules.admin.domain.Contact;
import modules.test.AbstractSkyveTest;

class BackupJobH2Test extends AbstractSkyveTest {
	private Class<? extends AbstractContentManager> previousContentManagerClass;
	private String previousSupportEmailAddress;

	@BeforeEach
	void configureContentManager() {
		previousContentManagerClass = AbstractContentManager.IMPLEMENTATION_CLASS;
		previousSupportEmailAddress = UtilImpl.SUPPORT_EMAIL_ADDRESS;

		AbstractContentManager.IMPLEMENTATION_CLASS = MissingFileContentManager.class;
		UtilImpl.SUPPORT_EMAIL_ADDRESS = null;
	}

	@AfterEach
	void restoreContentManager() {
		AbstractContentManager.IMPLEMENTATION_CLASS = previousContentManagerClass;
		UtilImpl.SUPPORT_EMAIL_ADDRESS = previousSupportEmailAddress;
	}

	@Test
	@SuppressWarnings("java:S1854") // the saved contact is not read again
	void backupFlagsProblemsWhenAContentFileIsMissingFromTheContentStore() throws Exception {
		Contact contact = Contact.newInstance();
		contact.setName("Missing content file");
		contact.setImage(UUID.randomUUID().toString());
		contact = p.save(contact);
		p.commit(false);
		p.begin();
		BackupJob job = new BackupJob();

		job.execute();

		File backupZip = job.getBackupZip();
		assertTrue(backupZip.getName().endsWith("_PROBLEMS.zip"));
		assertTrue(job.getLog().stream().anyMatch(entry -> entry.contains("is missing a file in the content store for attribute [" +
																			Contact.imagePropertyName + "]")));
	}

	/**
	 * Simulates a content store whose index entry exists but whose file has gone.
	 */
	public static class MissingFileContentManager extends NoOpContentManager {
		@Override
		public AttachmentContent getAttachment(String contentId) throws Exception {
			throw new FileNotFoundException(contentId);
		}
	}
}
