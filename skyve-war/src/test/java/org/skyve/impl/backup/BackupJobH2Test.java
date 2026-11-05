package org.skyve.impl.backup;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.time.LocalDate;
import java.time.LocalTime;
import java.util.Map;
import java.util.UUID;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.content.AttachmentContent;
import org.skyve.domain.Bean;
import org.skyve.domain.app.admin.DataMaintenance.DataSensitivity;
import org.skyve.domain.types.DateOnly;
import org.skyve.domain.types.TimeOnly;
import org.skyve.impl.content.AbstractContentManager;
import org.skyve.impl.content.NoOpContentManager;
import org.skyve.impl.metadata.model.AbstractAttribute;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.model.Attribute.Sensitivity;
import org.skyve.util.Util;
import org.supercsv.io.CsvMapReader;
import org.supercsv.prefs.CsvPreference;

import modules.admin.domain.Contact;
import modules.admin.domain.DataMaintenance;
import modules.test.AbstractSkyveTest;
import modules.test.domain.AllAttributesPersistent;

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
	@SuppressWarnings({"java:S1854", "null"}) // the saved contact is not read again
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

	@Test
	@SuppressWarnings({"java:S1854", "null"}) // the saved bean is not read again
	void backupRedactsSensitiveDatesToTheFirstOfTheMonthAndSensitiveTimesToTheHour() throws Exception {
		AbstractAttribute dateAttribute = (AbstractAttribute) aapd.getAttribute(AllAttributesPersistent.datePropertyName);
		AbstractAttribute timeAttribute = (AbstractAttribute) aapd.getAttribute(AllAttributesPersistent.timePropertyName);
		Sensitivity previousDateSensitivity = dateAttribute.getSensitivity();
		Sensitivity previousTimeSensitivity = timeAttribute.getSensitivity();
		try {
			dateAttribute.setSensitivity(Sensitivity.secret);
			timeAttribute.setSensitivity(Sensitivity.secret);
			AllAttributesPersistent bean = Util.constructRandomInstance(u, m, aapd, 1);
			bean.setDate(new DateOnly(LocalDate.of(2024, 3, 15)));
			bean.setTime(new TimeOnly(10, 37, 42));
			bean = p.save(bean);
			String bizId = bean.getBizId();
			p.commit(false);
			p.begin();
			DataMaintenance options = DataMaintenance.newInstance();
			options.setDataSensitivity(DataSensitivity.secret);
			BackupJob job = new BackupJob();
			job.setBean(options);

			job.execute();

			Map<String, String> row = backedUpRow(job.getBackupZip(), aapd.getPersistent().getAgnosticIdentifier(), bizId);
			LocalDate date = new java.sql.Date(Long.parseLong(row.get(AllAttributesPersistent.datePropertyName))).toLocalDate();
			LocalTime time = new java.sql.Time(Long.parseLong(row.get(AllAttributesPersistent.timePropertyName))).toLocalTime();
			assertEquals(1, date.getDayOfMonth());
			assertEquals(0, time.getMinute());
			assertEquals(0, time.getSecond());
		}
		finally {
			dateAttribute.setSensitivity(previousDateSensitivity);
			timeAttribute.setSensitivity(previousTimeSensitivity);
		}
	}

	/**
	 * Reads the row of a table's CSV file within a backup zip.
	 */
	private static Map<String, String> backedUpRow(File backupZip, String agnosticIdentifier, String bizId) throws Exception {
		try (ZipFile zip = new ZipFile(backupZip)) {
			ZipEntry entry = zip.stream()
									.filter(e -> e.getName().endsWith(agnosticIdentifier + ".csv"))
									.findFirst()
									.orElseThrow();
			try (CsvMapReader reader = new CsvMapReader(new InputStreamReader(zip.getInputStream(entry), StandardCharsets.UTF_8),
															CsvPreference.STANDARD_PREFERENCE)) {
				String[] headers = reader.getHeader(true);
				Map<String, String> row = reader.read(headers);
				while (row != null) {
					if (bizId.equals(row.get(Bean.DOCUMENT_ID))) {
						return row;
					}
					row = reader.read(headers);
				}
			}
		}
		throw new AssertionError("No row for " + bizId + " was backed up for " + agnosticIdentifier);
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
