package modules.admin.DataMaintenance.actions;

import java.io.File;
import java.io.InputStream;
import java.util.List;
import java.util.stream.Collectors;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;

import org.apache.commons.io.IOUtils;
import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.content.MimeType;
import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.MessageSeverity;
import org.skyve.domain.messages.ValidationException;
import org.skyve.metadata.controller.Download;
import org.skyve.metadata.controller.DownloadAction;
import org.skyve.util.PushMessage;
import org.skyve.util.Util;
import org.skyve.web.WebContext;

import modules.admin.domain.DataMaintenance;

public class DownloadProblems extends DownloadAction<DataMaintenance> {

	private final String PROBLEMS_FILE_NAME = "problems.txt";
	private final String BACKUP_STRING = "backup_";
	private final String BACKUP_HAS_NO_PROBLEMS_FILE_ERROR = "The selected backup has no problems.txt file inside.";
	private final String BACKUP_HAS_NO_PROBLEMS = "No problems exist in this backup.";

	@Override
	public void prepare(DataMaintenance bean, WebContext webContext) throws Exception {
		// Check problems exist for the selected backup
		String selectedBackupName = bean.getSelectedBackupName();

		@SuppressWarnings("resource")
		ZipFile zipFile = null;
		List<ZipEntry> entries = null;
		try {
			zipFile = new ZipFile(
				Util.getContentDirectory() + File.separator + BACKUP_STRING + CORE.getCustomer().getName() + File.separator
						+ selectedBackupName);

			entries = zipFile.stream()
					.filter(e -> e.getName().equals(PROBLEMS_FILE_NAME))
					.limit(1)
					.collect(Collectors.toList());
		} finally {
			if (zipFile != null) {
				zipFile.close();
			}
		}

		if (entries.size() < 1) {
			throw new ValidationException(new Message(DataMaintenance.selectedBackupNamePropertyName,
					BACKUP_HAS_NO_PROBLEMS_FILE_ERROR));
		}
	}

	// Note: this action is only available for non-external backups
	@Override
	public Download download(DataMaintenance bean, WebContext webContext)
	throws Exception {
		String selectedBackupName = bean.getSelectedBackupName();

		@SuppressWarnings("resource")
		ZipFile zipFile = null;
		byte[] bytes = null;
		try {
			zipFile = new ZipFile(
					Util.getContentDirectory() + File.separator + BACKUP_STRING + CORE.getCustomer().getName() + File.separator
					+ selectedBackupName);

			ZipEntry entry = zipFile.stream()
				.filter(e -> e.getName().equals(PROBLEMS_FILE_NAME))
				.limit(1)
				.collect(Collectors.toList())
				.get(0);

			@SuppressWarnings("resource")
			InputStream is = zipFile.getInputStream(entry);
			if (is != null) {
				bytes = is.readAllBytes();
			}
			IOUtils.close(is);
		} finally {
			if (zipFile != null) {
				zipFile.close();
			}
		}
		
		if (bytes == null || bytes.length == 0) {
			EXT.push(new PushMessage().user(CORE.getUser()).growl(MessageSeverity.info, BACKUP_HAS_NO_PROBLEMS));
		}
		
		return new Download(PROBLEMS_FILE_NAME, bytes, MimeType.plain);
	}
}
