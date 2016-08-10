package modules.admin.DataMaintenance.actions;

import modules.admin.domain.DataMaintenance;

import java.io.File;

import org.skyve.CORE;
import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.ValidationException;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.util.FileUtil;
import org.skyve.util.Util;
import org.skyve.web.WebContext;

public class Restore implements ServerSideAction<DataMaintenance> {
	private static final long serialVersionUID = 8521252561712649481L;

	@Override
	public ServerSideActionResult execute(DataMaintenance bean, WebContext webContext)
	throws Exception {
		bean.setRefreshContent(Boolean.TRUE);

		String selectedBackupName = bean.getSelectedBackupName();
		File backup = new File(String.format("%sbackup_%s%s%s", 
												Util.getContentDirectory(),
												CORE.getUser().getCustomerName(),
												File.separator,
												selectedBackupName));
		if (! backup.exists()) {
			Util.LOGGER.warning("Backup " + backup.getAbsolutePath() + " DNE");
			throw new ValidationException(new Message("Backup " + selectedBackupName + " no longer exists"));
		}
		
		String extractDirName = selectedBackupName.substring(0, selectedBackupName.length() - 4);
		File extractDir = new File(backup.getParentFile(), extractDirName);
		Util.LOGGER.info("Extract " + backup.getAbsolutePath() + " to " + extractDir.getAbsolutePath());
		if (extractDir.exists()) {
			Util.LOGGER.info(backup.getAbsolutePath() + " already exists - delete it.");
			FileUtil.delete(extractDir);
			Util.LOGGER.info(backup.getAbsolutePath() + " deleted");
		}
		FileUtil.extractZipArchive(backup, extractDir);
		Util.LOGGER.info("Extracted " + backup.getAbsolutePath() + " to " + extractDir.getAbsolutePath());

		String schemaName = bean.getSchemaName();
		Util.LOGGER.info("Truncate " + ((schemaName == null) ? "default" : schemaName) + " schema");
		org.skyve.impl.backup.Truncate.truncate(bean.getSchemaName());
		Util.LOGGER.info("Restore " + extractDirName);
		org.skyve.impl.backup.Restore.restore(extractDirName);
		Util.LOGGER.info("Reindex textual indexes.");
		org.skyve.impl.backup.Reindex.reindex();
		Util.LOGGER.info("Delete extracted folder " + extractDir.getAbsolutePath());
		FileUtil.delete(extractDir);
		Util.LOGGER.info("DONE");

		return new ServerSideActionResult(bean);
	}
}
