package modules.admin.DataMaintenance.actions;

import java.io.File;

import org.skyve.CORE;
import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.ValidationException;
import org.skyve.impl.backup.DDL;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.util.FileUtil;
import org.skyve.util.Util;
import org.skyve.web.WebContext;

import modules.admin.domain.DataMaintenance;
import modules.admin.domain.DataMaintenance.RestorePreProcess;

public class Restore implements ServerSideAction<DataMaintenance> {
	private static final long serialVersionUID = 8521252561712649481L;

	@Override
	public ServerSideActionResult execute(DataMaintenance bean, WebContext webContext)
	throws Exception {
		
		if(bean.getRestorePreProcess()==null){
			Customer customer = CORE.getUser().getCustomer();
			Module module = customer.getModule(DataMaintenance.MODULE_NAME);
			Document document = module.getDocument(customer, DataMaintenance.DOCUMENT_NAME);
			
			StringBuilder sb = new StringBuilder(64);
			sb.append("You must select a ");
			sb.append(document.getAttribute(DataMaintenance.restorePreProcessPropertyName).getDisplayName());
			sb.append(" before you can perform this action.");
			
			throw new ValidationException(new Message(DataMaintenance.restorePreProcessPropertyName, sb.toString()));
		}
		
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
		
		if (bean.getRestorePreProcess() == null) {
			throw new ValidationException(new Message(DataMaintenance.restorePreProcessPropertyName,
														"Please select a pre-process value."));
		}
		
		String extractDirName = selectedBackupName.substring(0, selectedBackupName.length() - 4);
		File extractDir = new File(backup.getParentFile(), extractDirName);
		Util.LOGGER.info("Extract " + backup.getAbsolutePath() + " to " + extractDir.getAbsolutePath());
		if (extractDir.exists()) {
			Util.LOGGER.info(extractDir.getAbsolutePath() + " already exists - delete it.");
			FileUtil.delete(extractDir);
			Util.LOGGER.info(extractDir.getAbsolutePath() + " deleted");
		}
		FileUtil.extractZipArchive(backup, extractDir);
		Util.LOGGER.info("Extracted " + backup.getAbsolutePath() + " to " + extractDir.getAbsolutePath());

		RestorePreProcess pre = bean.getRestorePreProcess();
		boolean truncateDatabase = RestorePreProcess.deleteData.equals(pre);
		String schemaName = bean.getSchemaName();
		Util.LOGGER.info("Truncate " + ((schemaName == null) ? "default" : schemaName) + " schema");
		org.skyve.impl.backup.Truncate.truncate(bean.getSchemaName(), truncateDatabase, true);

		if (RestorePreProcess.createUsingBackup.equals(pre)) {
			DDL.create(new File(extractDir, "create.sql"), true);
		}
		else if (RestorePreProcess.createUsingMetadata.equals(pre)) {
			DDL.create(null, true);
		}
		else if (RestorePreProcess.dropUsingBackupAndCreateUsingBackup.equals(pre)) {
			DDL.drop(new File(extractDir, "drop.sql"), true);
			DDL.create(new File(extractDir, "create.sql"), true);
		}
		else if (RestorePreProcess.dropUsingBackupAndCreateUsingMetadata.equals(pre)) {
			DDL.drop(new File(extractDir, "drop.sql"), true);
			DDL.create(null, true);
		}
		else if (RestorePreProcess.dropUsingMetadataAndCreateUsingBackup.equals(pre)) {
			DDL.drop(null, true);
			DDL.create(new File(extractDir, "create.sql"), true);
		}
		else if (RestorePreProcess.dropUsingMetadataAndCreateUsingMetadata.equals(pre)) {
			DDL.drop(null, true);
			DDL.create(null, true);
		}

		Util.LOGGER.info("Restore " + extractDirName);
		org.skyve.impl.backup.Restore.restore(extractDirName, true);
		Util.LOGGER.info("DDL Sync");
		DDL.sync(true);
		Util.LOGGER.info("Reindex textual indexes.");
		org.skyve.impl.backup.Reindex.reindex();
		Util.LOGGER.info("Delete extracted folder " + extractDir.getAbsolutePath());
		FileUtil.delete(extractDir);
		Util.LOGGER.info("DONE");

		return new ServerSideActionResult(bean);
	}
}
