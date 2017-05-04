package modules.admin.DataMaintenance.actions;

import java.io.File;

import modules.admin.DataMaintenance.DataMaintenanceBizlet;
import modules.admin.domain.DataMaintenance;

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

public class Restore implements ServerSideAction<DataMaintenance> {
	private static final long serialVersionUID = 8521252561712649481L;

	@Override
	public ServerSideActionResult<DataMaintenance> execute(DataMaintenance bean, WebContext webContext)
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

		DataMaintenanceBizlet.RestorePreProcess pre = DataMaintenanceBizlet.RestorePreProcess.valueOf(bean.getRestorePreProcess());
		boolean truncateDatabase = DataMaintenanceBizlet.RestorePreProcess.deleteData.equals(pre);
		String schemaName = bean.getSchemaName();
		if (truncateDatabase) {
			Util.LOGGER.info("Truncate " + ((schemaName == null) ? "default" : schemaName) + " schema");
		}
		org.skyve.impl.backup.Truncate.truncate(bean.getSchemaName(), truncateDatabase, true);

		boolean createUsingBackup = false;
		boolean sync = false;
		if (DataMaintenanceBizlet.RestorePreProcess.createUsingBackup.equals(pre)) {
			createUsingBackup = true;
			DDL.create(new File(extractDir, "create.sql"), true);
			sync = true;
		}
		else if (DataMaintenanceBizlet.RestorePreProcess.createUsingMetadata.equals(pre)) {
			DDL.create(null, true);
			sync = true;
		}
		else if (DataMaintenanceBizlet.RestorePreProcess.dropUsingBackupAndCreateUsingBackup.equals(pre)) {
			createUsingBackup = true;
			DDL.drop(new File(extractDir, "drop.sql"), true);
			DDL.create(new File(extractDir, "create.sql"), true);
			sync = true;
		}
		else if (DataMaintenanceBizlet.RestorePreProcess.dropUsingBackupAndCreateUsingMetadata.equals(pre)) {
			DDL.drop(new File(extractDir, "drop.sql"), true);
			DDL.create(null, true);
			sync = true;
		}
		else if (DataMaintenanceBizlet.RestorePreProcess.dropUsingMetadataAndCreateUsingBackup.equals(pre)) {
			createUsingBackup = true;
			DDL.drop(null, true);
			DDL.create(new File(extractDir, "create.sql"), true);
			sync = true;
		}
		else if (DataMaintenanceBizlet.RestorePreProcess.dropUsingMetadataAndCreateUsingMetadata.equals(pre)) {
			DDL.drop(null, true);
			DDL.create(null, true);
			sync = true;
		}

		Util.LOGGER.info("Restore " + extractDirName);
		org.skyve.impl.backup.Restore.restore(extractDirName, createUsingBackup);
		Util.LOGGER.info("DDL Sync");
		if (sync) {
			DDL.sync(true);
		}
		Util.LOGGER.info("Reindex textual indexes.");
		org.skyve.impl.backup.Reindex.reindex();
		Util.LOGGER.info("Delete extracted folder " + extractDir.getAbsolutePath());
		FileUtil.delete(extractDir);
		Util.LOGGER.info("DONE");

		return new ServerSideActionResult<>(bean);
	}
}
