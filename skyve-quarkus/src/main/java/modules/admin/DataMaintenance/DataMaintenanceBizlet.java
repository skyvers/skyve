package modules.admin.DataMaintenance;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.SingletonCachedBizlet;
import org.skyve.metadata.module.Module;
import org.skyve.web.WebContext;

import modules.admin.ModulesUtil.DomainValueSortByDescription;
import modules.admin.Jobs.JobsBizlet;
import modules.admin.domain.DataMaintenance;
import modules.admin.domain.DataMaintenance.RestorePreProcess;
import modules.admin.domain.ModuleDocument;

public class DataMaintenanceBizlet extends SingletonCachedBizlet<DataMaintenance> {
	public static final String SYSTEM_DATA_REFRESH_NOTIFICATION = "SYSTEM Document Data Refresh Notification";
	public static final String SYSTEM_DATA_REFRESH_DEFAULT_SUBJECT = "Perform Document Data Refresh - Complete";
	public static final String SYSTEM_DATA_REFRESH_DEFAULT_BODY = "The document data refresh is complete." + JobsBizlet.SYSTEM_JOB_NOTIFICATION_LINK_TO_JOBS;

	@Override
	public DataMaintenance newInstance(DataMaintenance bean) throws Exception {
		DataMaintenance result = super.newInstance(bean);
		
		Customer c = CORE.getUser().getCustomer();
		for (Module m : c.getModules()) {
			for (String k : m.getDocumentRefs().keySet()) {
				Document d = m.getDocument(c, k);
				if (d.isPersistable()) {
					ModuleDocument doc = ModuleDocument.newInstance();
					doc.setModuleName(m.getName());
					doc.setDocumentName(d.getName());
					doc.setModDocName(String.format("%s.%s", m.getLocalisedTitle(), d.getLocalisedSingularAlias()));
					result.getRefreshDocuments().add(doc);
				}
			}
		}

		return result;
	}

	@Override
	public List<DomainValue> getConstantDomainValues(String attributeName) throws Exception {
		List<DomainValue> result = null;
		
		if (DataMaintenance.modDocNamePropertyName.equals(attributeName) ) {
			result = new ArrayList<>();
			Customer c = CORE.getUser().getCustomer();
			for (Module m : c.getModules()) {
				for (String k : m.getDocumentRefs().keySet()) {
					Document d = m.getDocument(c, k);
					if (d.isPersistable()) {
						result.add(new DomainValue(String.format("%s.%s", m.getName(), k), 
													String.format("%s.%s", m.getLocalisedTitle(), d.getLocalisedSingularAlias())));
					}
				}
			}
			Collections.sort(result, new DomainValueSortByDescription());
		}
		else if(DataMaintenance.auditModuleNamePropertyName.equals(attributeName)){
			result = new ArrayList<>();
			Customer c = CORE.getUser().getCustomer();
			for (Module m : c.getModules()) {
				result.add(new DomainValue(m.getName(), m.getLocalisedTitle()));
			}
			Collections.sort(result, new DomainValueSortByDescription());			
		}
		else if(DataMaintenance.restorePreProcessPropertyName.equals(attributeName)){
			result = new ArrayList<>();
			result.add(RestorePreProcess.noProcessing.toDomainValue());
			if(UtilImpl.CUSTOMER!=null){
				result.add(RestorePreProcess.dropTablesUsingMetadataRecreateTablesFromBackupCreatesql.toDomainValue());
				result.add(RestorePreProcess.dropTablesUsingBackupDropsqlRecreateTablesFromBackupCreatesql.toDomainValue());
				result.add(RestorePreProcess.dropTablesUsingMetadataRecreateTablesFromMetadata.toDomainValue());
				result.add(RestorePreProcess.dropTablesUsingBackupDropsqlRecreateTablesFromMetadata.toDomainValue());
				result.add(RestorePreProcess.createTablesFromBackup.toDomainValue());
				result.add(RestorePreProcess.createTablesFromMetadata.toDomainValue());
			}
			result.add(RestorePreProcess.deleteExistingTableDataUsingMetadata.toDomainValue());
		}

		return result;
	}

	@Override
	public void preRerender(String source, DataMaintenance bean, WebContext webContext) throws Exception {
		if (DataMaintenance.restorePreProcessPropertyName.equals(source)) {
			String instructionHint = null;
			RestorePreProcess pre = bean.getRestorePreProcess();
			if (pre != null) {
				switch (pre) {
					case noProcessing:
						instructionHint="Use this option when you've created your database from scratch (or with the bootstrap) and Skyve has created all database objects. You know the backup is from the same version and the schema is synchronised (matches the metadata).";	
						break;
					case createTablesFromBackup:
						instructionHint="Use this option when you've created a empty schema (manually or scripted).";
						break;
					case createTablesFromMetadata:
						instructionHint="Use this option when you have a empty schema but the backup application version doesn't match your version.";
						break;
					case deleteExistingTableDataUsingMetadata:
						instructionHint="Use this option when the backup is from the same version of the application and your data size is not large (i.e. just delete the data and then run the restore.)";
						break;
					case dropTablesUsingBackupDropsqlRecreateTablesFromBackupCreatesql:
						instructionHint="Use this option when your schema matches the application version of the backup (maybe your previous attempt to restore failed). You can't drop the schema without stopping the server and if you do that, you can't log in any more without restoring. Since the backup/restore only looks after tables under Skyve control, it could be that extra tables have constraints that you need to drop or other issues that you only find after trying to restore.";
						break;
					case dropTablesUsingBackupDropsqlRecreateTablesFromMetadata:
						instructionHint="Use this option when you've tried a restore before and your database is now in the shape of the backup application version.";
						break;
					case dropTablesUsingMetadataRecreateTablesFromBackupCreatesql:
						instructionHint="Use this option when your backup is from a different version of the application, you want the schema to be dropped (the schema matches the metadata) using the system metadata deployed, but you need the schema to look like it did when the backup was taken. (Part of the restore post-process is to sync the schema and reindex content.)";
						break;
					case dropTablesUsingMetadataRecreateTablesFromMetadata:
						instructionHint="Use this option when you know the backup is from the same version of the application. You have a large amount of data that you want to delete and the quickest way is drop and recreate the schema.";
						break;
					default:
						break;
				}
			}
			bean.setInstructionHint(instructionHint);
		}
		
		super.preRerender(source, bean, webContext);
	}


	@Override
	public List<DomainValue> getDynamicDomainValues(String attributeName, DataMaintenance bean)
			throws Exception {

		
		if(DataMaintenance.auditDocumentNamePropertyName.equals(attributeName) && bean.getAuditModuleName()!=null){
			List<DomainValue> result = new ArrayList<>();
			Customer c = CORE.getUser().getCustomer();
			Module m = c.getModule(bean.getAuditModuleName());
			for (String k : m.getDocumentRefs().keySet()) {
				Document d = m.getDocument(c, k);
				if (d.isPersistable()) {
					result.add(new DomainValue(d.getName(), d.getLocalisedSingularAlias()));
				}
			}

			Collections.sort(result, new DomainValueSortByDescription());
			return result;
		}
		
		return super.getDynamicDomainValues(attributeName, bean);
	}

	@Override
	public DataMaintenance preExecute(ImplicitActionName actionName, DataMaintenance bean, Bean parentBean, WebContext webContext) throws Exception {

		if(ImplicitActionName.Edit.equals(actionName) || ImplicitActionName.New.equals(actionName)) {
			bean.setRestorePreProcess(RestorePreProcess.deleteExistingTableDataUsingMetadata);
		}
		
		return super.preExecute(actionName, bean, parentBean, webContext);
	}



}
