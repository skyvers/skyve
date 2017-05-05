package modules.admin.DataMaintenance;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.skyve.CORE;
import org.skyve.domain.types.Enumeration;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.Persistence;
import org.skyve.web.WebContext;

import modules.ModulesUtil.DomainValueSortByDescription;
import modules.admin.domain.DataMaintenance;
import modules.admin.domain.DataMaintenanceModuleDocument;

public class DataMaintenanceBizlet extends Bizlet<DataMaintenance> {
	private static final long serialVersionUID = 1L;
	
	public static final String SYSTEM_DATA_REFRESH_NOTIFICATION = "SYSTEM Document Data Refresh Notification";
	public static final String SYSTEM_DATA_REFRESH_DEFAULT_SUBJECT = "Perform Document Data Refresh - Complete";
	public static final String SYSTEM_DATA_REFRESH_DEFAULT_BODY = "The document data refresh is complete. Check Job log for details.";

	public static enum RestorePreProcess implements Enumeration {
		noProcessing("noProcessing", "No Processing"),
		dropUsingMetadataAndCreateUsingBackup("dropUsingMetadataAndCreateUsingBackup", "Drop tables using metadata & recreate tables from backup create.sql"),
		dropUsingBackupAndCreateUsingBackup("dropUsingBackupAndCreateUsingBackup", "Drop tables using backup drop.sql & recreate tables from backup create.sql"),
		dropUsingMetadataAndCreateUsingMetadata("dropUsingMetadataAndCreateUsingMetadata", "Drop tables using metadata & recreate tables from metadata"),
		dropUsingBackupAndCreateUsingMetadata("dropUsingBackupAndCreateUsingMetadata", "Drop tables using backup drop.sql & recreate tables from metadata"),
		createUsingBackup("createUsingBackup", "Create tables from backup"),
		createUsingMetadata("createUsingMetadata", "Create tables from metadata"),
		deleteData("deleteData", "Delete existing table data using metadata");

		private String code;
		private String description;

		/** @hidden */
		private DomainValue domainValue;

		/** @hidden */
		private static List<DomainValue> domainValues;

		private RestorePreProcess(String code, String description) {
			this.code = code;
			this.description = description;
			this.domainValue = new DomainValue(code, description);
		}

		@Override
		public String toCode() {
			return code;
		}

		@Override
		public String toDescription() {
			return description;
		}

		@Override
		public DomainValue toDomainValue() {
			return domainValue;
		}

		public static RestorePreProcess fromCode(String code) {
			RestorePreProcess result = null;

			for (RestorePreProcess value : values()) {
				if (value.code.equals(code)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static RestorePreProcess fromDescription(String description) {
			RestorePreProcess result = null;

			for (RestorePreProcess value : values()) {
				if (value.description.equals(description)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static List<DomainValue> toDomainValues() {
			if (domainValues == null) {
				RestorePreProcess[] values = values();
				domainValues = new ArrayList<>(values.length);
				for (RestorePreProcess value : values) {
					domainValues.add(value.domainValue);
				}
			}

			return domainValues;
		}
	}

	
	@Override
	public DataMaintenance newInstance(DataMaintenance bean) throws Exception {
		Persistence persistence = CORE.getPersistence();
		DocumentQuery q = persistence.newDocumentQuery(DataMaintenance.MODULE_NAME, DataMaintenance.DOCUMENT_NAME);
		DataMaintenance result = q.beanResult();
		if (result == null) {
			result = bean;
		}
		
		Customer c = CORE.getUser().getCustomer();
		for (Module m : c.getModules()) {
			for (String k : m.getDocumentRefs().keySet()) {
				Document d = m.getDocument(c, k);
				if (d.getPersistent() != null) {
					DataMaintenanceModuleDocument doc = DataMaintenanceModuleDocument.newInstance();
					doc.setModuleName(m.getName());
					doc.setDocumentName(d.getName());
					doc.setModDocName(String.format("%s.%s", m.getTitle(), d.getSingularAlias()));
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
					if (d.getPersistent() != null) {
						result.add(new DomainValue(String.format("%s.%s", m.getName(), k), 
													String.format("%s.%s", m.getTitle(), d.getSingularAlias())));
					}
				}
			}
			Collections.sort(result, new DomainValueSortByDescription());
		}
		else if(DataMaintenance.auditModuleNamePropertyName.equals(attributeName)){
			result = new ArrayList<>();
			Customer c = CORE.getUser().getCustomer();
			for (Module m : c.getModules()) {
				result.add(new DomainValue(m.getName(), m.getTitle()));
			}
			Collections.sort(result, new DomainValueSortByDescription());			
		}
		else if(DataMaintenance.restorePreProcessPropertyName.equals(attributeName)){
			result = new ArrayList<>();
			result.add(new DomainValue(RestorePreProcess.noProcessing.toCode(), RestorePreProcess.noProcessing.toDescription()));
			if(UtilImpl.CUSTOMER!=null){
				result.add(new DomainValue(RestorePreProcess.dropUsingMetadataAndCreateUsingBackup.toCode(), RestorePreProcess.dropUsingMetadataAndCreateUsingBackup.toDescription()));
				result.add(new DomainValue(RestorePreProcess.dropUsingBackupAndCreateUsingBackup.toCode(), RestorePreProcess.dropUsingBackupAndCreateUsingBackup.toDescription()));
				result.add(new DomainValue(RestorePreProcess.dropUsingMetadataAndCreateUsingMetadata.toCode(), RestorePreProcess.dropUsingMetadataAndCreateUsingMetadata.toDescription()));
				result.add(new DomainValue(RestorePreProcess.dropUsingBackupAndCreateUsingMetadata.toCode(), RestorePreProcess.dropUsingBackupAndCreateUsingMetadata.toDescription()));
				result.add(new DomainValue(RestorePreProcess.createUsingBackup.toCode(), RestorePreProcess.createUsingBackup.toDescription()));
				result.add(new DomainValue(RestorePreProcess.createUsingMetadata.toCode(), RestorePreProcess.createUsingMetadata.toDescription()));
			}
			result.add(new DomainValue(RestorePreProcess.deleteData.toCode(), RestorePreProcess.deleteData.toDescription()));
		}

		return result;
	}

	@Override
	public void preRerender(String source, DataMaintenance bean, WebContext webContext) throws Exception {

		if(DataMaintenance.restorePreProcessPropertyName.equals(source)){
			String instructionHint = null;
			DataMaintenanceBizlet.RestorePreProcess pre = DataMaintenanceBizlet.RestorePreProcess.valueOf(bean.getRestorePreProcess());
			switch(pre){
			case noProcessing:
				instructionHint="Use this option when you've created your database from scratch (or with the bootstrap) and you've let the Skyve create all DDL. You know the backup is from the same version and the schema is synchronised (matches the metadata).";	
				break;
			case createUsingBackup:
				instructionHint="Use this option when you've created a empty schema (manually or scripted).";
				break;
			case createUsingMetadata:
				instructionHint="Use this option when you have a empty schema but the backup doesn't match the current metadata.";
				break;
			case deleteData:
				instructionHint="Use this option when the backup matches and you have trivial or testing data (i.e. just delete the data and then run the restore.)";
				break;
			case dropUsingBackupAndCreateUsingBackup:
				instructionHint="Use this option when for some reason the schema is in the shape of the backup (maybe your previous attempt to restore failed). You cant drop the schema without stopping the server and if you do that, you can't log in any more without restoring. Since the backup/restore only looks after tables under Skyve control, it could be that extra tables have constraints that you need to drop or other issues that you only find after trying to restore.";
				break;
			case dropUsingBackupAndCreateUsingMetadata:
				instructionHint="Use this option when you've tried a restore before and your database is now in the shape of the backup.";
				break;
			case dropUsingMetadataAndCreateUsingBackup:
				instructionHint="Use this option when your backup is a different version or you're not sure, you want the schema to be dropped (the schema matches the metadata) using the system metadata deployed, but you need the schema to look like it did when the backup was taken. (Part of the restore post-process is to sync the schema and reindex content.)";
				break;
			case dropUsingMetadataAndCreateUsingMetadata:
				instructionHint="Use this option when you know the backup is compatible with the schema as it currently stands. You have a large amount of data that you want to delete and the quickest way is drop and recreate the schema.";
				break;
			default:
				break;

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
				if (d.getPersistent() != null) {
					result.add(new DomainValue(d.getName(), d.getSingularAlias()));
				}
			}

			Collections.sort(result, new DomainValueSortByDescription());
			return result;
		}
		
		return super.getDynamicDomainValues(attributeName, bean);
	}



}
