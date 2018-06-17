package org.skyve.impl.backup;

public interface RestoreOptions {
	public static enum ContentOption {
		clearOrphanedContentIds,
		saveOrphanedContentIds,
		error
	}
	
	public static enum PreProcess {
		noProcessing,
		dropUsingMetadataAndCreateUsingBackup,
		dropUsingBackupAndCreateUsingBackup,
		dropUsingMetadataAndCreateUsingMetadata,
		dropUsingBackupAndCreateUsingMetadata,
		createUsingBackup,
		createUsingMetadata,
		deleteData;
	}
	
	public static enum IndexingOption {
		data,
		content,
		both,
		none;
	}
	
	public PreProcess getPreProcess();
	public String getSelectedBackupName();
	public ContentOption getContentOption();
	public IndexingOption getIndexingOption();
}
