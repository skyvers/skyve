package org.skyve.impl.backup;

/**
 * Configuration contract for the restore operation, specifying how existing
 * database schema and content should be handled before data is restored.
 */
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
