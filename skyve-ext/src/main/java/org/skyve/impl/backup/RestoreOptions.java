package org.skyve.impl.backup;

/**
 * Configuration contract for the restore operation, specifying how existing
 * database schema and content should be handled before data is restored.
 */
public interface RestoreOptions {
	@SuppressWarnings("java:S115") // Enum names are restore option codes.
	public enum ContentOption {
		clearOrphanedContentIds,
		saveOrphanedContentIds,
		error
	}
	
	@SuppressWarnings("java:S115") // Enum names are restore option codes.
	public enum PreProcess {
		noProcessing,
		dropUsingMetadataAndCreateUsingBackup,
		dropUsingBackupAndCreateUsingBackup,
		dropUsingMetadataAndCreateUsingMetadata,
		dropUsingBackupAndCreateUsingMetadata,
		createUsingBackup,
		createUsingMetadata,
		deleteData;
	}
	
	@SuppressWarnings("java:S115") // Enum names are restore option codes.
	public enum IndexingOption {
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
