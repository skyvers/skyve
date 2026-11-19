package modules.admin.domain;

import jakarta.annotation.Generated;
import org.skyve.metadata.module.ModuleJob;

/**
 * Compile-time references to the jobs declared in the admin module.
 * Generated - local changes will be overwritten.
 */
@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
public enum AdminJob implements ModuleJob {
	/** The "jPerformDocumentActionForTag" job. */
	J_PERFORM_DOCUMENT_ACTION_FOR_TAG("jPerformDocumentActionForTag"),
	/** The "jDeleteAllTaggedDataForTag" job. */
	J_DELETE_ALL_TAGGED_DATA_FOR_TAG("jDeleteAllTaggedDataForTag"),
	/** The "jProcessCommunicationsForTag" job. */
	J_PROCESS_COMMUNICATIONS_FOR_TAG("jProcessCommunicationsForTag"),
	/** The "jTruncateAuditLog" job. */
	J_TRUNCATE_AUDIT_LOG("jTruncateAuditLog"),
	/** The "jBackup" job. */
	J_BACKUP("jBackup"),
	/** The "jAdhocBackup" job. */
	J_ADHOC_BACKUP("jAdhocBackup"),
	/** The "jReindex" job. */
	J_REINDEX("jReindex"),
	/** The "jReindexAttachments" job. */
	J_REINDEX_ATTACHMENTS("jReindexAttachments"),
	/** The "jReindexBeans" job. */
	J_REINDEX_BEANS("jReindexBeans"),
	/** The "jRefreshDocumentTuples" job. */
	J_REFRESH_DOCUMENT_TUPLES("jRefreshDocumentTuples"),
	/** The "jBulkUserCreation" job. */
	J_BULK_USER_CREATION("jBulkUserCreation"),
	/** The "jGenerateTestData" job. */
	J_GENERATE_TEST_DATA("jGenerateTestData"),
	/** The "jAvailableDiskSpaceAlarm" job. */
	J_AVAILABLE_DISK_SPACE_ALARM("jAvailableDiskSpaceAlarm"),
	/** The "jDifferentCountryLoginNotification" job. */
	J_DIFFERENT_COUNTRY_LOGIN_NOTIFICATION("jDifferentCountryLoginNotification"),
	/** The "jPasswordChangeNotification" job. */
	J_PASSWORD_CHANGE_NOTIFICATION("jPasswordChangeNotification"),
	/** The "ArchiveJob" job. */
	ARCHIVE_JOB("ArchiveJob"),
	/** The "DumpHeapJob" job. */
	DUMP_HEAP_JOB("DumpHeapJob");

	private final String name;

	private AdminJob(String name) {
		this.name = name;
	}

	@Override
	public String moduleName() {
		return "admin";
	}

	@Override
	public String jobName() {
		return name;
	}
}
