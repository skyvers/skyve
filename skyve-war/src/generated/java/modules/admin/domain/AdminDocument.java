package modules.admin.domain;

import jakarta.annotation.Generated;
import org.skyve.metadata.model.document.ModuleDocument;

/**
 * Compile-time references to the documents declared in the admin module.
 * Generated - local changes will be overwritten.
 */
@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
public enum AdminDocument implements ModuleDocument {
	/** The "Audit" document. */
	AUDIT("Audit"),
	/** The "AuditList" document. */
	AUDIT_LIST("AuditList"),
	/** The "ChangePassword" document. */
	CHANGE_PASSWORD("ChangePassword"),
	/** The "Communication" document. */
	COMMUNICATION("Communication"),
	/** The "CommunicationTemplate" document. */
	COMMUNICATION_TEMPLATE("CommunicationTemplate"),
	/** The "Configuration" document. */
	CONFIGURATION("Configuration"),
	/** The "Contact" document. */
	CONTACT("Contact"),
	/** The "Content" document. */
	CONTENT("Content"),
	/** The "ControlPanel" document. */
	CONTROL_PANEL("ControlPanel"),
	/** The "CorruptArchiveError" document. */
	CORRUPT_ARCHIVE_ERROR("CorruptArchiveError"),
	/** The "Country" document. */
	COUNTRY("Country"),
	/** The "DataGroup" document. */
	DATA_GROUP("DataGroup"),
	/** The "DataMaintenance" document. */
	DATA_MAINTENANCE("DataMaintenance"),
	/** The "DocumentCreator" document. */
	DOCUMENT_CREATOR("DocumentCreator"),
	/** The "DocumentNumber" document. */
	DOCUMENT_NUMBER("DocumentNumber"),
	/** The "DownloadFolder" document. */
	DOWNLOAD_FOLDER("DownloadFolder"),
	/** The "DynamicEntity" document. */
	DYNAMIC_ENTITY("DynamicEntity"),
	/** The "DynamicRelation" document. */
	DYNAMIC_RELATION("DynamicRelation"),
	/** The "Generic" document. */
	GENERIC("Generic"),
	/** The "Group" document. */
	GROUP("Group"),
	/** The "GroupRole" document. */
	GROUP_ROLE("GroupRole"),
	/** The "HeapDumpList" document. */
	HEAP_DUMP_LIST("HeapDumpList"),
	/** The "ImportExport" document. */
	IMPORT_EXPORT("ImportExport"),
	/** The "ImportExportColumn" document. */
	IMPORT_EXPORT_COLUMN("ImportExportColumn"),
	/** The "Job" document. */
	JOB("Job"),
	/** The "JobSchedule" document. */
	JOB_SCHEDULE("JobSchedule"),
	/** The "Jobs" document. */
	JOBS("Jobs"),
	/** The "MailLog" document. */
	MAIL_LOG("MailLog"),
	/** The "MailLogList" document. */
	MAIL_LOG_LIST("MailLogList"),
	/** The "ModuleDocument" document. */
	MODULE_DOCUMENT("ModuleDocument"),
	/** The "MonitoringDashboard" document. */
	MONITORING_DASHBOARD("MonitoringDashboard"),
	/** The "ReportDataset" document. */
	REPORT_DATASET("ReportDataset"),
	/** The "ReportManager" document. */
	REPORT_MANAGER("ReportManager"),
	/** The "ReportParameter" document. */
	REPORT_PARAMETER("ReportParameter"),
	/** The "ReportTemplate" document. */
	REPORT_TEMPLATE("ReportTemplate"),
	/** The "SecurityLog" document. */
	SECURITY_LOG("SecurityLog"),
	/** The "SelfRegistration" document. */
	SELF_REGISTRATION("SelfRegistration"),
	/** The "SelfRegistrationActivation" document. */
	SELF_REGISTRATION_ACTIVATION("SelfRegistrationActivation"),
	/** The "Snapshot" document. */
	SNAPSHOT("Snapshot"),
	/** The "Snapshots" document. */
	SNAPSHOTS("Snapshots"),
	/** The "Startup" document. */
	STARTUP("Startup"),
	/** The "Subscription" document. */
	SUBSCRIPTION("Subscription"),
	/** The "SystemDashboard" document. */
	SYSTEM_DASHBOARD("SystemDashboard"),
	/** The "Tag" document. */
	TAG("Tag"),
	/** The "Tagged" document. */
	TAGGED("Tagged"),
	/** The "User" document. */
	USER("User"),
	/** The "UserAccount" document. */
	USER_ACCOUNT("UserAccount"),
	/** The "UserCandidateContact" document. */
	USER_CANDIDATE_CONTACT("UserCandidateContact"),
	/** The "UserDashboard" document. */
	USER_DASHBOARD("UserDashboard"),
	/** The "UserList" document. */
	USER_LIST("UserList"),
	/** The "UserLoginRecord" document. */
	USER_LOGIN_RECORD("UserLoginRecord"),
	/** The "UserMonthlyHits" document. */
	USER_MONTHLY_HITS("UserMonthlyHits"),
	/** The "UserProxy" document. */
	USER_PROXY("UserProxy"),
	/** The "UserRole" document. */
	USER_ROLE("UserRole"),
	/** The "UserToken" document. */
	USER_TOKEN("UserToken");

	private final String name;

	private AdminDocument(String name) {
		this.name = name;
	}

	@Override
	public String moduleName() {
		return "admin";
	}

	@Override
	public String documentName() {
		return name;
	}
}
