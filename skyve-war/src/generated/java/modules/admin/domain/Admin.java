package modules.admin.domain;

import jakarta.annotation.Generated;

/**
 * The metadata names declared in the admin module.
 * Generated - local changes will be overwritten.
 */
@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
public final class Admin {
	/** The name of the admin module. */
	public static final String MODULE_NAME = "admin";

	/** The role names declared in the admin module. */
	public static final class Roles {
		/** The "Anonymous" role name. */
		public static final String ANONYMOUS = "Anonymous";
		/** The "AppUser" role name. */
		public static final String APP_USER = "AppUser";
		/** The "AuditManager" role name. */
		public static final String AUDIT_MANAGER = "AuditManager";
		/** The "BasicUser" role name. */
		public static final String BASIC_USER = "BasicUser";
		/** The "ContactManager" role name. */
		public static final String CONTACT_MANAGER = "ContactManager";
		/** The "ContactViewer" role name. */
		public static final String CONTACT_VIEWER = "ContactViewer";
		/** The "DevOps" role name. */
		public static final String DEV_OPS = "DevOps";
		/** The "JobMaintainer" role name. */
		public static final String JOB_MAINTAINER = "JobMaintainer";
		/** The "SecurityAdministrator" role name. */
		public static final String SECURITY_ADMINISTRATOR = "SecurityAdministrator";
		/** The "ViewUser" role name. */
		public static final String VIEW_USER = "ViewUser";

		private Roles() {
			// prevent instantiation
		}
	}

	/** The document names declared in the admin module. */
	public static final class Documents {
		/** The "Audit" document name. */
		public static final String AUDIT = "Audit";
		/** The "AuditList" document name. */
		public static final String AUDIT_LIST = "AuditList";
		/** The "ChangePassword" document name. */
		public static final String CHANGE_PASSWORD = "ChangePassword";
		/** The "Communication" document name. */
		public static final String COMMUNICATION = "Communication";
		/** The "CommunicationTemplate" document name. */
		public static final String COMMUNICATION_TEMPLATE = "CommunicationTemplate";
		/** The "Configuration" document name. */
		public static final String CONFIGURATION = "Configuration";
		/** The "Contact" document name. */
		public static final String CONTACT = "Contact";
		/** The "Content" document name. */
		public static final String CONTENT = "Content";
		/** The "ControlPanel" document name. */
		public static final String CONTROL_PANEL = "ControlPanel";
		/** The "CorruptArchiveError" document name. */
		public static final String CORRUPT_ARCHIVE_ERROR = "CorruptArchiveError";
		/** The "Country" document name. */
		public static final String COUNTRY = "Country";
		/** The "DataGroup" document name. */
		public static final String DATA_GROUP = "DataGroup";
		/** The "DataMaintenance" document name. */
		public static final String DATA_MAINTENANCE = "DataMaintenance";
		/** The "DocumentCreator" document name. */
		public static final String DOCUMENT_CREATOR = "DocumentCreator";
		/** The "DocumentNumber" document name. */
		public static final String DOCUMENT_NUMBER = "DocumentNumber";
		/** The "DownloadFolder" document name. */
		public static final String DOWNLOAD_FOLDER = "DownloadFolder";
		/** The "DynamicEntity" document name. */
		public static final String DYNAMIC_ENTITY = "DynamicEntity";
		/** The "DynamicRelation" document name. */
		public static final String DYNAMIC_RELATION = "DynamicRelation";
		/** The "Generic" document name. */
		public static final String GENERIC = "Generic";
		/** The "Group" document name. */
		public static final String GROUP = "Group";
		/** The "GroupRole" document name. */
		public static final String GROUP_ROLE = "GroupRole";
		/** The "HeapDumpList" document name. */
		public static final String HEAP_DUMP_LIST = "HeapDumpList";
		/** The "ImportExport" document name. */
		public static final String IMPORT_EXPORT = "ImportExport";
		/** The "ImportExportColumn" document name. */
		public static final String IMPORT_EXPORT_COLUMN = "ImportExportColumn";
		/** The "Job" document name. */
		public static final String JOB = "Job";
		/** The "JobSchedule" document name. */
		public static final String JOB_SCHEDULE = "JobSchedule";
		/** The "Jobs" document name. */
		public static final String JOBS = "Jobs";
		/** The "MailLog" document name. */
		public static final String MAIL_LOG = "MailLog";
		/** The "MailLogList" document name. */
		public static final String MAIL_LOG_LIST = "MailLogList";
		/** The "ModuleDocument" document name. */
		public static final String MODULE_DOCUMENT = "ModuleDocument";
		/** The "MonitoringDashboard" document name. */
		public static final String MONITORING_DASHBOARD = "MonitoringDashboard";
		/** The "ReportDataset" document name. */
		public static final String REPORT_DATASET = "ReportDataset";
		/** The "ReportManager" document name. */
		public static final String REPORT_MANAGER = "ReportManager";
		/** The "ReportParameter" document name. */
		public static final String REPORT_PARAMETER = "ReportParameter";
		/** The "ReportTemplate" document name. */
		public static final String REPORT_TEMPLATE = "ReportTemplate";
		/** The "SecurityLog" document name. */
		public static final String SECURITY_LOG = "SecurityLog";
		/** The "SelfRegistration" document name. */
		public static final String SELF_REGISTRATION = "SelfRegistration";
		/** The "SelfRegistrationActivation" document name. */
		public static final String SELF_REGISTRATION_ACTIVATION = "SelfRegistrationActivation";
		/** The "Snapshot" document name. */
		public static final String SNAPSHOT = "Snapshot";
		/** The "Snapshots" document name. */
		public static final String SNAPSHOTS = "Snapshots";
		/** The "Startup" document name. */
		public static final String STARTUP = "Startup";
		/** The "Subscription" document name. */
		public static final String SUBSCRIPTION = "Subscription";
		/** The "SystemDashboard" document name. */
		public static final String SYSTEM_DASHBOARD = "SystemDashboard";
		/** The "Tag" document name. */
		public static final String TAG = "Tag";
		/** The "Tagged" document name. */
		public static final String TAGGED = "Tagged";
		/** The "User" document name. */
		public static final String USER = "User";
		/** The "UserAccount" document name. */
		public static final String USER_ACCOUNT = "UserAccount";
		/** The "UserCandidateContact" document name. */
		public static final String USER_CANDIDATE_CONTACT = "UserCandidateContact";
		/** The "UserDashboard" document name. */
		public static final String USER_DASHBOARD = "UserDashboard";
		/** The "UserList" document name. */
		public static final String USER_LIST = "UserList";
		/** The "UserLoginRecord" document name. */
		public static final String USER_LOGIN_RECORD = "UserLoginRecord";
		/** The "UserMonthlyHits" document name. */
		public static final String USER_MONTHLY_HITS = "UserMonthlyHits";
		/** The "UserProxy" document name. */
		public static final String USER_PROXY = "UserProxy";
		/** The "UserRole" document name. */
		public static final String USER_ROLE = "UserRole";
		/** The "UserToken" document name. */
		public static final String USER_TOKEN = "UserToken";

		private Documents() {
			// prevent instantiation
		}
	}

	/** The query names declared in the admin module. */
	public static final class Queries {
		/** The "qContacts" query name. */
		public static final String Q_CONTACTS = "qContacts";
		/** The "qContactsByContactType" query name. */
		public static final String Q_CONTACTS_BY_CONTACT_TYPE = "qContactsByContactType";
		/** The "qUsers" query name. */
		public static final String Q_USERS = "qUsers";
		/** The "qUserProxies" query name. */
		public static final String Q_USER_PROXIES = "qUserProxies";
		/** The "qUserTokens" query name. */
		public static final String Q_USER_TOKENS = "qUserTokens";
		/** The "qActiveUserProxies" query name. */
		public static final String Q_ACTIVE_USER_PROXIES = "qActiveUserProxies";
		/** The "qUsersInGroup" query name. */
		public static final String Q_USERS_IN_GROUP = "qUsersInGroup";
		/** The "qUsersByGroup" query name. */
		public static final String Q_USERS_BY_GROUP = "qUsersByGroup";
		/** The "qLoginHistory" query name. */
		public static final String Q_LOGIN_HISTORY = "qLoginHistory";
		/** The "qMyLoginHistory" query name. */
		public static final String Q_MY_LOGIN_HISTORY = "qMyLoginHistory";
		/** The "qCompletedJobs" query name. */
		public static final String Q_COMPLETED_JOBS = "qCompletedJobs";
		/** The "qDashboardJobs" query name. */
		public static final String Q_DASHBOARD_JOBS = "qDashboardJobs";
		/** The "qAllSchedules" query name. */
		public static final String Q_ALL_SCHEDULES = "qAllSchedules";
		/** The "qExistingHits" query name. */
		public static final String Q_EXISTING_HITS = "qExistingHits";
		/** The "qSnapshots" query name. */
		public static final String Q_SNAPSHOTS = "qSnapshots";
		/** The "qTags" query name. */
		public static final String Q_TAGS = "qTags";
		/** The "qAudits" query name. */
		public static final String Q_AUDITS = "qAudits";
		/** The "qCommunications" query name. */
		public static final String Q_COMMUNICATIONS = "qCommunications";
		/** The "qCommunicationsForTemplate" query name. */
		public static final String Q_COMMUNICATIONS_FOR_TEMPLATE = "qCommunicationsForTemplate";
		/** The "qCommunicationTemplate" query name. */
		public static final String Q_COMMUNICATION_TEMPLATE = "qCommunicationTemplate";
		/** The "qSubscriptions" query name. */
		public static final String Q_SUBSCRIPTIONS = "qSubscriptions";
		/** The "qUserSubscriptions" query name. */
		public static final String Q_USER_SUBSCRIPTIONS = "qUserSubscriptions";
		/** The "qTagCommunications" query name. */
		public static final String Q_TAG_COMMUNICATIONS = "qTagCommunications";
		/** The "qImportExport" query name. */
		public static final String Q_IMPORT_EXPORT = "qImportExport";
		/** The "qDocumentNumbers" query name. */
		public static final String Q_DOCUMENT_NUMBERS = "qDocumentNumbers";
		/** The "qEnabledReports" query name. */
		public static final String Q_ENABLED_REPORTS = "qEnabledReports";
		/** The "qReportSchedules" query name. */
		public static final String Q_REPORT_SCHEDULES = "qReportSchedules";
		/** The "qReportTemplates" query name. */
		public static final String Q_REPORT_TEMPLATES = "qReportTemplates";
		/** The "qSecurityLogs" query name. */
		public static final String Q_SECURITY_LOGS = "qSecurityLogs";
		/** The "qMailLogs" query name. */
		public static final String Q_MAIL_LOGS = "qMailLogs";
		/** The "qMySecurityLogs" query name. */
		public static final String Q_MY_SECURITY_LOGS = "qMySecurityLogs";

		private Queries() {
			// prevent instantiation
		}
	}

	private Admin() {
		// prevent instantiation
	}
}
