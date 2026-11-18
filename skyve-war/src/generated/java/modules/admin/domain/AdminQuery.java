package modules.admin.domain;

import jakarta.annotation.Generated;
import org.skyve.metadata.module.query.ModuleQuery;

/**
 * Compile-time references to the metadata queries declared in the admin module.
 * Generated - local changes will be overwritten.
 */
@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
public enum AdminQuery implements ModuleQuery {
	/** The "qContacts" metadata query. */
	Q_CONTACTS("qContacts"),
	/** The "qContactsByContactType" metadata query. */
	Q_CONTACTS_BY_CONTACT_TYPE("qContactsByContactType"),
	/** The "qUsers" metadata query. */
	Q_USERS("qUsers"),
	/** The "qUserProxies" metadata query. */
	Q_USER_PROXIES("qUserProxies"),
	/** The "qUserTokens" metadata query. */
	Q_USER_TOKENS("qUserTokens"),
	/** The "qActiveUserProxies" metadata query. */
	Q_ACTIVE_USER_PROXIES("qActiveUserProxies"),
	/** The "qUsersInGroup" metadata query. */
	Q_USERS_IN_GROUP("qUsersInGroup"),
	/** The "qUsersByGroup" metadata query. */
	Q_USERS_BY_GROUP("qUsersByGroup"),
	/** The "qLoginHistory" metadata query. */
	Q_LOGIN_HISTORY("qLoginHistory"),
	/** The "qMyLoginHistory" metadata query. */
	Q_MY_LOGIN_HISTORY("qMyLoginHistory"),
	/** The "qCompletedJobs" metadata query. */
	Q_COMPLETED_JOBS("qCompletedJobs"),
	/** The "qDashboardJobs" metadata query. */
	Q_DASHBOARD_JOBS("qDashboardJobs"),
	/** The "qAllSchedules" metadata query. */
	Q_ALL_SCHEDULES("qAllSchedules"),
	/** The "qExistingHits" metadata query. */
	Q_EXISTING_HITS("qExistingHits"),
	/** The "qSnapshots" metadata query. */
	Q_SNAPSHOTS("qSnapshots"),
	/** The "qTags" metadata query. */
	Q_TAGS("qTags"),
	/** The "qAudits" metadata query. */
	Q_AUDITS("qAudits"),
	/** The "qCommunications" metadata query. */
	Q_COMMUNICATIONS("qCommunications"),
	/** The "qCommunicationsForTemplate" metadata query. */
	Q_COMMUNICATIONS_FOR_TEMPLATE("qCommunicationsForTemplate"),
	/** The "qCommunicationTemplate" metadata query. */
	Q_COMMUNICATION_TEMPLATE("qCommunicationTemplate"),
	/** The "qSubscriptions" metadata query. */
	Q_SUBSCRIPTIONS("qSubscriptions"),
	/** The "qUserSubscriptions" metadata query. */
	Q_USER_SUBSCRIPTIONS("qUserSubscriptions"),
	/** The "qTagCommunications" metadata query. */
	Q_TAG_COMMUNICATIONS("qTagCommunications"),
	/** The "qImportExport" metadata query. */
	Q_IMPORT_EXPORT("qImportExport"),
	/** The "qDocumentNumbers" metadata query. */
	Q_DOCUMENT_NUMBERS("qDocumentNumbers"),
	/** The "qEnabledReports" metadata query. */
	Q_ENABLED_REPORTS("qEnabledReports"),
	/** The "qReportSchedules" metadata query. */
	Q_REPORT_SCHEDULES("qReportSchedules"),
	/** The "qReportTemplates" metadata query. */
	Q_REPORT_TEMPLATES("qReportTemplates"),
	/** The "qSecurityLogs" metadata query. */
	Q_SECURITY_LOGS("qSecurityLogs"),
	/** The "qMailLogs" metadata query. */
	Q_MAIL_LOGS("qMailLogs");

	private final String name;

	private AdminQuery(String name) {
		this.name = name;
	}

	@Override
	public String moduleName() {
		return "admin";
	}

	@Override
	public String queryName() {
		return name;
	}
}
