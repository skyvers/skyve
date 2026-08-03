package modules.admin.domain;

import jakarta.annotation.Generated;
import org.skyve.metadata.module.query.ModuleQuery;

/**
 * Compile-time references to the querys declared in the admin module.
 * Generated - local changes will be overwritten.
 */
@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
public enum AdminQuery implements ModuleQuery {
	/** The "qContacts" query. */
	Q_CONTACTS("qContacts"),
	/** The "qContactsByContactType" query. */
	Q_CONTACTS_BY_CONTACT_TYPE("qContactsByContactType"),
	/** The "qUsers" query. */
	Q_USERS("qUsers"),
	/** The "qUserProxies" query. */
	Q_USER_PROXIES("qUserProxies"),
	/** The "qUserTokens" query. */
	Q_USER_TOKENS("qUserTokens"),
	/** The "qActiveUserProxies" query. */
	Q_ACTIVE_USER_PROXIES("qActiveUserProxies"),
	/** The "qUsersInGroup" query. */
	Q_USERS_IN_GROUP("qUsersInGroup"),
	/** The "qUsersByGroup" query. */
	Q_USERS_BY_GROUP("qUsersByGroup"),
	/** The "qLoginHistory" query. */
	Q_LOGIN_HISTORY("qLoginHistory"),
	/** The "qMyLoginHistory" query. */
	Q_MY_LOGIN_HISTORY("qMyLoginHistory"),
	/** The "qCompletedJobs" query. */
	Q_COMPLETED_JOBS("qCompletedJobs"),
	/** The "qDashboardJobs" query. */
	Q_DASHBOARD_JOBS("qDashboardJobs"),
	/** The "qAllSchedules" query. */
	Q_ALL_SCHEDULES("qAllSchedules"),
	/** The "qExistingHits" query. */
	Q_EXISTING_HITS("qExistingHits"),
	/** The "qSnapshots" query. */
	Q_SNAPSHOTS("qSnapshots"),
	/** The "qTags" query. */
	Q_TAGS("qTags"),
	/** The "qAudits" query. */
	Q_AUDITS("qAudits"),
	/** The "qCommunications" query. */
	Q_COMMUNICATIONS("qCommunications"),
	/** The "qCommunicationsForTemplate" query. */
	Q_COMMUNICATIONS_FOR_TEMPLATE("qCommunicationsForTemplate"),
	/** The "qCommunicationTemplate" query. */
	Q_COMMUNICATION_TEMPLATE("qCommunicationTemplate"),
	/** The "qSubscriptions" query. */
	Q_SUBSCRIPTIONS("qSubscriptions"),
	/** The "qUserSubscriptions" query. */
	Q_USER_SUBSCRIPTIONS("qUserSubscriptions"),
	/** The "qTagCommunications" query. */
	Q_TAG_COMMUNICATIONS("qTagCommunications"),
	/** The "qImportExport" query. */
	Q_IMPORT_EXPORT("qImportExport"),
	/** The "qDocumentNumbers" query. */
	Q_DOCUMENT_NUMBERS("qDocumentNumbers"),
	/** The "qEnabledReports" query. */
	Q_ENABLED_REPORTS("qEnabledReports"),
	/** The "qReportSchedules" query. */
	Q_REPORT_SCHEDULES("qReportSchedules"),
	/** The "qReportTemplates" query. */
	Q_REPORT_TEMPLATES("qReportTemplates"),
	/** The "qSecurityLogs" query. */
	Q_SECURITY_LOGS("qSecurityLogs"),
	/** The "qMailLogs" query. */
	Q_MAIL_LOGS("qMailLogs"),
	/** The "qMySecurityLogs" query. */
	Q_MY_SECURITY_LOGS("qMySecurityLogs");

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
