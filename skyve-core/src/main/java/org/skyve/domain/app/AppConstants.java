package org.skyve.domain.app;

public final class AppConstants {
	// Modules
	public static final String ADMIN_MODULE_NAME = "admin";

	// Documents
	public static final String CHANGE_PASSWORD_DOCUMENT_NAME = "ChangePassword";
	public static final String CONTACT_DOCUMENT_NAME = "Contact";
	public static final String COMMUNICATION_DOCUMENT_NAME = "Communication";
	public static final String COMMNICATION_TEMPLATE_DOCUMENT_NAME = "CommunicationTemplate";
	public static final String CONFIGURATION_DOCUMENT_NAME = "Configuration";
	public static final String DOCUMENT_NUMBER_DOCUMENT_NAME = "DocumentNumber";
	public static final String GENERIC_DOCUMENT_NAME = "Generic";
	public static final String GROUP_DOCUMENT_NAME = "Group";
	public static final String GROUP_ROLE_DOCUMENT_NAME = "GroupRole";
	public static final String JOB_DOCUMENT_NAME = "Job";
	public static final String REPORT_DATASET_DOCUMENT_NAME = "ReportDataset";
	public static final String REPORT_PARAMETER_DOCUMENT_NAME = "ReportParameter";
	public static final String REPORT_TEMPLATE_DOCUMENT_NAME = "ReportTemplate";
	public static final String SELF_REGISTRATION_DOCUMENT_NAME = "SelfRegistration";
	public static final String SNAPSHOT_DOCUMENT_NAME = "Snapshot";
	public static final String SUBSCRIPTION_DOCUMENT_NAME = "Subscription";
	public static final String TAG_DOCUMENT_NAME = "Tag";
	public static final String TAGGED_DOCUMENT_NAME = "Tagged";
	public static final String USER_DOCUMENT_NAME = "User";
	public static final String USER_ROLE_DOCUMENT_NAME = "UserRole";

	// Attributes
	public static final String BEAN_BIZID_ATTRIBUTE_NAME = "beanBizId";
	public static final String BEAN_DOCUMENT_NAME_ATTRIBUTE_NAME = "beanDocumentName";
	public static final String BEAN_MODULE_NAME_ATTRIBUTE_NAME = "beanModuleName";
	public static final String CONFIRM_PASSWORD_ATTRIBUTE_NAME = "confirmPassword";
	public static final String CONTACT_ATTRIBUTE_NAME = "contact";
	public static final String CONTACT_TYPE_ATTRIBUTE_NAME = "contactType";
	public static final String DECLINED_ATTRIBUTE_NAME = "declined";
	public static final String DESCRIPTION_ATTRIBUTE_NAME = "description";
	public static final String DISPLAY_NAME_ATTRIBUTE_NAME = "displayName";
	public static final String DOCUMENT_NAME_ATTRIBUTE_NAME = "documentName";
	public static final String DOCUMENT_NUMBER_ATTRIBUTE_NAME = "documentNumber";
	public static final String EMAIL1_ATTRIBUTE_NAME = "email1";
	public static final String ENABLED_ATTRIBUTE_NAME = "enabled";
	public static final String END_TIME_ATTRIBUTE_NAME = "endTime";
	public static final String FROM_EMAIL_ATTRIBUTE_NAME = "fromEmail";
	public static final String GROUPS_ATTRIBUTE_NAME = "groups";
	public static final String LOG_ATTRIBUTE_NAME = "log";
	public static final String MEMO_1_ATTRIBUTE_NAME = "memo1";
	public static final String MODULE_NAME_ATTRIBUTE_NAME = "moduleName";
	public static final String NAME_ATTRIBUTE_NAME = "name";
	public static final String NEW_PASSWORD_ATTRIBUTE_NAME = "newPassword";
	public static final String OLD_PASSWORD_ATTRIBUTE_NAME = "oldPassword";
	public static final String ORDINAL_ATTRIBUTE_NAME = "ordinal";
	public static final String PASSWORD_ATTRIBUTE_NAME = "password";
	public static final String PASSWORD_LAST_CHANGED_ATTRIBUTE_NAME = "passwordLastChanged";
	public static final String PASSWORD_RESET_EMAIL_BODY_ATTRIBUTE_NAME = "passwordResetEmailBody";
	public static final String PASSWORD_RESET_EMAIL_SUBJECT_ATTRIBUTE_NAME = "passwordResetEmailSubject";
	public static final String PASSWORD_RESET_TOKEN_ATTRIBUTE_NAME = "passwordResetToken";
	public static final String PERCENTAGE_COMPLETE_ATTRIBUTE_NAME = "percentComplete";
	public static final String QUERY_NAME_ATTRIBUTE_NAME = "queryName";
	public static final String RECEIVER_IDENTIFIER_ATTRIBUTE_NAME = "receiverIdentifier";
	public static final String RESULTS_ATTRIBUTE_NAME = "results";
	public static final String ROLE_NAME_ATTRIBUTE_NAME = "roleName";
	public static final String ROLES_ATTRIBUTE_NAME = "roles";
	public static final String SEQUENCE_NAME_ATTRIBUTE_NAME = "sequenceName";
	public static final String SNAPSHOT_ATTRIBUTE_NAME = "snapshot";
	public static final String START_TIME_ATTRIBUTE_NAME = "startTime";
	public static final String STATUS_ATTRIBUTE_NAME = "status";
	public static final String TAG_ATTRIBUTE_NAME = "tag";
	public static final String TAGGED_MODULE_ATTRIBUTE_NAME = "taggedModule";
	public static final String TAGGED_DOCUMENT_ATTRIBUTE_NAME = "taggedDocument";
	public static final String TAGGED_BIZID_ATTRIBUTE_NAME = "taggedBizId";
	public static final String TEMPLATE_NAME_ATTRIBUTE_NAME = "templateName";
	public static final String USER_ATTRIBUTE_NAME = "user";
	public static final String USER_NAME_ATTRIBUTE_NAME = "userName";
	
	// Actions
	public static final String MAKE_PASSWORD_CHANGE_ACTION_NAME = "MakePasswordChange";
	public static final String RESEND_ACTIVATION_ACTION_NAME = "ResendActivation";
	
	// Jobs
	public static final String PROCESS_COMMUNICATIONS_FOR_TAG_JOB_NAME = "jProcessCommunicationsForTag";
	
	private AppConstants() {
		// prevent instantiation
	}
}
