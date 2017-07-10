package modules.admin.domain;

import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlEnum;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlSchemaType;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;
import modules.admin.domain.Audit.Operation;
import org.skyve.CORE;
import org.skyve.domain.types.Enumeration;
import org.skyve.domain.types.Timestamp;
import org.skyve.impl.domain.AbstractPersistentBean;
import org.skyve.impl.domain.types.jaxb.TimestampMapper;
import org.skyve.metadata.model.document.Bizlet.DomainValue;

/**
 * Data Maintenance
 * 
 * @depend - - - Operation
 * @depend - - - RefreshOption
 * @navhas n auditUser 0..1 User
 * @navhas n refreshDocuments 0..n DataMaintenanceModuleDocument
 * @stereotype "persistent"
 */
@XmlType
@XmlRootElement
public class DataMaintenance extends AbstractPersistentBean {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String MODULE_NAME = "admin";
	/** @hidden */
	public static final String DOCUMENT_NAME = "DataMaintenance";

	/** @hidden */
	public static final String modDocNamePropertyName = "modDocName";
	/** @hidden */
	public static final String schemaNamePropertyName = "schemaName";
	/** @hidden */
	public static final String refreshDocumentsPropertyName = "refreshDocuments";
	/** @hidden */
	public static final String notificationPropertyName = "notification";
	/** @hidden */
	public static final String ddlScriptPropertyName = "ddlScript";
	/** @hidden */
	public static final String dailyBackupRetentionPropertyName = "dailyBackupRetention";
	/** @hidden */
	public static final String weeklyBackupRetentionPropertyName = "weeklyBackupRetention";
	/** @hidden */
	public static final String monthlyBackupRetentionPropertyName = "monthlyBackupRetention";
	/** @hidden */
	public static final String yearlyBackupRetentionPropertyName = "yearlyBackupRetention";
	/** @hidden */
	public static final String restorePreProcessPropertyName = "restorePreProcess";
	/** @hidden */
	public static final String selectedBackupNamePropertyName = "selectedBackupName";
	/** @hidden */
	public static final String selectedContentIdPropertyName = "selectedContentId";
	/** @hidden */
	public static final String refreshBackupsPropertyName = "refreshBackups";
	/** @hidden */
	public static final String instructionHintPropertyName = "instructionHint";
	/** @hidden */
	public static final String refreshContentPropertyName = "refreshContent";
	/** @hidden */
	public static final String contentLinkPropertyName = "contentLink";
	/** @hidden */
	public static final String auditModuleNamePropertyName = "auditModuleName";
	/** @hidden */
	public static final String auditDocumentNamePropertyName = "auditDocumentName";
	/** @hidden */
	public static final String auditOperationPropertyName = "auditOperation";
	/** @hidden */
	public static final String auditTimestampStartPropertyName = "auditTimestampStart";
	/** @hidden */
	public static final String auditTimestampEndPropertyName = "auditTimestampEnd";
	/** @hidden */
	public static final String auditUserPropertyName = "auditUser";
	/** @hidden */
	public static final String auditMatchCountPropertyName = "auditMatchCount";
	/** @hidden */
	public static final String auditResponsePropertyName = "auditResponse";
	/** @hidden */
	public static final String refreshOptionPropertyName = "refreshOption";

	/**
	 * Option
	 **/
	@XmlEnum
	public static enum RefreshOption implements Enumeration {
		upsert("Upsert", "Upsert"),
		save("Save", "Save");

		private String code;
		private String description;

		/** @hidden */
		private DomainValue domainValue;

		/** @hidden */
		private static List<DomainValue> domainValues;

		private RefreshOption(String code, String description) {
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

		public static RefreshOption fromCode(String code) {
			RefreshOption result = null;

			for (RefreshOption value : values()) {
				if (value.code.equals(code)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static RefreshOption fromDescription(String description) {
			RefreshOption result = null;

			for (RefreshOption value : values()) {
				if (value.description.equals(description)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static List<DomainValue> toDomainValues() {
			if (domainValues == null) {
				RefreshOption[] values = values();
				domainValues = new ArrayList<>(values.length);
				for (RefreshOption value : values) {
					domainValues.add(value.domainValue);
				}
			}

			return domainValues;
		}
	}

	/**
	 * Module.Document
	 **/
	private String modDocName;
	/**
	 * Schema Name
	 **/
	private String schemaName;
	/**
	 * Refresh Documents
	 **/
	private List<DataMaintenanceModuleDocument> refreshDocuments = new ArrayList<>();
	/**
	 * Notify me on completion
	 **/
	private Boolean notification;
	/**
	 * Script
	 **/
	private String ddlScript;
	/**
	 * Daily Backup Retention
	 **/
	private Integer dailyBackupRetention;
	/**
	 * Weekly Backup Retention
	 **/
	private Integer weeklyBackupRetention;
	/**
	 * Monthly Backup Retention
	 **/
	private Integer monthlyBackupRetention;
	/**
	 * Yearly Backup Retention
	 **/
	private Integer yearlyBackupRetention;
	/**
	 * Restore Pre-Process
	 **/
	private String restorePreProcess;
	/**
	 * Selected Backup Name
	 **/
	private String selectedBackupName;
	/**
	 * Selected Content Id
	 **/
	private String selectedContentId;
	/**
	 * Refresh Backups
	 **/
	private Boolean refreshBackups = new Boolean(true);
	/**
	 * Hint
	 **/
	private String instructionHint;
	/**
	 * Refresh Content
	 **/
	private Boolean refreshContent = new Boolean(true);
	/**
	 * Content Link
	 **/
	private String contentLink;
	/**
	 * Module
	 **/
	private String auditModuleName;
	/**
	 * Document
	 **/
	private String auditDocumentName;
	/**
	 * Operation
	 **/
	private Operation auditOperation;
	/**
	 * From
	 **/
	private Timestamp auditTimestampStart;
	/**
	 * To
	 **/
	private Timestamp auditTimestampEnd;
	/**
	 * User
	 **/
	private User auditUser = null;
	/**
	 * Found
	 **/
	private Integer auditMatchCount;
	/**
	 * Status
	 **/
	private String auditResponse;
	/**
	 * Option
	 **/
	private RefreshOption refreshOption;

	@Override
	@XmlTransient
	public String getBizModule() {
		return DataMaintenance.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return DataMaintenance.DOCUMENT_NAME;
	}

	public static DataMaintenance newInstance() throws Exception {
		return CORE.getUser().getCustomer().getModule(MODULE_NAME).getDocument(CORE.getUser().getCustomer(), DOCUMENT_NAME).newInstance(CORE.getUser());
	}

	@Override
	@XmlTransient
	public String getBizKey() {
		try {
			return org.skyve.util.Binder.formatMessage(org.skyve.CORE.getUser().getCustomer(),
														"Data Maintenance{modDocName}",
														this);
		}
		catch (Exception e) {
			return "Unknown";
		}
	}

	@Override
	public boolean equals(Object o) {
		return ((o instanceof DataMaintenance) && 
					this.getBizId().equals(((DataMaintenance) o).getBizId()));
	}

	/**
	 * {@link #modDocName} accessor.
	 * @return	The value.
	 **/
	public String getModDocName() {
		return modDocName;
	}

	/**
	 * {@link #modDocName} mutator.
	 * @param modDocName	The new value.
	 **/
	@XmlElement
	public void setModDocName(String modDocName) {
		preset(modDocNamePropertyName, modDocName);
		this.modDocName = modDocName;
	}

	/**
	 * {@link #schemaName} accessor.
	 * @return	The value.
	 **/
	public String getSchemaName() {
		return schemaName;
	}

	/**
	 * {@link #schemaName} mutator.
	 * @param schemaName	The new value.
	 **/
	@XmlElement
	public void setSchemaName(String schemaName) {
		preset(schemaNamePropertyName, schemaName);
		this.schemaName = schemaName;
	}

	/**
	 * {@link #refreshDocuments} accessor.
	 * @return	The value.
	 **/
	@XmlElement
	public List<DataMaintenanceModuleDocument> getRefreshDocuments() {
		return refreshDocuments;
	}

	/**
	 * {@link #refreshDocuments} accessor.
	 * @param bizId	The bizId of the element in the list.
	 * @return	The value of the element in the list.
	 **/
	public DataMaintenanceModuleDocument getRefreshDocumentsElementById(String bizId) {
		return getElementById(refreshDocuments, bizId);
	}

	/**
	 * {@link #refreshDocuments} mutator.
	 * @param bizId	The bizId of the element in the list.
	 * @param element	The new value of the element in the list.
	 **/
	public void setRefreshDocumentsElementById(@SuppressWarnings("unused") String bizId, DataMaintenanceModuleDocument element) {
		 setElementById(refreshDocuments, element);
	}

	/**
	 * {@link #notification} accessor.
	 * @return	The value.
	 **/
	public Boolean getNotification() {
		return notification;
	}

	/**
	 * {@link #notification} mutator.
	 * @param notification	The new value.
	 **/
	@XmlElement
	public void setNotification(Boolean notification) {
		preset(notificationPropertyName, notification);
		this.notification = notification;
	}

	/**
	 * {@link #ddlScript} accessor.
	 * @return	The value.
	 **/
	public String getDdlScript() {
		return ddlScript;
	}

	/**
	 * {@link #ddlScript} mutator.
	 * @param ddlScript	The new value.
	 **/
	@XmlElement
	public void setDdlScript(String ddlScript) {
		this.ddlScript = ddlScript;
	}

	/**
	 * {@link #dailyBackupRetention} accessor.
	 * @return	The value.
	 **/
	public Integer getDailyBackupRetention() {
		return dailyBackupRetention;
	}

	/**
	 * {@link #dailyBackupRetention} mutator.
	 * @param dailyBackupRetention	The new value.
	 **/
	@XmlElement
	public void setDailyBackupRetention(Integer dailyBackupRetention) {
		preset(dailyBackupRetentionPropertyName, dailyBackupRetention);
		this.dailyBackupRetention = dailyBackupRetention;
	}

	/**
	 * {@link #weeklyBackupRetention} accessor.
	 * @return	The value.
	 **/
	public Integer getWeeklyBackupRetention() {
		return weeklyBackupRetention;
	}

	/**
	 * {@link #weeklyBackupRetention} mutator.
	 * @param weeklyBackupRetention	The new value.
	 **/
	@XmlElement
	public void setWeeklyBackupRetention(Integer weeklyBackupRetention) {
		preset(weeklyBackupRetentionPropertyName, weeklyBackupRetention);
		this.weeklyBackupRetention = weeklyBackupRetention;
	}

	/**
	 * {@link #monthlyBackupRetention} accessor.
	 * @return	The value.
	 **/
	public Integer getMonthlyBackupRetention() {
		return monthlyBackupRetention;
	}

	/**
	 * {@link #monthlyBackupRetention} mutator.
	 * @param monthlyBackupRetention	The new value.
	 **/
	@XmlElement
	public void setMonthlyBackupRetention(Integer monthlyBackupRetention) {
		preset(monthlyBackupRetentionPropertyName, monthlyBackupRetention);
		this.monthlyBackupRetention = monthlyBackupRetention;
	}

	/**
	 * {@link #yearlyBackupRetention} accessor.
	 * @return	The value.
	 **/
	public Integer getYearlyBackupRetention() {
		return yearlyBackupRetention;
	}

	/**
	 * {@link #yearlyBackupRetention} mutator.
	 * @param yearlyBackupRetention	The new value.
	 **/
	@XmlElement
	public void setYearlyBackupRetention(Integer yearlyBackupRetention) {
		preset(yearlyBackupRetentionPropertyName, yearlyBackupRetention);
		this.yearlyBackupRetention = yearlyBackupRetention;
	}

	/**
	 * {@link #restorePreProcess} accessor.
	 * @return	The value.
	 **/
	public String getRestorePreProcess() {
		return restorePreProcess;
	}

	/**
	 * {@link #restorePreProcess} mutator.
	 * @param restorePreProcess	The new value.
	 **/
	@XmlElement
	public void setRestorePreProcess(String restorePreProcess) {
		preset(restorePreProcessPropertyName, restorePreProcess);
		this.restorePreProcess = restorePreProcess;
	}

	/**
	 * {@link #selectedBackupName} accessor.
	 * @return	The value.
	 **/
	public String getSelectedBackupName() {
		return selectedBackupName;
	}

	/**
	 * {@link #selectedBackupName} mutator.
	 * @param selectedBackupName	The new value.
	 **/
	@XmlElement
	public void setSelectedBackupName(String selectedBackupName) {
		this.selectedBackupName = selectedBackupName;
	}

	/**
	 * {@link #selectedContentId} accessor.
	 * @return	The value.
	 **/
	public String getSelectedContentId() {
		return selectedContentId;
	}

	/**
	 * {@link #selectedContentId} mutator.
	 * @param selectedContentId	The new value.
	 **/
	@XmlElement
	public void setSelectedContentId(String selectedContentId) {
		this.selectedContentId = selectedContentId;
	}

	/**
	 * {@link #refreshBackups} accessor.
	 * @return	The value.
	 **/
	public Boolean getRefreshBackups() {
		return refreshBackups;
	}

	/**
	 * {@link #refreshBackups} mutator.
	 * @param refreshBackups	The new value.
	 **/
	@XmlElement
	public void setRefreshBackups(Boolean refreshBackups) {
		this.refreshBackups = refreshBackups;
	}

	/**
	 * {@link #instructionHint} accessor.
	 * @return	The value.
	 **/
	public String getInstructionHint() {
		return instructionHint;
	}

	/**
	 * {@link #instructionHint} mutator.
	 * @param instructionHint	The new value.
	 **/
	@XmlElement
	public void setInstructionHint(String instructionHint) {
		this.instructionHint = instructionHint;
	}

	/**
	 * {@link #refreshContent} accessor.
	 * @return	The value.
	 **/
	public Boolean getRefreshContent() {
		return refreshContent;
	}

	/**
	 * {@link #refreshContent} mutator.
	 * @param refreshContent	The new value.
	 **/
	@XmlElement
	public void setRefreshContent(Boolean refreshContent) {
		this.refreshContent = refreshContent;
	}

	/**
	 * {@link #contentLink} accessor.
	 * @return	The value.
	 **/
	public String getContentLink() {
		return contentLink;
	}

	/**
	 * {@link #contentLink} mutator.
	 * @param contentLink	The new value.
	 **/
	@XmlElement
	public void setContentLink(String contentLink) {
		this.contentLink = contentLink;
	}

	/**
	 * {@link #auditModuleName} accessor.
	 * @return	The value.
	 **/
	public String getAuditModuleName() {
		return auditModuleName;
	}

	/**
	 * {@link #auditModuleName} mutator.
	 * @param auditModuleName	The new value.
	 **/
	@XmlElement
	public void setAuditModuleName(String auditModuleName) {
		preset(auditModuleNamePropertyName, auditModuleName);
		this.auditModuleName = auditModuleName;
	}

	/**
	 * {@link #auditDocumentName} accessor.
	 * @return	The value.
	 **/
	public String getAuditDocumentName() {
		return auditDocumentName;
	}

	/**
	 * {@link #auditDocumentName} mutator.
	 * @param auditDocumentName	The new value.
	 **/
	@XmlElement
	public void setAuditDocumentName(String auditDocumentName) {
		preset(auditDocumentNamePropertyName, auditDocumentName);
		this.auditDocumentName = auditDocumentName;
	}

	/**
	 * {@link #auditOperation} accessor.
	 * @return	The value.
	 **/
	public Operation getAuditOperation() {
		return auditOperation;
	}

	/**
	 * {@link #auditOperation} mutator.
	 * @param auditOperation	The new value.
	 **/
	@XmlElement
	public void setAuditOperation(Operation auditOperation) {
		preset(auditOperationPropertyName, auditOperation);
		this.auditOperation = auditOperation;
	}

	/**
	 * {@link #auditTimestampStart} accessor.
	 * @return	The value.
	 **/
	public Timestamp getAuditTimestampStart() {
		return auditTimestampStart;
	}

	/**
	 * {@link #auditTimestampStart} mutator.
	 * @param auditTimestampStart	The new value.
	 **/
	@XmlSchemaType(name = "dateTime")
	@XmlJavaTypeAdapter(TimestampMapper.class)
	@XmlElement
	public void setAuditTimestampStart(Timestamp auditTimestampStart) {
		preset(auditTimestampStartPropertyName, auditTimestampStart);
		this.auditTimestampStart = auditTimestampStart;
	}

	/**
	 * {@link #auditTimestampEnd} accessor.
	 * @return	The value.
	 **/
	public Timestamp getAuditTimestampEnd() {
		return auditTimestampEnd;
	}

	/**
	 * {@link #auditTimestampEnd} mutator.
	 * @param auditTimestampEnd	The new value.
	 **/
	@XmlSchemaType(name = "dateTime")
	@XmlJavaTypeAdapter(TimestampMapper.class)
	@XmlElement
	public void setAuditTimestampEnd(Timestamp auditTimestampEnd) {
		preset(auditTimestampEndPropertyName, auditTimestampEnd);
		this.auditTimestampEnd = auditTimestampEnd;
	}

	/**
	 * {@link #auditUser} accessor.
	 * @return	The value.
	 **/
	public User getAuditUser() {
		return auditUser;
	}

	/**
	 * {@link #auditUser} mutator.
	 * @param auditUser	The new value.
	 **/
	@XmlElement
	public void setAuditUser(User auditUser) {
		preset(auditUserPropertyName, auditUser);
		this.auditUser = auditUser;
	}

	/**
	 * {@link #auditMatchCount} accessor.
	 * @return	The value.
	 **/
	public Integer getAuditMatchCount() {
		return auditMatchCount;
	}

	/**
	 * {@link #auditMatchCount} mutator.
	 * @param auditMatchCount	The new value.
	 **/
	@XmlElement
	public void setAuditMatchCount(Integer auditMatchCount) {
		this.auditMatchCount = auditMatchCount;
	}

	/**
	 * {@link #auditResponse} accessor.
	 * @return	The value.
	 **/
	public String getAuditResponse() {
		return auditResponse;
	}

	/**
	 * {@link #auditResponse} mutator.
	 * @param auditResponse	The new value.
	 **/
	@XmlElement
	public void setAuditResponse(String auditResponse) {
		this.auditResponse = auditResponse;
	}

	/**
	 * {@link #refreshOption} accessor.
	 * @return	The value.
	 **/
	public RefreshOption getRefreshOption() {
		return refreshOption;
	}

	/**
	 * {@link #refreshOption} mutator.
	 * @param refreshOption	The new value.
	 **/
	@XmlElement
	public void setRefreshOption(RefreshOption refreshOption) {
		this.refreshOption = refreshOption;
	}

	/**
	 * Attachment Selected
	 * @return	The condition

	 */
	@XmlTransient
	public boolean isAttachmentSelected() {
		return (contentLink != null);
	}

	/**	 * {@link #isAttachmentSelected} negation.

	 * @return	The negated condition

	 */
	public boolean isNotAttachmentSelected() {
		return (! isAttachmentSelected());
	}

	/**
	 * Current user is in the role "Audit Manager"
	 * @return	The condition

	 */
	@XmlTransient
	public boolean isAuditManager() {
		return (isUserInRole("admin","AuditManager"));
	}

	/**	 * {@link #isAuditManager} negation.

	 * @return	The negated condition

	 */
	public boolean isNotAuditManager() {
		return (! isAuditManager());
	}

	/**
	 * Backup Selected
	 * @return	The condition

	 */
	@XmlTransient
	public boolean isBackupSelected() {
		return (selectedBackupName != null);
	}

	/**	 * {@link #isBackupSelected} negation.

	 * @return	The negated condition

	 */
	public boolean isNotBackupSelected() {
		return (! isBackupSelected());
	}

	/**
	 * Refresh Backups
	 * @return	The condition

	 */
	@XmlTransient
	public boolean isBackupsRefreshRequired() {
		return (Boolean.TRUE.equals(refreshBackups));
	}

	/**	 * {@link #isBackupsRefreshRequired} negation.

	 * @return	The negated condition

	 */
	public boolean isNotBackupsRefreshRequired() {
		return (! isBackupsRefreshRequired());
	}

	/**
	 * Content Refresh Required
	 * @return	The condition

	 */
	@XmlTransient
	public boolean isContentRefreshRequired() {
		return (Boolean.TRUE.equals(refreshContent));
	}

	/**	 * {@link #isContentRefreshRequired} negation.

	 * @return	The negated condition

	 */
	public boolean isNotContentRefreshRequired() {
		return (! isContentRefreshRequired());
	}

	/**
	 * Content Selected
	 * @return	The condition

	 */
	@XmlTransient
	public boolean isContentSelected() {
		return (selectedContentId != null);
	}

	/**	 * {@link #isContentSelected} negation.

	 * @return	The negated condition

	 */
	public boolean isNotContentSelected() {
		return (! isContentSelected());
	}

	/**
	 * Truncation Job has commenced.
	 * @return	The condition

	 */
	@XmlTransient
	public boolean isJobCommenced() {
		return (auditResponse!=null);
	}

	/**	 * {@link #isJobCommenced} negation.

	 * @return	The negated condition

	 */
	public boolean isNotJobCommenced() {
		return (! isJobCommenced());
	}
}
