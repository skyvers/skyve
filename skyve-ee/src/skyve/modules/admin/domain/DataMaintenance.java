package modules.admin.domain;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlSchemaType;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;
import modules.admin.domain.Audit.Operation;
import org.skyve.CORE;
import org.skyve.domain.types.Timestamp;
import org.skyve.impl.domain.AbstractPersistentBean;
import org.skyve.impl.domain.types.jaxb.TimestampMapper;

/**
 * DataMaintenance
 * 
 * @depend - - - Operation
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
	public static final String dailyBackupRetentionPropertyName = "dailyBackupRetention";
	/** @hidden */
	public static final String weeklyBackupRetentionPropertyName = "weeklyBackupRetention";
	/** @hidden */
	public static final String monthlyBackupRetentionPropertyName = "monthlyBackupRetention";
	/** @hidden */
	public static final String yearlyBackupRetentionPropertyName = "yearlyBackupRetention";
	/** @hidden */
	public static final String selectedBackupTimestampFolderNamePropertyName = "selectedBackupTimestampFolderName";
	/** @hidden */
	public static final String selectedContentIdPropertyName = "selectedContentId";
	/** @hidden */
	public static final String refreshBackupsPropertyName = "refreshBackups";
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
	public static final String auditUserNamePropertyName = "auditUserName";
	/** @hidden */
	public static final String auditMatchCountPropertyName = "auditMatchCount";
	/** @hidden */
	public static final String auditResponsePropertyName = "auditResponse";

	private String modDocName;
	private String schemaName;
	private Integer dailyBackupRetention;
	private Integer weeklyBackupRetention;
	private Integer monthlyBackupRetention;
	private Integer yearlyBackupRetention;
	private String selectedBackupTimestampFolderName;
	private String selectedContentId;
	private Boolean refreshBackups = new Boolean(true);
	private Boolean refreshContent = new Boolean(true);
	private String contentLink;
	private String auditModuleName;
	private String auditDocumentName;
	private Operation auditOperation;
	private Timestamp auditTimestampStart;
	private Timestamp auditTimestampEnd;
	private String auditUserName;
	private Integer auditMatchCount;
	private String auditResponse;

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
	 **/
	public String getModDocName() {
		return modDocName;
	}

	/**
	 * {@link #modDocName} mutator.
	 * 
	 * @param modDocName	The new value to set.
	 **/
	@XmlElement
	public void setModDocName(String modDocName) {
		preset(modDocNamePropertyName, modDocName);
		this.modDocName = modDocName;
	}

	/**
	 * {@link #schemaName} accessor.
	 **/
	public String getSchemaName() {
		return schemaName;
	}

	/**
	 * {@link #schemaName} mutator.
	 * 
	 * @param schemaName	The new value to set.
	 **/
	@XmlElement
	public void setSchemaName(String schemaName) {
		preset(schemaNamePropertyName, schemaName);
		this.schemaName = schemaName;
	}

	/**
	 * {@link #dailyBackupRetention} accessor.
	 **/
	public Integer getDailyBackupRetention() {
		return dailyBackupRetention;
	}

	/**
	 * {@link #dailyBackupRetention} mutator.
	 * 
	 * @param dailyBackupRetention	The new value to set.
	 **/
	@XmlElement
	public void setDailyBackupRetention(Integer dailyBackupRetention) {
		preset(dailyBackupRetentionPropertyName, dailyBackupRetention);
		this.dailyBackupRetention = dailyBackupRetention;
	}

	/**
	 * {@link #weeklyBackupRetention} accessor.
	 **/
	public Integer getWeeklyBackupRetention() {
		return weeklyBackupRetention;
	}

	/**
	 * {@link #weeklyBackupRetention} mutator.
	 * 
	 * @param weeklyBackupRetention	The new value to set.
	 **/
	@XmlElement
	public void setWeeklyBackupRetention(Integer weeklyBackupRetention) {
		preset(weeklyBackupRetentionPropertyName, weeklyBackupRetention);
		this.weeklyBackupRetention = weeklyBackupRetention;
	}

	/**
	 * {@link #monthlyBackupRetention} accessor.
	 **/
	public Integer getMonthlyBackupRetention() {
		return monthlyBackupRetention;
	}

	/**
	 * {@link #monthlyBackupRetention} mutator.
	 * 
	 * @param monthlyBackupRetention	The new value to set.
	 **/
	@XmlElement
	public void setMonthlyBackupRetention(Integer monthlyBackupRetention) {
		preset(monthlyBackupRetentionPropertyName, monthlyBackupRetention);
		this.monthlyBackupRetention = monthlyBackupRetention;
	}

	/**
	 * {@link #yearlyBackupRetention} accessor.
	 **/
	public Integer getYearlyBackupRetention() {
		return yearlyBackupRetention;
	}

	/**
	 * {@link #yearlyBackupRetention} mutator.
	 * 
	 * @param yearlyBackupRetention	The new value to set.
	 **/
	@XmlElement
	public void setYearlyBackupRetention(Integer yearlyBackupRetention) {
		preset(yearlyBackupRetentionPropertyName, yearlyBackupRetention);
		this.yearlyBackupRetention = yearlyBackupRetention;
	}

	/**
	 * {@link #selectedBackupTimestampFolderName} accessor.
	 **/
	public String getSelectedBackupTimestampFolderName() {
		return selectedBackupTimestampFolderName;
	}

	/**
	 * {@link #selectedBackupTimestampFolderName} mutator.
	 * 
	 * @param selectedBackupTimestampFolderName	The new value to set.
	 **/
	@XmlElement
	public void setSelectedBackupTimestampFolderName(String selectedBackupTimestampFolderName) {
		this.selectedBackupTimestampFolderName = selectedBackupTimestampFolderName;
	}

	/**
	 * {@link #selectedContentId} accessor.
	 **/
	public String getSelectedContentId() {
		return selectedContentId;
	}

	/**
	 * {@link #selectedContentId} mutator.
	 * 
	 * @param selectedContentId	The new value to set.
	 **/
	@XmlElement
	public void setSelectedContentId(String selectedContentId) {
		this.selectedContentId = selectedContentId;
	}

	/**
	 * {@link #refreshBackups} accessor.
	 **/
	public Boolean getRefreshBackups() {
		return refreshBackups;
	}

	/**
	 * {@link #refreshBackups} mutator.
	 * 
	 * @param refreshBackups	The new value to set.
	 **/
	@XmlElement
	public void setRefreshBackups(Boolean refreshBackups) {
		this.refreshBackups = refreshBackups;
	}

	/**
	 * {@link #refreshContent} accessor.
	 **/
	public Boolean getRefreshContent() {
		return refreshContent;
	}

	/**
	 * {@link #refreshContent} mutator.
	 * 
	 * @param refreshContent	The new value to set.
	 **/
	@XmlElement
	public void setRefreshContent(Boolean refreshContent) {
		this.refreshContent = refreshContent;
	}

	/**
	 * {@link #contentLink} accessor.
	 **/
	public String getContentLink() {
		return contentLink;
	}

	/**
	 * {@link #contentLink} mutator.
	 * 
	 * @param contentLink	The new value to set.
	 **/
	@XmlElement
	public void setContentLink(String contentLink) {
		this.contentLink = contentLink;
	}

	/**
	 * {@link #auditModuleName} accessor.
	 **/
	public String getAuditModuleName() {
		return auditModuleName;
	}

	/**
	 * {@link #auditModuleName} mutator.
	 * 
	 * @param auditModuleName	The new value to set.
	 **/
	@XmlElement
	public void setAuditModuleName(String auditModuleName) {
		preset(auditModuleNamePropertyName, auditModuleName);
		this.auditModuleName = auditModuleName;
	}

	/**
	 * {@link #auditDocumentName} accessor.
	 **/
	public String getAuditDocumentName() {
		return auditDocumentName;
	}

	/**
	 * {@link #auditDocumentName} mutator.
	 * 
	 * @param auditDocumentName	The new value to set.
	 **/
	@XmlElement
	public void setAuditDocumentName(String auditDocumentName) {
		preset(auditDocumentNamePropertyName, auditDocumentName);
		this.auditDocumentName = auditDocumentName;
	}

	/**
	 * {@link #auditOperation} accessor.
	 **/
	public Operation getAuditOperation() {
		return auditOperation;
	}

	/**
	 * {@link #auditOperation} mutator.
	 * 
	 * @param auditOperation	The new value to set.
	 **/
	@XmlElement
	public void setAuditOperation(Operation auditOperation) {
		preset(auditOperationPropertyName, auditOperation);
		this.auditOperation = auditOperation;
	}

	/**
	 * {@link #auditTimestampStart} accessor.
	 **/
	public Timestamp getAuditTimestampStart() {
		return auditTimestampStart;
	}

	/**
	 * {@link #auditTimestampStart} mutator.
	 * 
	 * @param auditTimestampStart	The new value to set.
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
	 **/
	public Timestamp getAuditTimestampEnd() {
		return auditTimestampEnd;
	}

	/**
	 * {@link #auditTimestampEnd} mutator.
	 * 
	 * @param auditTimestampEnd	The new value to set.
	 **/
	@XmlSchemaType(name = "dateTime")
	@XmlJavaTypeAdapter(TimestampMapper.class)
	@XmlElement
	public void setAuditTimestampEnd(Timestamp auditTimestampEnd) {
		preset(auditTimestampEndPropertyName, auditTimestampEnd);
		this.auditTimestampEnd = auditTimestampEnd;
	}

	/**
	 * {@link #auditUserName} accessor.
	 **/
	public String getAuditUserName() {
		return auditUserName;
	}

	/**
	 * {@link #auditUserName} mutator.
	 * 
	 * @param auditUserName	The new value to set.
	 **/
	@XmlElement
	public void setAuditUserName(String auditUserName) {
		preset(auditUserNamePropertyName, auditUserName);
		this.auditUserName = auditUserName;
	}

	/**
	 * {@link #auditMatchCount} accessor.
	 **/
	public Integer getAuditMatchCount() {
		return auditMatchCount;
	}

	/**
	 * {@link #auditMatchCount} mutator.
	 * 
	 * @param auditMatchCount	The new value to set.
	 **/
	@XmlElement
	public void setAuditMatchCount(Integer auditMatchCount) {
		this.auditMatchCount = auditMatchCount;
	}

	/**
	 * {@link #auditResponse} accessor.
	 **/
	public String getAuditResponse() {
		return auditResponse;
	}

	/**
	 * {@link #auditResponse} mutator.
	 * 
	 * @param auditResponse	The new value to set.
	 **/
	@XmlElement
	public void setAuditResponse(String auditResponse) {
		this.auditResponse = auditResponse;
	}

	/**
	 * Attachment Selected
	 */
	@XmlTransient
	public boolean isAttachmentSelected() {
		return (contentLink != null);
	}

	public boolean isNotAttachmentSelected() {
		return (! isAttachmentSelected());
	}

	/**
	 * Current user is in the role "Audit Manager"
	 */
	@XmlTransient
	public boolean isAuditManager() {
		return (isUserInRole("admin","AuditManager"));
	}

	public boolean isNotAuditManager() {
		return (! isAuditManager());
	}

	/**
	 * Backup Selected
	 */
	@XmlTransient
	public boolean isBackupSelected() {
		return (selectedBackupTimestampFolderName != null);
	}

	public boolean isNotBackupSelected() {
		return (! isBackupSelected());
	}

	/**
	 * Refresh Backups
	 */
	@XmlTransient
	public boolean isBackupsRefreshRequired() {
		return (Boolean.TRUE.equals(refreshBackups));
	}

	public boolean isNotBackupsRefreshRequired() {
		return (! isBackupsRefreshRequired());
	}

	/**
	 * Content Refresh Required
	 */
	@XmlTransient
	public boolean isContentRefreshRequired() {
		return (Boolean.TRUE.equals(refreshContent));
	}

	public boolean isNotContentRefreshRequired() {
		return (! isContentRefreshRequired());
	}

	/**
	 * Content Selected
	 */
	@XmlTransient
	public boolean isContentSelected() {
		return (selectedContentId != null);
	}

	public boolean isNotContentSelected() {
		return (! isContentSelected());
	}

	/**
	 * Truncation Job has commenced.
	 */
	@XmlTransient
	public boolean isJobCommenced() {
		return (auditResponse!=null);
	}

	public boolean isNotJobCommenced() {
		return (! isJobCommenced());
	}
}
