package modules.admin.domain;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.XmlType;
import org.skyve.CORE;
import org.skyve.wildcat.domain.AbstractPersistentBean;

/**
 * DataMaintenance
 * 
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
	public static final String selectedBackupTimestampFolderNamePropertyName = "selectedBackupTimestampFolderName";
	/** @hidden */
	public static final String selectedContentIdPropertyName = "selectedContentId";
	/** @hidden */
	public static final String refreshBackupsPropertyName = "refreshBackups";
	/** @hidden */
	public static final String refreshContentPropertyName = "refreshContent";
	/** @hidden */
	public static final String contentLinkPropertyName = "contentLink";

	private String modDocName;
	private String schemaName;
	private String selectedBackupTimestampFolderName;
	private String selectedContentId;
	private Boolean refreshBackups = new Boolean(true);
	private Boolean refreshContent = new Boolean(true);
	private String contentLink;

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
														"Data Maintenance",
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

	@XmlTransient
	public boolean isAttachmentSelected() {
		return (contentLink != null);
	}

	public boolean isNotAttachmentSelected() {
		return (! isAttachmentSelected());
	}

	@XmlTransient
	public boolean isBackupSelected() {
		return (selectedBackupTimestampFolderName != null);
	}

	public boolean isNotBackupSelected() {
		return (! isBackupSelected());
	}

	@XmlTransient
	public boolean isBackupsRefreshRequired() {
		return (Boolean.TRUE.equals(refreshBackups));
	}

	public boolean isNotBackupsRefreshRequired() {
		return (! isBackupsRefreshRequired());
	}

	@XmlTransient
	public boolean isContentRefreshRequired() {
		return (Boolean.TRUE.equals(refreshContent));
	}

	public boolean isNotContentRefreshRequired() {
		return (! isContentRefreshRequired());
	}

	@XmlTransient
	public boolean isContentSelected() {
		return (selectedContentId != null);
	}

	public boolean isNotContentSelected() {
		return (! isContentSelected());
	}
}
