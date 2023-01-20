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
import modules.admin.DataMaintenance.DataMaintenanceExtension;
import org.skyve.CORE;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.types.DateTime;
import org.skyve.domain.types.Enumeration;
import org.skyve.impl.domain.AbstractPersistentBean;
import org.skyve.impl.domain.ChangeTrackingArrayList;
import org.skyve.impl.domain.types.jaxb.DateTimeMapper;
import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.util.Util;

/**
 * Data Maintenance
 * 
 * @depend - - - RestorePreProcess
 * @depend - - - ContentRestoreOption
 * @depend - - - RestoreIndexingOption
 * @depend - - - RefreshOption
 * @depend - - - EvictOption
 * @navhas n refreshDocuments 0..n ModuleDocument
 * @stereotype "persistent"
 */
@XmlType
@XmlRootElement
public abstract class DataMaintenance extends AbstractPersistentBean {
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
	public static final String confirmPasswordPropertyName = "confirmPassword";

	/** @hidden */
	public static final String injectBootstrapUserPropertyName = "injectBootstrapUser";

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
	public static final String contentRestoreOptionPropertyName = "contentRestoreOption";

	/** @hidden */
	public static final String restoreIndexingOptionPropertyName = "restoreIndexingOption";

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
	public static final String epochDatePropertyName = "epochDate";

	/** @hidden */
	public static final String auditLogRetentionDaysPropertyName = "auditLogRetentionDays";

	/** @hidden */
	public static final String auditResponsePropertyName = "auditResponse";

	/** @hidden */
	public static final String refreshOptionPropertyName = "refreshOption";

	/** @hidden */
	public static final String evictOptionPropertyName = "evictOption";

	/** @hidden */
	public static final String flagFailedPropertyName = "flagFailed";

	/**
	 * Pre-Process
	 **/
	@XmlEnum
	public static enum RestorePreProcess implements Enumeration {
		noProcessing("noProcessing", "No Processing"),
		dropTablesUsingMetadataRecreateTablesFromBackupCreatesql("dropUsingMetadataAndCreateUsingBackup", "Drop tables using metadata & recreate tables from backup create.sql"),
		dropTablesUsingBackupDropsqlRecreateTablesFromBackupCreatesql("dropUsingBackupAndCreateUsingBackup", "Drop tables using backup drop.sql & recreate tables from backup create.sql"),
		dropTablesUsingMetadataRecreateTablesFromMetadata("dropUsingMetadataAndCreateUsingMetadata", "Drop tables using metadata & recreate tables from metadata"),
		dropTablesUsingBackupDropsqlRecreateTablesFromMetadata("dropUsingBackupAndCreateUsingMetadata", "Drop tables using backup drop.sql & recreate tables from metadata"),
		createTablesFromBackup("createUsingBackup", "Create tables from backup"),
		createTablesFromMetadata("createUsingMetadata", "Create tables from metadata"),
		deleteExistingTableDataUsingMetadata("deleteData", "Delete existing table data using metadata");

		private String code;
		private String description;

		/** @hidden */
		private DomainValue domainValue;

		/** @hidden */
		private static List<DomainValue> domainValues;

		private RestorePreProcess(String code, String description) {
			this.code = code;
			this.description = description;
			this.domainValue = new DomainValue(code, description);
		}

		@Override
		public String toCode() {
			return code;
		}

		@Override
		public String toLocalisedDescription() {
			return Util.i18n(description);
		}

		@Override
		public DomainValue toDomainValue() {
			return domainValue;
		}

		public static RestorePreProcess fromCode(String code) {
			RestorePreProcess result = null;

			for (RestorePreProcess value : values()) {
				if (value.code.equals(code)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static RestorePreProcess fromLocalisedDescription(String description) {
			RestorePreProcess result = null;

			for (RestorePreProcess value : values()) {
				if (value.toLocalisedDescription().equals(description)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static List<DomainValue> toDomainValues() {
			if (domainValues == null) {
				RestorePreProcess[] values = values();
				domainValues = new ArrayList<>(values.length);
				for (RestorePreProcess value : values) {
					domainValues.add(value.domainValue);
				}
			}

			return domainValues;
		}
	}

	/**
	 * Content Option
	 **/
	@XmlEnum
	public static enum ContentRestoreOption implements Enumeration {
		clearOrphanedContentIDs("clearOrphanedContentIds", "Clear Orphaned Content IDs"),
		saveOrphanedContentIDs("saveOrphanedContentIds", "Save Orphaned Content IDs"),
		error("error", "Error");

		private String code;
		private String description;

		/** @hidden */
		private DomainValue domainValue;

		/** @hidden */
		private static List<DomainValue> domainValues;

		private ContentRestoreOption(String code, String description) {
			this.code = code;
			this.description = description;
			this.domainValue = new DomainValue(code, description);
		}

		@Override
		public String toCode() {
			return code;
		}

		@Override
		public String toLocalisedDescription() {
			return Util.i18n(description);
		}

		@Override
		public DomainValue toDomainValue() {
			return domainValue;
		}

		public static ContentRestoreOption fromCode(String code) {
			ContentRestoreOption result = null;

			for (ContentRestoreOption value : values()) {
				if (value.code.equals(code)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static ContentRestoreOption fromLocalisedDescription(String description) {
			ContentRestoreOption result = null;

			for (ContentRestoreOption value : values()) {
				if (value.toLocalisedDescription().equals(description)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static List<DomainValue> toDomainValues() {
			if (domainValues == null) {
				ContentRestoreOption[] values = values();
				domainValues = new ArrayList<>(values.length);
				for (ContentRestoreOption value : values) {
					domainValues.add(value.domainValue);
				}
			}

			return domainValues;
		}
	}

	/**
	 * Indexing Option
	 **/
	@XmlEnum
	public static enum RestoreIndexingOption implements Enumeration {
		data("data", "Data"),
		content("content", "Content"),
		both("both", "Both"),
		none("none", "None");

		private String code;
		private String description;

		/** @hidden */
		private DomainValue domainValue;

		/** @hidden */
		private static List<DomainValue> domainValues;

		private RestoreIndexingOption(String code, String description) {
			this.code = code;
			this.description = description;
			this.domainValue = new DomainValue(code, description);
		}

		@Override
		public String toCode() {
			return code;
		}

		@Override
		public String toLocalisedDescription() {
			return Util.i18n(description);
		}

		@Override
		public DomainValue toDomainValue() {
			return domainValue;
		}

		public static RestoreIndexingOption fromCode(String code) {
			RestoreIndexingOption result = null;

			for (RestoreIndexingOption value : values()) {
				if (value.code.equals(code)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static RestoreIndexingOption fromLocalisedDescription(String description) {
			RestoreIndexingOption result = null;

			for (RestoreIndexingOption value : values()) {
				if (value.toLocalisedDescription().equals(description)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static List<DomainValue> toDomainValues() {
			if (domainValues == null) {
				RestoreIndexingOption[] values = values();
				domainValues = new ArrayList<>(values.length);
				for (RestoreIndexingOption value : values) {
					domainValues.add(value.domainValue);
				}
			}

			return domainValues;
		}
	}

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
		public String toLocalisedDescription() {
			return Util.i18n(description);
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

		public static RefreshOption fromLocalisedDescription(String description) {
			RefreshOption result = null;

			for (RefreshOption value : values()) {
				if (value.toLocalisedDescription().equals(description)) {
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
	 * Cache Evict
	 * <br/>
	 * <p>Whether to evict each bean after processing.</p>
<p>Evicting beans will free memory for large data jobs, however there may be impacts if the action (processing) selected affects items that other beans may reference.</p>
	 **/
	@XmlEnum
	public static enum EvictOption implements Enumeration {
		bean("Bean", "Bean"),
		none("None", "None"),
		all("All", "All");

		private String code;
		private String description;

		/** @hidden */
		private DomainValue domainValue;

		/** @hidden */
		private static List<DomainValue> domainValues;

		private EvictOption(String code, String description) {
			this.code = code;
			this.description = description;
			this.domainValue = new DomainValue(code, description);
		}

		@Override
		public String toCode() {
			return code;
		}

		@Override
		public String toLocalisedDescription() {
			return Util.i18n(description);
		}

		@Override
		public DomainValue toDomainValue() {
			return domainValue;
		}

		public static EvictOption fromCode(String code) {
			EvictOption result = null;

			for (EvictOption value : values()) {
				if (value.code.equals(code)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static EvictOption fromLocalisedDescription(String description) {
			EvictOption result = null;

			for (EvictOption value : values()) {
				if (value.toLocalisedDescription().equals(description)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static List<DomainValue> toDomainValues() {
			if (domainValues == null) {
				EvictOption[] values = values();
				domainValues = new ArrayList<>(values.length);
				for (EvictOption value : values) {
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
	 * Confirm password
	 **/
	private String confirmPassword;

	/**
	 * Inject bootstrap user
	 **/
	private Boolean injectBootstrapUser;

	/**
	 * Schema Name
	 **/
	private String schemaName;

	/**
	 * Refresh Documents
	 **/
	private List<ModuleDocument> refreshDocuments = new ChangeTrackingArrayList<>("refreshDocuments", this);

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
	 * Pre-Process
	 **/
	private RestorePreProcess restorePreProcess;

	/**
	 * Content Option
	 **/
	private ContentRestoreOption contentRestoreOption = ContentRestoreOption.error;

	/**
	 * Indexing Option
	 **/
	private RestoreIndexingOption restoreIndexingOption = RestoreIndexingOption.both;

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
	private Boolean refreshBackups = Boolean.valueOf(true);

	/**
	 * admin.dataMaintenance.instructionHint.displayName
	 **/
	private String instructionHint;

	/**
	 * Refresh Content
	 **/
	private Boolean refreshContent = Boolean.valueOf(true);

	/**
	 * Content Link
	 **/
	private String contentLink;

	/**
	 * Epoch Date
	 **/
	private DateTime epochDate;

	/**
	 * Audit Log Retention Days
	 * <br/>
	 * How many days worth of Audits to keep. Audits earlier than this many days back will be pruned.
	 **/
	private Integer auditLogRetentionDays;

	/**
	 * Audit Response
	 **/
	private String auditResponse;

	/**
	 * Option
	 **/
	private RefreshOption refreshOption;

	/**
	 * Cache Evict
	 * <br/>
	 * <p>Whether to evict each bean after processing.</p>
<p>Evicting beans will free memory for large data jobs, however there may be impacts if the action (processing) selected affects items that other beans may reference.</p>
	 **/
	private EvictOption evictOption = EvictOption.bean;

	/**
	 * Flag Failed 
	 * <br/>
	 * Flag records that fail to Save/Upsert
	 **/
	private Boolean flagFailed = Boolean.valueOf(false);

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

	public static DataMaintenanceExtension newInstance() {
		try {
			return CORE.getUser().getCustomer().getModule(MODULE_NAME).getDocument(CORE.getUser().getCustomer(), DOCUMENT_NAME).newInstance(CORE.getUser());
		}
		catch (RuntimeException e) {
			throw e;
		}
		catch (Exception e) {
			throw new DomainException(e);
		}
	}

	@Override
	@XmlTransient
	public String getBizKey() {
		try {
			return org.skyve.util.Binder.formatMessage("Data Maintenance", this);
		}
		catch (@SuppressWarnings("unused") Exception e) {
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
	 * {@link #confirmPassword} accessor.
	 * @return	The value.
	 **/
	public String getConfirmPassword() {
		return confirmPassword;
	}

	/**
	 * {@link #confirmPassword} mutator.
	 * @param confirmPassword	The new value.
	 **/
	@XmlElement
	public void setConfirmPassword(String confirmPassword) {
		this.confirmPassword = confirmPassword;
	}

	/**
	 * {@link #injectBootstrapUser} accessor.
	 * @return	The value.
	 **/
	public Boolean getInjectBootstrapUser() {
		return injectBootstrapUser;
	}

	/**
	 * {@link #injectBootstrapUser} mutator.
	 * @param injectBootstrapUser	The new value.
	 **/
	@XmlElement
	public void setInjectBootstrapUser(Boolean injectBootstrapUser) {
		preset(injectBootstrapUserPropertyName, injectBootstrapUser);
		this.injectBootstrapUser = injectBootstrapUser;
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
	public List<ModuleDocument> getRefreshDocuments() {
		return refreshDocuments;
	}

	/**
	 * {@link #refreshDocuments} accessor.
	 * @param bizId	The bizId of the element in the list.
	 * @return	The value of the element in the list.
	 **/
	public ModuleDocument getRefreshDocumentsElementById(String bizId) {
		return getElementById(refreshDocuments, bizId);
	}

	/**
	 * {@link #refreshDocuments} mutator.
	 * @param bizId	The bizId of the element in the list.
	 * @param element	The new value of the element in the list.
	 **/
	public void setRefreshDocumentsElementById(String bizId, ModuleDocument element) {
		setElementById(refreshDocuments, element);
	}

	/**
	 * {@link #refreshDocuments} add.
	 * @param element	The element to add.
	 **/
	public boolean addRefreshDocumentsElement(ModuleDocument element) {
		return refreshDocuments.add(element);
	}

	/**
	 * {@link #refreshDocuments} add.
	 * @param index	The index in the list to add the element to.
	 * @param element	The element to add.
	 **/
	public void addRefreshDocumentsElement(int index, ModuleDocument element) {
		refreshDocuments.add(index, element);
	}

	/**
	 * {@link #refreshDocuments} remove.
	 * @param element	The element to remove.
	 **/
	public boolean removeRefreshDocumentsElement(ModuleDocument element) {
		return refreshDocuments.remove(element);
	}

	/**
	 * {@link #refreshDocuments} remove.
	 * @param index	The index in the list to remove the element from.
	 **/
	public ModuleDocument removeRefreshDocumentsElement(int index) {
		return refreshDocuments.remove(index);
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
	public RestorePreProcess getRestorePreProcess() {
		return restorePreProcess;
	}

	/**
	 * {@link #restorePreProcess} mutator.
	 * @param restorePreProcess	The new value.
	 **/
	@XmlElement
	public void setRestorePreProcess(RestorePreProcess restorePreProcess) {
		preset(restorePreProcessPropertyName, restorePreProcess);
		this.restorePreProcess = restorePreProcess;
	}

	/**
	 * {@link #contentRestoreOption} accessor.
	 * @return	The value.
	 **/
	public ContentRestoreOption getContentRestoreOption() {
		return contentRestoreOption;
	}

	/**
	 * {@link #contentRestoreOption} mutator.
	 * @param contentRestoreOption	The new value.
	 **/
	@XmlElement
	public void setContentRestoreOption(ContentRestoreOption contentRestoreOption) {
		preset(contentRestoreOptionPropertyName, contentRestoreOption);
		this.contentRestoreOption = contentRestoreOption;
	}

	/**
	 * {@link #restoreIndexingOption} accessor.
	 * @return	The value.
	 **/
	public RestoreIndexingOption getRestoreIndexingOption() {
		return restoreIndexingOption;
	}

	/**
	 * {@link #restoreIndexingOption} mutator.
	 * @param restoreIndexingOption	The new value.
	 **/
	@XmlElement
	public void setRestoreIndexingOption(RestoreIndexingOption restoreIndexingOption) {
		preset(restoreIndexingOptionPropertyName, restoreIndexingOption);
		this.restoreIndexingOption = restoreIndexingOption;
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
	 * {@link #epochDate} accessor.
	 * @return	The value.
	 **/
	public DateTime getEpochDate() {
		return epochDate;
	}

	/**
	 * {@link #epochDate} mutator.
	 * @param epochDate	The new value.
	 **/
	@XmlElement
	@XmlSchemaType(name = "dateTime")
	@XmlJavaTypeAdapter(DateTimeMapper.class)
	public void setEpochDate(DateTime epochDate) {
		preset(epochDatePropertyName, epochDate);
		this.epochDate = epochDate;
	}

	/**
	 * {@link #auditLogRetentionDays} accessor.
	 * @return	The value.
	 **/
	public Integer getAuditLogRetentionDays() {
		return auditLogRetentionDays;
	}

	/**
	 * {@link #auditLogRetentionDays} mutator.
	 * @param auditLogRetentionDays	The new value.
	 **/
	@XmlElement
	public void setAuditLogRetentionDays(Integer auditLogRetentionDays) {
		preset(auditLogRetentionDaysPropertyName, auditLogRetentionDays);
		this.auditLogRetentionDays = auditLogRetentionDays;
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
	 * {@link #evictOption} accessor.
	 * @return	The value.
	 **/
	public EvictOption getEvictOption() {
		return evictOption;
	}

	/**
	 * {@link #evictOption} mutator.
	 * @param evictOption	The new value.
	 **/
	@XmlElement
	public void setEvictOption(EvictOption evictOption) {
		this.evictOption = evictOption;
	}

	/**
	 * {@link #flagFailed} accessor.
	 * @return	The value.
	 **/
	public Boolean getFlagFailed() {
		return flagFailed;
	}

	/**
	 * {@link #flagFailed} mutator.
	 * @param flagFailed	The new value.
	 **/
	@XmlElement
	public void setFlagFailed(Boolean flagFailed) {
		this.flagFailed = flagFailed;
	}

	/**
	 * Attachment Selected
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isAttachmentSelected() {
		return (contentLink != null);
	}

	/**
	 * {@link #isAttachmentSelected} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotAttachmentSelected() {
		return (! isAttachmentSelected());
	}

	/**
	 * Current user is in the role "Audit Manager"
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isAuditManager() {
		return (isUserInRole("admin","AuditManager"));
	}

	/**
	 * {@link #isAuditManager} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotAuditManager() {
		return (! isAuditManager());
	}

	/**
	 * Backup Selected
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isBackupSelected() {
		return (selectedBackupName != null);
	}

	/**
	 * {@link #isBackupSelected} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotBackupSelected() {
		return (! isBackupSelected());
	}

	/**
	 * Refresh Backups
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isBackupsRefreshRequired() {
		return (Boolean.TRUE.equals(refreshBackups));
	}

	/**
	 * {@link #isBackupsRefreshRequired} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotBackupsRefreshRequired() {
		return (! isBackupsRefreshRequired());
	}

	/**
	 * Content Refresh Required
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isContentRefreshRequired() {
		return (Boolean.TRUE.equals(refreshContent));
	}

	/**
	 * {@link #isContentRefreshRequired} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotContentRefreshRequired() {
		return (! isContentRefreshRequired());
	}

	/**
	 * Content Selected
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isContentSelected() {
		return (selectedContentId != null);
	}

	/**
	 * {@link #isContentSelected} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotContentSelected() {
		return (! isContentSelected());
	}

	/**
	 * Truncation Job has commenced.
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isJobCommenced() {
		return (auditResponse!=null);
	}

	/**
	 * {@link #isJobCommenced} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotJobCommenced() {
		return (! isJobCommenced());
	}
}
