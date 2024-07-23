package modules.admin.domain;

import jakarta.annotation.Generated;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlEnum;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlTransient;
import jakarta.xml.bind.annotation.XmlType;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import modules.admin.ImportExport.ImportExportExtension;
import org.skyve.CORE;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.types.Enumeration;
import org.skyve.impl.domain.AbstractPersistentBean;
import org.skyve.impl.domain.ChangeTrackingArrayList;
import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.util.Util;

/**
 * Import Export
 * 
 * @depend - - - Mode
 * @depend - - - RollbackErrors
 * @depend - - - LoadType
 * @navcomposed 1 importExportColumns 0..n ImportExportColumn
 * @stereotype "persistent"
 */
@XmlType
@XmlRootElement
@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
public abstract class ImportExport extends AbstractPersistentBean {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String MODULE_NAME = "admin";

	/** @hidden */
	public static final String DOCUMENT_NAME = "ImportExport";

	/** @hidden */
	public static final String modePropertyName = "mode";

	/** @hidden */
	public static final String moduleNamePropertyName = "moduleName";

	/** @hidden */
	public static final String documentNamePropertyName = "documentName";

	/** @hidden */
	public static final String importFileAbsolutePathPropertyName = "importFileAbsolutePath";

	/** @hidden */
	public static final String importFileNamePropertyName = "importFileName";

	/** @hidden */
	public static final String exportFileAbsolutePathPropertyName = "exportFileAbsolutePath";

	/** @hidden */
	public static final String resultsPropertyName = "results";

	/** @hidden */
	@Deprecated
	public static final String advancedModePropertyName = "advancedMode";

	/** @hidden */
	@Deprecated
	public static final String columnTitlesOnlyPropertyName = "columnTitlesOnly";

	/** @hidden */
	public static final String fileContainsHeadersPropertyName = "fileContainsHeaders";

	/** @hidden */
	public static final String detailedLoggingPropertyName = "detailedLogging";

	/** @hidden */
	public static final String importExportColumnsPropertyName = "importExportColumns";

	/** @hidden */
	public static final String rollbackErrorsPropertyName = "rollbackErrors";

	/** @hidden */
	public static final String loadTypePropertyName = "loadType";

	/**
	 * Mode
	 **/
	@XmlEnum
	@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
	public static enum Mode implements Enumeration {
		importData("importData", "Import Data"),
		exportData("exportData", "Export Data");

		private String code;
		private String description;

		/** @hidden */
		private DomainValue domainValue;

		/** @hidden */
		private static List<DomainValue> domainValues = Stream.of(values()).map(Mode::toDomainValue).collect(Collectors.toUnmodifiableList());

		private Mode(String code, String description) {
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

		public static Mode fromCode(String code) {
			Mode result = null;

			for (Mode value : values()) {
				if (value.code.equals(code)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static Mode fromLocalisedDescription(String description) {
			Mode result = null;

			for (Mode value : values()) {
				if (value.toLocalisedDescription().equals(description)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static List<DomainValue> toDomainValues() {
			return domainValues;
		}
	}

	/**
	 * Error handling
	 **/
	@XmlEnum
	@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
	public static enum RollbackErrors implements Enumeration {
		rollbackErrors("rollbackErrors", "admin.importExport.rollbackErrors.rollbackErrors.description"),
		noRollbackErrors("noRollbackErrors", "admin.importExport.rollbackErrors.noRollbackErrors.description");

		private String code;
		private String description;

		/** @hidden */
		private DomainValue domainValue;

		/** @hidden */
		private static List<DomainValue> domainValues = Stream.of(values()).map(RollbackErrors::toDomainValue).collect(Collectors.toUnmodifiableList());

		private RollbackErrors(String code, String description) {
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

		public static RollbackErrors fromCode(String code) {
			RollbackErrors result = null;

			for (RollbackErrors value : values()) {
				if (value.code.equals(code)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static RollbackErrors fromLocalisedDescription(String description) {
			RollbackErrors result = null;

			for (RollbackErrors value : values()) {
				if (value.toLocalisedDescription().equals(description)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static List<DomainValue> toDomainValues() {
			return domainValues;
		}
	}

	/**
	 * Loading strategy
	 * <br/>
	 * <p><b>Loading strategy</b></p>
<p><b>Create related records if they don't exist (recommended)</b>
<br/>
<i>With this option, uploaded records will be created but where these reference other records, they will only be created if a match can't be found</i></p>
<p><b>Create everything even if there might be duplicates</b>
<br/>
<i>With this option, new records will always be created</i></p>
	 **/
	@XmlEnum
	@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
	public static enum LoadType implements Enumeration {
		createFind("createFind", "admin.importExport.loadType.createFind.description"),
		createAll("createAll", "admin.importExport.loadType.createAll.description");

		private String code;
		private String description;

		/** @hidden */
		private DomainValue domainValue;

		/** @hidden */
		private static List<DomainValue> domainValues = Stream.of(values()).map(LoadType::toDomainValue).collect(Collectors.toUnmodifiableList());

		private LoadType(String code, String description) {
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

		public static LoadType fromCode(String code) {
			LoadType result = null;

			for (LoadType value : values()) {
				if (value.code.equals(code)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static LoadType fromLocalisedDescription(String description) {
			LoadType result = null;

			for (LoadType value : values()) {
				if (value.toLocalisedDescription().equals(description)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static List<DomainValue> toDomainValues() {
			return domainValues;
		}
	}

	/**
	 * Mode
	 **/
	private Mode mode = Mode.importData;

	/**
	 * Module Name
	 **/
	private String moduleName;

	/**
	 * Document
	 **/
	private String documentName;

	/**
	 * File Absolute Path
	 **/
	private String importFileAbsolutePath;

	/**
	 * Imported File
	 **/
	private String importFileName;

	/**
	 * File Absolute Path
	 **/
	private String exportFileAbsolutePath;

	/**
	 * Results
	 **/
	private String results;

	/**
	 * Advanced Mode
	 **/
	@Deprecated
	private Boolean advancedMode;

	/**
	 * Include Titles only
	 **/
	@Deprecated
	private Boolean columnTitlesOnly;

	/**
	 * Column Headers
	 **/
	private Boolean fileContainsHeaders = Boolean.valueOf(true);

	/**
	 * Detailed logging
	 * <br/>
	 * Include all import details row by row to server log
	 **/
	private Boolean detailedLogging = Boolean.valueOf(true);

	/**
	 * Columns
	 **/
	private List<ImportExportColumn> importExportColumns = new ChangeTrackingArrayList<>("importExportColumns", this);

	/**
	 * Error handling
	 **/
	private RollbackErrors rollbackErrors = RollbackErrors.rollbackErrors;

	/**
	 * Loading strategy
	 * <br/>
	 * <p><b>Loading strategy</b></p>
<p><b>Create related records if they don't exist (recommended)</b>
<br/>
<i>With this option, uploaded records will be created but where these reference other records, they will only be created if a match can't be found</i></p>
<p><b>Create everything even if there might be duplicates</b>
<br/>
<i>With this option, new records will always be created</i></p>
	 **/
	private LoadType loadType = LoadType.createFind;

	@Override
	@XmlTransient
	public String getBizModule() {
		return ImportExport.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return ImportExport.DOCUMENT_NAME;
	}

	public static ImportExportExtension newInstance() {
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
			return org.skyve.util.Binder.formatMessage("{moduleName}.{documentName}", this);
		}
		catch (@SuppressWarnings("unused") Exception e) {
			return "Unknown";
		}
	}

	@Override
	public boolean equals(Object o) {
		return ((o instanceof ImportExport) && 
					this.getBizId().equals(((ImportExport) o).getBizId()));
	}

	/**
	 * {@link #mode} accessor.
	 * @return	The value.
	 **/
	public Mode getMode() {
		return mode;
	}

	/**
	 * {@link #mode} mutator.
	 * @param mode	The new value.
	 **/
	@XmlElement
	public void setMode(Mode mode) {
		preset(modePropertyName, mode);
		this.mode = mode;
	}

	/**
	 * {@link #moduleName} accessor.
	 * @return	The value.
	 **/
	public String getModuleName() {
		return moduleName;
	}

	/**
	 * {@link #moduleName} mutator.
	 * @param moduleName	The new value.
	 **/
	@XmlElement
	public void setModuleName(String moduleName) {
		preset(moduleNamePropertyName, moduleName);
		this.moduleName = moduleName;
	}

	/**
	 * {@link #documentName} accessor.
	 * @return	The value.
	 **/
	public String getDocumentName() {
		return documentName;
	}

	/**
	 * {@link #documentName} mutator.
	 * @param documentName	The new value.
	 **/
	@XmlElement
	public void setDocumentName(String documentName) {
		preset(documentNamePropertyName, documentName);
		this.documentName = documentName;
	}

	/**
	 * {@link #importFileAbsolutePath} accessor.
	 * @return	The value.
	 **/
	public String getImportFileAbsolutePath() {
		return importFileAbsolutePath;
	}

	/**
	 * {@link #importFileAbsolutePath} mutator.
	 * @param importFileAbsolutePath	The new value.
	 **/
	@XmlElement
	public void setImportFileAbsolutePath(String importFileAbsolutePath) {
		preset(importFileAbsolutePathPropertyName, importFileAbsolutePath);
		this.importFileAbsolutePath = importFileAbsolutePath;
	}

	/**
	 * {@link #importFileName} accessor.
	 * @return	The value.
	 **/
	public String getImportFileName() {
		return importFileName;
	}

	/**
	 * {@link #importFileName} mutator.
	 * @param importFileName	The new value.
	 **/
	@XmlElement
	public void setImportFileName(String importFileName) {
		preset(importFileNamePropertyName, importFileName);
		this.importFileName = importFileName;
	}

	/**
	 * {@link #exportFileAbsolutePath} accessor.
	 * @return	The value.
	 **/
	public String getExportFileAbsolutePath() {
		return exportFileAbsolutePath;
	}

	/**
	 * {@link #exportFileAbsolutePath} mutator.
	 * @param exportFileAbsolutePath	The new value.
	 **/
	@XmlElement
	public void setExportFileAbsolutePath(String exportFileAbsolutePath) {
		preset(exportFileAbsolutePathPropertyName, exportFileAbsolutePath);
		this.exportFileAbsolutePath = exportFileAbsolutePath;
	}

	/**
	 * {@link #results} accessor.
	 * @return	The value.
	 **/
	public String getResults() {
		return results;
	}

	/**
	 * {@link #results} mutator.
	 * @param results	The new value.
	 **/
	@XmlElement
	public void setResults(String results) {
		this.results = results;
	}

	/**
	 * {@link #advancedMode} accessor.
	 * @return	The value.
	 **/
	@Deprecated
	public Boolean getAdvancedMode() {
		return advancedMode;
	}

	/**
	 * {@link #advancedMode} mutator.
	 * @param advancedMode	The new value.
	 **/
	@Deprecated
	@XmlElement
	public void setAdvancedMode(Boolean advancedMode) {
		preset(advancedModePropertyName, advancedMode);
		this.advancedMode = advancedMode;
	}

	/**
	 * {@link #columnTitlesOnly} accessor.
	 * @return	The value.
	 **/
	@Deprecated
	public Boolean getColumnTitlesOnly() {
		return columnTitlesOnly;
	}

	/**
	 * {@link #columnTitlesOnly} mutator.
	 * @param columnTitlesOnly	The new value.
	 **/
	@Deprecated
	@XmlElement
	public void setColumnTitlesOnly(Boolean columnTitlesOnly) {
		preset(columnTitlesOnlyPropertyName, columnTitlesOnly);
		this.columnTitlesOnly = columnTitlesOnly;
	}

	/**
	 * {@link #fileContainsHeaders} accessor.
	 * @return	The value.
	 **/
	public Boolean getFileContainsHeaders() {
		return fileContainsHeaders;
	}

	/**
	 * {@link #fileContainsHeaders} mutator.
	 * @param fileContainsHeaders	The new value.
	 **/
	@XmlElement
	public void setFileContainsHeaders(Boolean fileContainsHeaders) {
		preset(fileContainsHeadersPropertyName, fileContainsHeaders);
		this.fileContainsHeaders = fileContainsHeaders;
	}

	/**
	 * {@link #detailedLogging} accessor.
	 * @return	The value.
	 **/
	public Boolean getDetailedLogging() {
		return detailedLogging;
	}

	/**
	 * {@link #detailedLogging} mutator.
	 * @param detailedLogging	The new value.
	 **/
	@XmlElement
	public void setDetailedLogging(Boolean detailedLogging) {
		preset(detailedLoggingPropertyName, detailedLogging);
		this.detailedLogging = detailedLogging;
	}

	/**
	 * {@link #importExportColumns} accessor.
	 * @return	The value.
	 **/
	@XmlElement
	public List<ImportExportColumn> getImportExportColumns() {
		return importExportColumns;
	}

	/**
	 * {@link #importExportColumns} accessor.
	 * @param bizId	The bizId of the element in the list.
	 * @return	The value of the element in the list.
	 **/
	public ImportExportColumn getImportExportColumnsElementById(String bizId) {
		return getElementById(importExportColumns, bizId);
	}

	/**
	 * {@link #importExportColumns} mutator.
	 * @param bizId	The bizId of the element in the list.
	 * @param element	The new value of the element in the list.
	 **/
	public void setImportExportColumnsElementById(String bizId, ImportExportColumn element) {
		setElementById(importExportColumns, element);
	}

	/**
	 * {@link #importExportColumns} add.
	 * @param element	The element to add.
	 **/
	public boolean addImportExportColumnsElement(ImportExportColumn element) {
		boolean result = importExportColumns.add(element);
		if (result) {
			element.setParent((ImportExportExtension) this);
		}
		return result;
	}

	/**
	 * {@link #importExportColumns} add.
	 * @param index	The index in the list to add the element to.
	 * @param element	The element to add.
	 **/
	public void addImportExportColumnsElement(int index, ImportExportColumn element) {
		importExportColumns.add(index, element);
		element.setParent((ImportExportExtension) this);
	}

	/**
	 * {@link #importExportColumns} remove.
	 * @param element	The element to remove.
	 **/
	public boolean removeImportExportColumnsElement(ImportExportColumn element) {
		boolean result = importExportColumns.remove(element);
		if (result) {
			element.setParent(null);
		}
		return result;
	}

	/**
	 * {@link #importExportColumns} remove.
	 * @param index	The index in the list to remove the element from.
	 **/
	public ImportExportColumn removeImportExportColumnsElement(int index) {
		ImportExportColumn result = importExportColumns.remove(index);
		result.setParent(null);
		return result;
	}

	/**
	 * {@link #rollbackErrors} accessor.
	 * @return	The value.
	 **/
	public RollbackErrors getRollbackErrors() {
		return rollbackErrors;
	}

	/**
	 * {@link #rollbackErrors} mutator.
	 * @param rollbackErrors	The new value.
	 **/
	@XmlElement
	public void setRollbackErrors(RollbackErrors rollbackErrors) {
		preset(rollbackErrorsPropertyName, rollbackErrors);
		this.rollbackErrors = rollbackErrors;
	}

	/**
	 * {@link #loadType} accessor.
	 * @return	The value.
	 **/
	public LoadType getLoadType() {
		return loadType;
	}

	/**
	 * {@link #loadType} mutator.
	 * @param loadType	The new value.
	 **/
	@XmlElement
	public void setLoadType(LoadType loadType) {
		preset(loadTypePropertyName, loadType);
		this.loadType = loadType;
	}

	/**
	 * contextSet
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isContextSet() {
		return (getModuleName()!=null && getDocumentName()!=null);
	}

	/**
	 * {@link #isContextSet} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotContextSet() {
		return (! isContextSet());
	}

	/**
	 * Whether the upload file exists
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isFileExists() {
		return (Mode.importData.equals(mode) && importFileAbsolutePath!=null);
	}

	/**
	 * {@link #isFileExists} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotFileExists() {
		return (! isFileExists());
	}

	/**
	 * The load type  - whether to normalise the input or create all records
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isLoadTypeCreateFind() {
		return (!isShowExport()
				&& LoadType.createFind.equals(loadType));
	}

	/**
	 * {@link #isLoadTypeCreateFind} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotLoadTypeCreateFind() {
		return (! isLoadTypeCreateFind());
	}

	/**
	 * Whether to show the export mode view rather than the default import
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isShowExport() {
		return (Mode.exportData.equals(mode));
	}

	/**
	 * {@link #isShowExport} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotShowExport() {
		return (! isShowExport());
	}

	/**
	 * Whether to show advanced binding strings
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isShowExpressions() {
		return (((ImportExportExtension) this).anyColumnHasExpression());
	}

	/**
	 * {@link #isShowExpressions} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotShowExpressions() {
		return (! isShowExpressions());
	}

	/**
	 * showResults
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isShowResults() {
		return (results!=null);
	}

	/**
	 * {@link #isShowResults} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotShowResults() {
		return (! isShowResults());
	}

	/**
	 * Whether to show the upload button
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isShowUpload() {
		return (!isShowExport() && !isFileExists());
	}

	/**
	 * {@link #isShowUpload} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotShowUpload() {
		return (! isShowUpload());
	}

	/**
	 * Whether to show the upload columns
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isShowUploadColumns() {
		return (!isShowExport() && isFileExists());
	}

	/**
	 * {@link #isShowUploadColumns} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotShowUploadColumns() {
		return (! isShowUploadColumns());
	}
}
