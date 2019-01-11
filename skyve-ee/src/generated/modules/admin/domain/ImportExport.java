package modules.admin.domain;

import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlEnum;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.XmlType;
import modules.admin.ImportExport.ImportExportExtension;
import org.skyve.CORE;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.types.Enumeration;
import org.skyve.impl.domain.AbstractPersistentBean;
import org.skyve.metadata.model.document.Bizlet.DomainValue;

/**
 * Import Export
 * 
 * @depend - - - Mode
 * @navcomposed 1 importExportColumns 0..n ImportExportColumn
 * @stereotype "persistent"
 */
@XmlType
@XmlRootElement
public class ImportExport extends AbstractPersistentBean {
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
	public static final String advancedModePropertyName = "advancedMode";
	/** @hidden */
	public static final String fileContainsHeadersPropertyName = "fileContainsHeaders";
	/** @hidden */
	public static final String importExportColumnsPropertyName = "importExportColumns";

	/**
	 * Mode
	 **/
	@XmlEnum
	public static enum Mode implements Enumeration {
		importData("importData", "Import Data"),
		exportData("exportData", "Export Data");

		private String code;
		private String description;

		/** @hidden */
		private DomainValue domainValue;

		/** @hidden */
		private static List<DomainValue> domainValues;

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
		public String toDescription() {
			return description;
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

		public static Mode fromDescription(String description) {
			Mode result = null;

			for (Mode value : values()) {
				if (value.description.equals(description)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static List<DomainValue> toDomainValues() {
			if (domainValues == null) {
				Mode[] values = values();
				domainValues = new ArrayList<>(values.length);
				for (Mode value : values) {
					domainValues.add(value.domainValue);
				}
			}

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
	 * Advanced
	 **/
	private Boolean advancedMode;
	/**
	 * Column Headers
	 **/
	private Boolean fileContainsHeaders = new Boolean(true);
	/**
	 * Columns
	 **/
	private List<ImportExportColumn> importExportColumns = new ArrayList<>();

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
			return org.skyve.util.Binder.formatMessage(org.skyve.CORE.getUser().getCustomer(),
														"{moduleName}.{documentName}",
														this);
		}
		catch (Exception e) {
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
	public Boolean getAdvancedMode() {
		return advancedMode;
	}

	/**
	 * {@link #advancedMode} mutator.
	 * @param advancedMode	The new value.
	 **/
	@XmlElement
	public void setAdvancedMode(Boolean advancedMode) {
		preset(advancedModePropertyName, advancedMode);
		this.advancedMode = advancedMode;
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
	 * Whether to show advanced binding strings
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isShowAdvancedBindings() {
		return (((ImportExportExtension) this).anyColumnHasExpression());
	}

	/**
	 * {@link #isShowAdvancedBindings} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotShowAdvancedBindings() {
		return (! isShowAdvancedBindings());
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
}
