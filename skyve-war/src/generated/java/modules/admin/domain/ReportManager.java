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
import modules.admin.ReportManager.ReportManagerExtension;
import modules.admin.ReportTemplate.ReportTemplateExtension;
import org.skyve.CORE;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.types.Enumeration;
import org.skyve.impl.domain.AbstractTransientBean;
import org.skyve.impl.domain.ChangeTrackingArrayList;
import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.util.Util;

/**
 * Report Manager
 * <br/>
 * <p>The Report Manager document provides functions to manage system Report Templates and Report Configurations.</p>
 * 
 * @depend - - - ImportActionType
 * @navhas n currentReports 0..n ReportTemplate
 * @stereotype "transient"
 */
@XmlType
@XmlRootElement
@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
public abstract class ReportManager extends AbstractTransientBean {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String MODULE_NAME = "admin";

	/** @hidden */
	public static final String DOCUMENT_NAME = "ReportManager";

	/** @hidden */
	public static final String pathToZipPropertyName = "pathToZip";

	/** @hidden */
	public static final String importActionTypePropertyName = "importActionType";

	/** @hidden */
	public static final String currentReportsPropertyName = "currentReports";

	/**
	 * Import action
	 **/
	@XmlEnum
	@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
	public static enum ImportActionType implements Enumeration {
		validateOnlyReportConfigurationsAndTemplates("validate", "Validate only report configurations and templates"),
		validateThenImportReportConfigurationsAndTemplates("import", "Validate then import report configurations and templates");

		private String code;
		private String description;

		/** @hidden */
		private DomainValue domainValue;

		/** @hidden */
		private static List<DomainValue> domainValues = Stream.of(values()).map(ImportActionType::toDomainValue).collect(Collectors.toUnmodifiableList());

		private ImportActionType(String code, String description) {
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

		public static ImportActionType fromCode(String code) {
			ImportActionType result = null;

			for (ImportActionType value : values()) {
				if (value.code.equals(code)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static ImportActionType fromLocalisedDescription(String description) {
			ImportActionType result = null;

			for (ImportActionType value : values()) {
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
	 * Path to Zip
	 * <br/>
	 * The path of the zip of report configurations for download
	 **/
	private String pathToZip;

	/**
	 * Import action
	 **/
	private ImportActionType importActionType = ImportActionType.validateOnlyReportConfigurationsAndTemplates;

	/**
	 * Current Reports
	 **/
	private List<ReportTemplateExtension> currentReports = new ChangeTrackingArrayList<>("currentReports", this);

	@Override
	@XmlTransient
	public String getBizModule() {
		return ReportManager.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return ReportManager.DOCUMENT_NAME;
	}

	public static ReportManagerExtension newInstance() {
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
		return toString();

	}

	@Override
	public boolean equals(Object o) {
		return ((o instanceof ReportManager) && 
					this.getBizId().equals(((ReportManager) o).getBizId()));
	}

	/**
	 * {@link #pathToZip} accessor.
	 * @return	The value.
	 **/
	public String getPathToZip() {
		return pathToZip;
	}

	/**
	 * {@link #pathToZip} mutator.
	 * @param pathToZip	The new value.
	 **/
	@XmlElement
	public void setPathToZip(String pathToZip) {
		preset(pathToZipPropertyName, pathToZip);
		this.pathToZip = pathToZip;
	}

	/**
	 * {@link #importActionType} accessor.
	 * @return	The value.
	 **/
	public ImportActionType getImportActionType() {
		return importActionType;
	}

	/**
	 * {@link #importActionType} mutator.
	 * @param importActionType	The new value.
	 **/
	@XmlElement
	public void setImportActionType(ImportActionType importActionType) {
		preset(importActionTypePropertyName, importActionType);
		this.importActionType = importActionType;
	}

	/**
	 * {@link #currentReports} accessor.
	 * @return	The value.
	 **/
	@XmlElement
	public List<ReportTemplateExtension> getCurrentReports() {
		return currentReports;
	}

	/**
	 * {@link #currentReports} accessor.
	 * @param bizId	The bizId of the element in the list.
	 * @return	The value of the element in the list.
	 **/
	public ReportTemplateExtension getCurrentReportsElementById(String bizId) {
		return getElementById(currentReports, bizId);
	}

	/**
	 * {@link #currentReports} mutator.
	 * @param bizId	The bizId of the element in the list.
	 * @param element	The new value of the element in the list.
	 **/
	public void setCurrentReportsElementById(String bizId, ReportTemplateExtension element) {
		setElementById(currentReports, element);
	}

	/**
	 * {@link #currentReports} add.
	 * @param element	The element to add.
	 **/
	public boolean addCurrentReportsElement(ReportTemplateExtension element) {
		return currentReports.add(element);
	}

	/**
	 * {@link #currentReports} add.
	 * @param index	The index in the list to add the element to.
	 * @param element	The element to add.
	 **/
	public void addCurrentReportsElement(int index, ReportTemplateExtension element) {
		currentReports.add(index, element);
	}

	/**
	 * {@link #currentReports} remove.
	 * @param element	The element to remove.
	 **/
	public boolean removeCurrentReportsElement(ReportTemplateExtension element) {
		return currentReports.remove(element);
	}

	/**
	 * {@link #currentReports} remove.
	 * @param index	The index in the list to remove the element from.
	 **/
	public ReportTemplateExtension removeCurrentReportsElement(int index) {
		return currentReports.remove(index);
	}
}
