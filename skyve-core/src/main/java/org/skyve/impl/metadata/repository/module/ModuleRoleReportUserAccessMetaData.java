package org.skyve.impl.metadata.repository.module;

import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.UserAccess;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * JAXB-annotated descriptor for a {@code <reportAccess>} user-access grant in
 * a module role.
 *
 * <p>Grants access to run a named report on a module resource.
 * Extends {@link ModuleRoleUserAccessMetaData}.
 *
 * <p>Threading: not thread-safe.  Read-only after JAXB unmarshalling.
 *
 * @see ModuleRoleUserAccessMetaData
 */
@XmlType(namespace = XMLMetaData.MODULE_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.MODULE_NAMESPACE, name = "reportAccess")
public class ModuleRoleReportUserAccessMetaData extends ModuleRoleUserAccessMetaData {
	private static final long serialVersionUID = 9077804784844042341L;

	private String moduleName;
	private String documentName;
	private String reportName;

	/**
	 * Returns the target module name for this report-access grant.
	 *
	 * @return the target module name
	 */
	public String getModuleName() {
		return moduleName;
	}

	/**
	 * Sets the target module name for this report-access grant.
	 *
	 * <p>Side effects: normalises the supplied value with
	 * {@link UtilImpl#processStringValue(String)} before storing it.
	 *
	 * @param moduleName the target module name; blank values become {@code null}
	 */
	@XmlAttribute(name = "module", required = true)
	public void setModuleName(String moduleName) {
		this.moduleName = UtilImpl.processStringValue(moduleName);
	}

	/**
	 * Returns the target document name for this report-access grant.
	 *
	 * @return the target document name
	 */
	public String getDocumentName() {
		return documentName;
	}

	/**
	 * Sets the target document name for this report-access grant.
	 *
	 * <p>Side effects: normalises the supplied value with
	 * {@link UtilImpl#processStringValue(String)} before storing it.
	 *
	 * @param documentName the target document name; blank values become {@code null}
	 */
	@XmlAttribute(name = "document", required = true)
	public void setDocumentName(String documentName) {
		this.documentName = UtilImpl.processStringValue(documentName);
	}

	/**
	 * Returns the target report name for this report-access grant.
	 *
	 * @return the target report name
	 */
	public String getReportName() {
		return reportName;
	}

	/**
	 * Sets the target report name for this report-access grant.
	 *
	 * <p>Side effects: normalises the supplied value with
	 * {@link UtilImpl#processStringValue(String)} before storing it.
	 *
	 * @param reportName the target report name; blank values become {@code null}
	 */
	@XmlAttribute(name = "report", required = true)
	public void setReportName(String reportName) {
		this.reportName = UtilImpl.processStringValue(reportName);
	}
	
	/**
	 * Validates that all report-access references are present.
	 *
	 * @param metaDataName the source metadata identifier used in validation errors
	 * @param roleName the owning role name used in validation errors
	 * @param module unused for first-pass validation in this type
	 * @throws MetaDataException if {@code module}, {@code document}, or {@code report} is missing
	 */
	@Override
	public void validate(String metaDataName, String roleName, Module module) {
		if (moduleName == null) {
			throw new MetaDataException(metaDataName + " : [module] is required for all report user accesses defined in module role " + roleName);
		}
		if (documentName == null) {
			throw new MetaDataException(metaDataName + " : [document] is required for all report user accesses defined in module role " + roleName);
		}
		if (reportName == null) {
			throw new MetaDataException(metaDataName + " : [report] is required for all report user accesses defined in module role " + roleName);
		}
		// NB can't validate moduleName, documentName or reportName exists until second pass validation in LocalDesignRepository.validateModuleForGenerateDomain()
	}
	
	/**
	 * Creates a report user access descriptor for the configured module/document/report triple.
	 *
	 * @param unusedModuleName ignored because this access stores its own target module
	 * @return the runtime report user access
	 */
	@Override
	public UserAccess toUserAccess(String unusedModuleName) {
		return UserAccess.report(moduleName, documentName, reportName);
	}
}
