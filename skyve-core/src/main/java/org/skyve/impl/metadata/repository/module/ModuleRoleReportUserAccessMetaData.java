package org.skyve.impl.metadata.repository.module;

import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.UserAccess;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

@XmlType(namespace = XMLMetaData.MODULE_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.MODULE_NAMESPACE, name = "reportAccess")
public class ModuleRoleReportUserAccessMetaData extends ModuleRoleUserAccessMetaData {
	private static final long serialVersionUID = 9077804784844042341L;

	private String moduleName;
	private String documentName;
	private String reportName;

	public String getModuleName() {
		return moduleName;
	}

	@XmlAttribute(name = "module", required = true)
	public void setModuleName(String moduleName) {
		this.moduleName = UtilImpl.processStringValue(moduleName);
	}

	public String getDocumentName() {
		return documentName;
	}

	@XmlAttribute(name = "document", required = true)
	public void setDocumentName(String documentName) {
		this.documentName = UtilImpl.processStringValue(documentName);
	}

	public String getReportName() {
		return reportName;
	}

	@XmlAttribute(name = "report", required = true)
	public void setReportName(String reportName) {
		this.reportName = UtilImpl.processStringValue(reportName);
	}
	
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
	
	@Override
	public UserAccess toUserAccess(String unusedModuleName) {
		return UserAccess.report(moduleName, documentName, reportName);
	}
}
