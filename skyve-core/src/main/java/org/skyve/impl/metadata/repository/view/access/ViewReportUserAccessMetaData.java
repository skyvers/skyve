package org.skyve.impl.metadata.repository.view.access;

import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.UserAccess;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.VIEW_NAMESPACE, name = "reportAccess")
public class ViewReportUserAccessMetaData extends ViewUserAccessMetaData {
	private static final long serialVersionUID = -7193764223948804934L;

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
	public void validate(String metaDataName, Module module) {
		if (moduleName == null) {
			throw new MetaDataException(metaDataName + " : [module] is required for all report user accesses");
		}
		if (documentName == null) {
			throw new MetaDataException(metaDataName + " : [document] is required for all report user accesses");
		}
		if (reportName == null) {
			throw new MetaDataException(metaDataName + " : [report] is required for all report user accesses");
		}
		// NB can't validate moduleName, documentName or reportName until second pass validation in LocalDesignRepository.validateViewForGenerateDomain()
	}
	
	@Override
	public UserAccess toUserAccess(String unusedModuleName, String unusedDocumentName) {
		return UserAccess.modelAggregate(moduleName, documentName, reportName);
	}
}
