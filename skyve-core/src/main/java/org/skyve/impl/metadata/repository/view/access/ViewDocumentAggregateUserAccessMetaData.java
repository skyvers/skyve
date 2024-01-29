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
@XmlRootElement(namespace = XMLMetaData.VIEW_NAMESPACE, name = "documentAggregate")
public class ViewDocumentAggregateUserAccessMetaData extends ViewUserAccessMetaData {
	private static final long serialVersionUID = 2238960331586617850L;

	private String documentName;

	public String getDocumentName() {
		return documentName;
	}

	@XmlAttribute(name = "document", required = true)
	public void setDocumentName(String documentName) {
		this.documentName = UtilImpl.processStringValue(documentName);
	}
	
	@Override
	public void validate(String metaDataName, Module module) {
		if (documentName == null) {
			throw new MetaDataException(metaDataName + " : [documentName] is required for all documentAggregate, modelAggregate & singular user accesses.");
		}
		if (! module.getDocumentRefs().keySet().contains(documentName)) {
			String moduleName = module.getName();
			throw new MetaDataException(metaDataName + " : [documentName] " + documentName + " does not exist for user access " + toUserAccess(moduleName, documentName).toString() + " in module " + moduleName);
		}
	}

	@Override
	public UserAccess toUserAccess(String moduleName, String documentNameParameter) {
		return UserAccess.documentAggregate(moduleName, documentNameParameter);
	}
}
