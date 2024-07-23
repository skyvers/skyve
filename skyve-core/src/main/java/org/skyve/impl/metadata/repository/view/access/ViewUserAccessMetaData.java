package org.skyve.impl.metadata.repository.view.access;

import java.util.ArrayList;
import java.util.List;

import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.SerializableMetaData;
import org.skyve.metadata.user.UserAccess;

import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlType;

@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE)
public abstract class ViewUserAccessMetaData implements SerializableMetaData {
	private static final long serialVersionUID = -1548088130239406885L;

	private List<ViewUserAccessUxUiMetadata> uxuis = new ArrayList<>();

	@XmlElement(namespace = XMLMetaData.VIEW_NAMESPACE, name = "uxui")
	public List<ViewUserAccessUxUiMetadata> getUxuis() {
		return uxuis;
	}
	
	public abstract void validate(String metaDataName, Module module);
	public abstract UserAccess toUserAccess(String moduleName, String documentName);
}
