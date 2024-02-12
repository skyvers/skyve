package org.skyve.impl.metadata.repository.view.actions;

import org.skyve.impl.metadata.view.ActionImpl;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlType;

@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE)
public abstract class ClassAction extends ValidatableAction {
	private static final long serialVersionUID = -2913422200090616971L;

	private String className;

	public String getClassName() {
		return className;
	}
	
	@XmlAttribute(required = false)
	public void setClassName(String className) {
		this.className = UtilImpl.processStringValue(className);
	}

	@Override
	public ActionImpl toMetaDataAction() {
		ActionImpl result = super.toMetaDataAction();
		
		result.setResourceName(className);
		if (getName() == null) {
			result.setName(className);
		}
		
		return result;
	}
}
