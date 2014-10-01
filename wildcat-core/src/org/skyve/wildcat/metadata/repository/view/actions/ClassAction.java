package org.skyve.wildcat.metadata.repository.view.actions;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlType;

import org.skyve.wildcat.metadata.view.ActionImpl;
import org.skyve.wildcat.util.UtilImpl;
import org.skyve.wildcat.util.XMLUtil;

@XmlType(namespace = XMLUtil.VIEW_NAMESPACE)
public abstract class ClassAction extends PositionableAction {
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
