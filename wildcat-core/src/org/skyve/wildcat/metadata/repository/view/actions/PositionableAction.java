package org.skyve.wildcat.metadata.repository.view.actions;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlType;

import org.skyve.wildcat.util.XMLUtil;

@XmlType(namespace = XMLUtil.VIEW_NAMESPACE)
public abstract class PositionableAction extends Action {
	private Boolean inActionPanel;

	public Boolean isInActionPanel() {
		return inActionPanel;
	}
	
	@XmlAttribute(required = false)
	public void setInActionPanel(Boolean inActionPanel) {
		this.inActionPanel = inActionPanel;
	}

	@Override
	public org.skyve.wildcat.metadata.view.Action toMetaDataAction() {
		org.skyve.wildcat.metadata.view.Action result = super.toMetaDataAction();
		result.setInActionPanel(inActionPanel);
		return result;
	}
}
