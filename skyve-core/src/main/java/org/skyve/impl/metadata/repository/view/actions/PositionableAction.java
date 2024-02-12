package org.skyve.impl.metadata.repository.view.actions;

import org.skyve.impl.metadata.view.ActionImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.view.Action.ActionShow;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlType;

@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE)
public abstract class PositionableAction extends ActionMetaData {
	private static final long serialVersionUID = -7322904477575844567L;

	private Boolean inActionPanel;
	private ActionShow show;

	public Boolean getInActionPanel() {
		return inActionPanel;
	}
	
	@XmlAttribute(required = false)
	public void setInActionPanel(Boolean inActionPanel) {
		this.inActionPanel = inActionPanel;
	}

	public ActionShow getShow() {
		return show;
	}
	
	@XmlAttribute(required = false)
	public void setShow(ActionShow show) {
		this.show = show;
	}

	@Override
	public ActionImpl toMetaDataAction() {
		ActionImpl result = super.toMetaDataAction();
		result.setInActionPanel(inActionPanel);
		result.setShow(show);
		return result;
	}
}
