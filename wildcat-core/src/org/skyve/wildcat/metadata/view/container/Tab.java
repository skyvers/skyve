package org.skyve.wildcat.metadata.view.container;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.metadata.view.Disableable;
import org.skyve.metadata.view.Invisible;
import org.skyve.wildcat.metadata.Container;
import org.skyve.wildcat.util.UtilImpl;
import org.skyve.wildcat.util.XMLUtil;

@XmlRootElement(namespace = XMLUtil.VIEW_NAMESPACE)
@XmlType(namespace = XMLUtil.VIEW_NAMESPACE, 
			propOrder = {"title", "icon16x16RelativeFileName", "disabledConditionName", "invisibleConditionName", "selectedConditionName"})
public final class Tab extends Container implements Disableable, Invisible {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = -3216551162394859248L;

	private String title;
	private String icon16x16RelativeFileName;
	private String disabledConditionName;
	private String invisibleConditionName;
	private String selectedConditionName;
	
	public String getTitle() {
		return title;
	}

	@XmlAttribute(required = true)
	public void setTitle(String title) {
		this.title = UtilImpl.processStringValue(title);
	}

	public String getIcon16x16RelativeFileName() {
		return icon16x16RelativeFileName;
	}

	@XmlAttribute(name = "icon16x16RelativeFileName")
	public void setIcon16x16RelativeFileName(String icon16x16RelativeFileName) {
		this.icon16x16RelativeFileName = UtilImpl.processStringValue(icon16x16RelativeFileName);
	}

	@Override
	public String getDisabledConditionName() {
		return disabledConditionName;
	}

	@Override
	@XmlAttribute(name = "disabled", required = false)
	public void setDisabledConditionName(String disabledConditionName) {
		this.disabledConditionName = disabledConditionName;
	}

	@Override
	public String getInvisibleConditionName() {
		return invisibleConditionName;
	}

	@Override
	@XmlAttribute(name = "invisible", required = false)
	public void setInvisibleConditionName(String invisibleConditionName) {
		this.invisibleConditionName = invisibleConditionName;
	}

	public String getSelectedConditionName() {
		return selectedConditionName;
	}

	@XmlAttribute(name = "selected", required = false)
	public void setSelectedConditionName(String selectedConditionName) {
		this.selectedConditionName = selectedConditionName;
	}
}
