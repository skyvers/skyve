package org.skyve.wildcat.metadata.view.event;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.metadata.view.Disableable;
import org.skyve.wildcat.bind.BindUtil;
import org.skyve.wildcat.metadata.view.widget.bound.AbstractBound;
import org.skyve.wildcat.util.UtilImpl;
import org.skyve.wildcat.util.XMLUtil;

@XmlType(namespace = XMLUtil.VIEW_NAMESPACE)
@XmlRootElement(namespace = XMLUtil.VIEW_NAMESPACE, name = "setDisabled")
public class SetDisabledEventAction extends AbstractBound implements EventAction, Disableable {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = 749553391893947191L;

	private String disabledConditionName;

	@Override
	public String getDisabledConditionName() {
		return disabledConditionName;
	}

	@Override
	@XmlAttribute(name = "disabled")
	public void setDisabledConditionName(String disabledConditionName) {
		this.disabledConditionName = UtilImpl.processStringValue(disabledConditionName);
	}

	@Override
	@XmlAttribute(name = "enabled")
	public void setEnabledConditionName(String enabledConditionName) {
		this.disabledConditionName = BindUtil.negateCondition(UtilImpl.processStringValue(enabledConditionName));
	}
}
