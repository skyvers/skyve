package org.skyve.wildcat.metadata.view.widget.bound.input;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlType;

import org.skyve.metadata.view.Disableable;
import org.skyve.metadata.view.Invisible;
import org.skyve.wildcat.bind.BindUtil;
import org.skyve.wildcat.metadata.view.widget.bound.AbstractBound;
import org.skyve.wildcat.util.UtilImpl;
import org.skyve.wildcat.util.XMLUtil;

@XmlType(namespace = XMLUtil.VIEW_NAMESPACE,
			propOrder = {"invisibleConditionName", "visibleConditionName", "disabledConditionName", "enabledConditionName"})
public abstract class InputWidget extends AbstractBound implements Disableable, Invisible {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = -5332816537102477174L;

	private String disabledConditionName;
	private String invisibleConditionName;

	@Override
	public String getDisabledConditionName() {
		return disabledConditionName;
	}

	@Override
	@XmlAttribute(name = "disabled", required = false)
	public void setDisabledConditionName(String disabledConditionName) {
		this.disabledConditionName = UtilImpl.processStringValue(disabledConditionName);
	}

	// to enable JAXB XML marshaling
	@SuppressWarnings("static-method")
	String getEnabledConditionName() {
		return null;
	}

	@Override
	@XmlAttribute(name = "enabled", required = false)
	public void setEnabledConditionName(String enabledConditionName) {
		this.disabledConditionName = BindUtil.negateCondition(UtilImpl.processStringValue(enabledConditionName));
	}

	@Override
	public String getInvisibleConditionName() {
		return invisibleConditionName;
	}

	@Override
	@XmlAttribute(name = "invisible", required = false)
	public void setInvisibleConditionName(String invisibleConditionName) {
		this.invisibleConditionName = UtilImpl.processStringValue(invisibleConditionName);
	}

	// to enable JAXB XML marshaling
	@SuppressWarnings("static-method")
	String getVisibleConditionName() {
		return null;
	}

	@Override
	@XmlAttribute(name = "visible", required = false)
	public void setVisibleConditionName(String visibleConditionName) {
		this.invisibleConditionName = BindUtil.negateCondition(UtilImpl.processStringValue(visibleConditionName));
	}
}
