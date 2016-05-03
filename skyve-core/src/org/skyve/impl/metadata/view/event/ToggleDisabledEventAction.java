package org.skyve.impl.metadata.view.event;

import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.metadata.view.widget.bound.AbstractBound;
import org.skyve.impl.util.XMLUtil;
import org.skyve.impl.metadata.view.event.EventAction;

@XmlType(namespace = XMLUtil.VIEW_NAMESPACE)
@XmlRootElement(namespace = XMLUtil.VIEW_NAMESPACE, name = "toggleDisabled")
public class ToggleDisabledEventAction extends AbstractBound implements EventAction {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = -89146740336444176L;
}
