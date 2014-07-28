package org.skyve.wildcat.metadata.view.event;

import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.wildcat.metadata.view.widget.bound.AbstractBound;
import org.skyve.wildcat.util.XMLUtil;

@XmlType(namespace = XMLUtil.VIEW_NAMESPACE)
@XmlRootElement(namespace = XMLUtil.VIEW_NAMESPACE, name = "toggleDisabled")
public class ToggleDisabledEventAction extends AbstractBound implements EventAction {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = -89146740336444176L;
}
