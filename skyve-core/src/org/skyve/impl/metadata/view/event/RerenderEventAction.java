package org.skyve.impl.metadata.view.event;

import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.util.XMLUtil;
import org.skyve.impl.metadata.view.event.EventAction;

@XmlType(namespace = XMLUtil.VIEW_NAMESPACE)
@XmlRootElement(namespace = XMLUtil.VIEW_NAMESPACE, name = "rerender")
public class RerenderEventAction implements EventAction {
	// no attributes
}
