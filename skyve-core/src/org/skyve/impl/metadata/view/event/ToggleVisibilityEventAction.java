package org.skyve.impl.metadata.view.event;

import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.metadata.view.widget.bound.AbstractBound;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.impl.metadata.view.event.EventAction;

@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.VIEW_NAMESPACE, name = "toggleVisibility")
public class ToggleVisibilityEventAction extends AbstractBound implements EventAction {
	private static final long serialVersionUID = 7698439970713412634L;
}
