package org.skyve.impl.metadata.view.widget.bound.input;

import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.metadata.view.widget.bound.AbstractBound;
import org.skyve.impl.util.XMLUtil;

@XmlType(namespace = XMLUtil.VIEW_NAMESPACE)
@XmlRootElement(namespace = XMLUtil.VIEW_NAMESPACE, name = "default")
public class DefaultWidget extends AbstractBound {
	/**
	 * For Serialization.
	 */
	private static final long serialVersionUID = 1007637557499176432L;

	// no properties to add
}
