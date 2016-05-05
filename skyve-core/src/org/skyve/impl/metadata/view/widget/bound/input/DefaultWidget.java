package org.skyve.impl.metadata.view.widget.bound.input;

import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.metadata.view.widget.bound.AbstractBound;
import org.skyve.impl.util.XMLMetaData;

@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.VIEW_NAMESPACE, name = "default")
public class DefaultWidget extends AbstractBound {
	/**
	 * For Serialization.
	 */
	private static final long serialVersionUID = 1007637557499176432L;

	// no properties to add
}
