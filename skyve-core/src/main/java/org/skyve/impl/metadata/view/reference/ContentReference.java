package org.skyve.impl.metadata.view.reference;

import org.skyve.impl.metadata.view.widget.bound.AbstractBound;
import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * This reference generates a href to the content specified by the binding.
 * @author mike
 *
 */
@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.VIEW_NAMESPACE)
public class ContentReference extends AbstractBound implements Reference {
	private static final long serialVersionUID = -3654900648809959380L;
}
