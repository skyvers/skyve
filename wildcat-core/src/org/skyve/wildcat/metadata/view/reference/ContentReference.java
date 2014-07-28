package org.skyve.wildcat.metadata.view.reference;

import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.wildcat.metadata.view.widget.bound.AbstractBound;
import org.skyve.wildcat.util.XMLUtil;

/**
 * This reference generates a href to the content specified by the binding.
 * @author mike
 *
 */
@XmlType(namespace = XMLUtil.VIEW_NAMESPACE)
@XmlRootElement(namespace = XMLUtil.VIEW_NAMESPACE)
public class ContentReference extends AbstractBound implements Reference {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = -3654900648809959380L;
}
