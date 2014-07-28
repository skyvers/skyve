package org.skyve.wildcat.metadata.view.reference;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.wildcat.util.UtilImpl;
import org.skyve.wildcat.util.XMLUtil;

/**
 * Specifies the modal window or frame target behaviour for opening a reference.
 * 
 * @author mike
 */
@XmlType(namespace = XMLUtil.VIEW_NAMESPACE)
public class ReferenceTarget {
	@XmlType(namespace = XMLUtil.VIEW_NAMESPACE)
	public enum ReferenceTargetType {
		self,
		blankFrame,
		namedFame,
		modalWindow,
		modalWindowWithEmbeddedFrame
	}
	
	private ReferenceTargetType type;
	private String name;

	public ReferenceTargetType getType() {
		return type;
	}
	
	@XmlAttribute(required = true)
	public void setType(ReferenceTargetType type) {
		this.type = type;
	}

	public String getName() {
		return name;
	}
	@XmlElement(namespace = XMLUtil.VIEW_NAMESPACE)
	public void setName(String name) {
		this.name = UtilImpl.processStringValue(name);
	}
}
