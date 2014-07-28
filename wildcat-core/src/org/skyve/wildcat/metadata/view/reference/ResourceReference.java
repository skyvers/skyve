package org.skyve.wildcat.metadata.view.reference;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.wildcat.util.XMLUtil;

/**
 * This reference generates a href to the customer resource based on the relative file name.
 * @author mike
 *
 */
@XmlType(namespace = XMLUtil.VIEW_NAMESPACE)
@XmlRootElement(namespace = XMLUtil.VIEW_NAMESPACE)
public class ResourceReference implements Reference {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = 1726752281166975694L;

	private String relativeFile;

	public String getRelativeFile() {
		return relativeFile;
	}

	@XmlAttribute(required = true)
	public void setRelativeFile(String relativeFile) {
		this.relativeFile = relativeFile;
	}
}
