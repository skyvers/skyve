package org.skyve.impl.metadata.view.reference;

import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * This reference generates a href to the customer resource based on the relative file name.
 * @author mike
 *
 */
@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.VIEW_NAMESPACE)
public class ResourceReference implements Reference {
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
