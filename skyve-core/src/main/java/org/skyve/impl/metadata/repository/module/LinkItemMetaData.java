package org.skyve.impl.metadata.repository.module;

import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * JAXB-annotated descriptor for a {@code <link>} menu item in a module menu.
 *
 * <p>Provides a navigation link to an arbitrary URL, a named action, or another
 * Skyve route.  Extends {@link ItemMetaData}.
 *
 * <p>Threading: not thread-safe.  Read-only after JAXB unmarshalling.
 *
 * @see ItemMetaData
 */
@XmlType(name = "link", namespace = XMLMetaData.MODULE_NAMESPACE)
@XmlRootElement(name = "link", namespace = XMLMetaData.MODULE_NAMESPACE)
public class LinkItemMetaData extends ItemMetaData {
	private static final long serialVersionUID = 4058261818028726112L;

	private String href;

	public String getHref() {
		return href;
	}
	@XmlAttribute(name = "href", required = true)
	public void setHref(String href) {
		this.href = UtilImpl.processStringValue(href);
	}
}
