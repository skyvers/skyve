package org.skyve.impl.metadata.repository.customer;

import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.customer.HTMLResources;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlType;

/**
 * JAXB-annotated descriptor for the {@code <htmlResources>} element in a
 * {@code customer.xml} file.
 *
 * <p>Specifies customer-supplied HTML resource paths (head content, body content)
 * that are injected into every page response for the tenant.
 *
 * <p>Threading: not thread-safe.  Read-only after JAXB unmarshalling.
 *
 * @see org.skyve.metadata.customer.HTMLResources
 */
@XmlType(namespace = XMLMetaData.CUSTOMER_NAMESPACE)
public class HTMLResourcesMetaData implements HTMLResources {
	private static final long serialVersionUID = 3982423990945642514L;

	private String cssRelativeFileName;
	
	@Override
	public String getCssRelativeFileName() {
		return cssRelativeFileName;
	}

	@XmlAttribute(name = "css")
	public void setCssRelativeFileName(String cssRelativeFileName) {
		this.cssRelativeFileName = UtilImpl.processStringValue(cssRelativeFileName);
	}
}
