package org.skyve.impl.metadata.repository.customer;

import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlType;

/**
 * JAXB-annotated descriptor for the {@code <uiResources>} element in a
 * {@code customer.xml} file.
 *
 * <p>Specifies customer-specific UI resource paths (theme, icon set) that override
 * the framework defaults for all views rendered for this tenant.
 *
 * <p>Threading: not thread-safe.  Read-only after JAXB unmarshalling.
 *
 * @see org.skyve.metadata.customer.UIResources
 */
@XmlType(namespace = XMLMetaData.CUSTOMER_NAMESPACE)
public class UIResources implements org.skyve.metadata.customer.UIResources {
	private static final long serialVersionUID = 7253552655682697331L;

	private String logoRelativeFileName;
	
	@Override
	public String getLogoRelativeFileName() {
		return logoRelativeFileName;
	}

	@XmlAttribute(name = "logo")
	public void setLogoRelativeFileName(String logoRelativeFileName) {
		this.logoRelativeFileName = UtilImpl.processStringValue(logoRelativeFileName);
	}
}
