package org.skyve.impl.metadata.repository.module;

import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.SerializableMetaData;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlType;

/**
 * JAXB-annotated record identifying the UX/UI scope to which a view element
 * or menu item applies.
 *
 * <p>Carries a UX/UI name (e.g. {@code desktop}, {@code phone}) so that
 * conditional rendering or menu filtering can restrict content to the
 * appropriate device or theme context.
 *
 * <p>Threading: not thread-safe.  Read-only after JAXB unmarshalling.
 */
@XmlType(namespace = XMLMetaData.MODULE_NAMESPACE)
public class ApplicableTo implements SerializableMetaData {
	private static final long serialVersionUID = 8204068663179740572L;

	private String uxui;

	public String getUxUi() {
		return uxui;
	}

	@XmlAttribute(name = "name", required = true)
	public void setUxUi(String uxui) {
		this.uxui = UtilImpl.processStringValue(uxui);
	}
}
