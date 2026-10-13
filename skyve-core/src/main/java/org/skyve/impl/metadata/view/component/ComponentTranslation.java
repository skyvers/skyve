package org.skyve.impl.metadata.view.component;

import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.SerializableMetaData;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * JAXB-annotated mapping entry that renames an action or a condition of a
 * component view during view composition.
 *
 * <p>Declares that what the component view calls {@code fromComponent} is
 * {@code mappedTo} in the containing view, enabling component reuse across
 * documents that name things differently. Authored as an {@code <action/>}
 * within a component's {@code <actions>}, or a {@code <condition/>} within its
 * {@code <conditions>}. For a condition, either side may be an inline
 * condition expression as well as a condition name.
 *
 * <p>Threading: not thread-safe.  Read-only after JAXB unmarshalling.
 */
@XmlRootElement(namespace = XMLMetaData.VIEW_NAMESPACE)
@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE, propOrder = {"fromComponent", "mappedTo"})
public class ComponentTranslation implements SerializableMetaData {
	private static final long serialVersionUID = -1586157078585007880L;

	private String fromComponent;
	private String mappedTo;

	public String getFromComponent() {
		return fromComponent;
	}

	@XmlAttribute(required = true)
	public void setFromComponent(String fromComponent) {
		this.fromComponent = UtilImpl.processStringValue(fromComponent);
	}

	public String getMappedTo() {
		return mappedTo;
	}

	@XmlAttribute(required = true)
	public void setMappedTo(String mappedTo) {
		this.mappedTo = UtilImpl.processStringValue(mappedTo);
	}
}
