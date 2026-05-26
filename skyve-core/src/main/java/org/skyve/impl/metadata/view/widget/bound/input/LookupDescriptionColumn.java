package org.skyve.impl.metadata.view.widget.bound.input;

import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.NamedMetaData;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;
import jakarta.xml.bind.annotation.XmlValue;

@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE)
/**
 * JAXB-annotated descriptor for an extra display column shown in a
 * {@link LookupDescription} pick-list popup.
 *
 * <p>Adds an additional column — identified by name and label — to the
 * lookup result grid, giving users more context when selecting a value.
 *
 * <p>Threading: not thread-safe.  Read-only after JAXB unmarshalling.
 *
 * @see LookupDescription
 */
@XmlRootElement(namespace = XMLMetaData.VIEW_NAMESPACE)
public class LookupDescriptionColumn implements NamedMetaData {
	private static final long serialVersionUID = -8247885015791117746L;

	private String name;
	private Boolean filterable;
	
	@Override
	public String getName() {
		return name;
	}
	
	@XmlValue
	public void setName(String name) {
		this.name = UtilImpl.processStringValue(name);
	}
	
	public Boolean getFilterable() {
		return filterable;
	}

	@XmlAttribute(name = "filterable", required = false)
	public void setFilterable(Boolean filterable) {
		this.filterable = filterable;
	}
}
