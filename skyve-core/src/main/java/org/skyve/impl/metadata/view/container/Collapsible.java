package org.skyve.impl.metadata.view.container;

import org.skyve.impl.util.XMLMetaData;

import jakarta.annotation.Nonnull;
import jakarta.xml.bind.annotation.XmlType;

/**
 * JAXB-annotated enumeration of collapsibility modes for panel-type containers.
 *
 * <p>Controls whether a container can be collapsed by the user, and if so,
 * whether it starts expanded or collapsed.
 */
@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE)
@SuppressWarnings("java:S115") // Suppress "Constant names should comply with a naming convention" as these are not constants but enum values
public enum Collapsible {
	open("open"), closed("closed");
	
	private String collapsibleString;
	
	private Collapsible(@Nonnull String collapsibleString) {
		this.collapsibleString = collapsibleString;
	}
	
	/**
	 * Returns the metadata token used for XML serialization.
	 *
	 * @return the stable XML token for this collapse mode
	 */
	public @Nonnull String toCollapsibleString() {
		return this.collapsibleString;
	}
}
