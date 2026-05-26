package org.skyve.impl.metadata.repository.module;

import java.util.ArrayList;
import java.util.List;

import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * Abstract JAXB base for all menu item descriptor types in a module menu.
 *
 * <p>Carries the item name and the optional list of {@link ApplicableTo} UX/UI
 * scope filters.  Concrete subclasses ({@link CalendarItemMetaData},
 * {@link EditItemMetaData}, {@link ListItemMetaData}, etc.) add type-specific
 * properties (document name, query, role grants, etc.).
 *
 * <p>Threading: not thread-safe.  Read-only after JAXB unmarshalling.
 *
 * @see GroupMetaData
 * @see CalendarItemMetaData
 * @see ListItemMetaData
 */
@XmlType(namespace = XMLMetaData.MODULE_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.MODULE_NAMESPACE, name = "item")
public abstract class ItemMetaData extends ActionMetaData {
	private static final long serialVersionUID = -2533938765989721342L;

	private List<GrantedTo> roles = new ArrayList<>();

	@XmlElement(namespace = XMLMetaData.MODULE_NAMESPACE, name = "role", required = true)
	public List<GrantedTo> getRoles() {
		return roles;
	}
}
