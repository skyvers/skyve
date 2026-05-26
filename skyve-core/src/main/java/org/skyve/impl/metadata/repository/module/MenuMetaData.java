package org.skyve.impl.metadata.repository.module;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.skyve.impl.metadata.repository.PropertyMapAdapter;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.DecoratedMetaData;

import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlElementRef;
import jakarta.xml.bind.annotation.XmlElementRefs;
import jakarta.xml.bind.annotation.XmlType;
import jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

/**
 * JAXB-annotated holder for the top-level module menu structure.
 *
 * <p>Contains the ordered list of root-level menu groups and items for a module
 * as declared in the {@code module.xml} file.  The menu entries are polymorphic
 * JAXB element-refs that resolve to concrete {@link ItemMetaData} and
 * {@link GroupMetaData} instances.
 *
 * <p>Threading: not thread-safe.  Read-only after JAXB unmarshalling.
 *
 * @see GroupMetaData
 * @see ItemMetaData
 */
@XmlType(namespace = XMLMetaData.MODULE_NAMESPACE)
public class MenuMetaData implements DecoratedMetaData {
	private static final long serialVersionUID = 8381343095222755228L;

	private List<ActionMetaData> actions = new ArrayList<>();
	
	@XmlElement(namespace = XMLMetaData.MODULE_NAMESPACE)
	@XmlJavaTypeAdapter(PropertyMapAdapter.class)
	private Map<String, String> properties = new TreeMap<>();


	@XmlElementRefs({@XmlElementRef(type = ItemMetaData.class), @XmlElementRef(type = GroupMetaData.class)})
	public List<ActionMetaData> getActions() {
		return actions;
	}
	
	@Override
	public Map<String, String> getProperties() {
		return properties;
	}
}
