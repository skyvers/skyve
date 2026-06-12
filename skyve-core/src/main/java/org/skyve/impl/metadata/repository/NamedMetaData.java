package org.skyve.impl.metadata.repository;

import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;

import com.google.common.base.MoreObjects;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlType;

/**
 * JAXB base type for all named metadata elements.
 *
 * <p>Every descriptor element that has a {@code name} attribute — documents, views,
 * queries, roles, etc. — extends this class.  The {@code name} is the primary key used
 * by the repository to look up and cache metadata objects.
 *
 * <p>Threading: not thread-safe.  Instances are populated during JAXB unmarshalling
 * and are read-only once placed in the repository cache.
 *
 * @see org.skyve.metadata.NamedMetaData
 */
@XmlType(namespace = XMLMetaData.COMMON_NAMESPACE)
@SuppressWarnings("java:S2176") // JAXB metadata vocabulary intentionally mirrors org.skyve.metadata.NamedMetaData.
public abstract class NamedMetaData implements org.skyve.metadata.NamedMetaData {
	private static final long serialVersionUID = 3158067742748907120L;

	private String name;

	@Override
	public String getName() {
		return name;
	}

	@XmlAttribute
	public void setName(String name) {
		this.name = UtilImpl.processStringValue(name);
	}
	
    @Override
    public String toString() {

        return MoreObjects.toStringHelper(this)
                          .add("name", name)
                          .toString();
    }
}
