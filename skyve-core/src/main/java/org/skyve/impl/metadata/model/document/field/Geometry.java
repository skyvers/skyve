package org.skyve.impl.metadata.model.document.field;

import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.model.document.DomainType;

import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlTransient;
import jakarta.xml.bind.annotation.XmlType;

/**
 * Document field type for 2D spatial geometry values using JTS geometry.
 *
 * <p>Stored as a spatial column (e.g., {@code GEOMETRY} on MySQL/MariaDB,
 * {@code geometry} on PostgreSQL with PostGIS).  The domain type is
 * {@link org.locationtech.jts.geom.Geometry}.  Coordinates use the WGS-84
 * coordinate reference system by default.
 *
 * <p>Threading: not thread-safe.  Instances are populated during metadata loading
 * and are read-only once placed in the repository cache.
 *
 * @see Field
 */
@XmlType(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
public class Geometry extends Field {
	private static final long serialVersionUID = -6419883664269071344L;

	public Geometry() {
		setAttributeType(AttributeType.geometry);
	}

	/**
	 * Not used
	 */
	@Override
	@XmlTransient
	public DomainType getDomainType() {
		return null;
	}
}
