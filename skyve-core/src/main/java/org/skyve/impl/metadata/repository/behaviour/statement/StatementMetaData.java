package org.skyve.impl.metadata.repository.behaviour.statement;

import java.util.Map;
import java.util.TreeMap;

import org.skyve.domain.Bean;
import org.skyve.impl.metadata.repository.PropertyMapAdapter;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.DecoratedMetaData;

import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlType;
import jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

/**
 * JAXB base type for all behaviour statement elements in an action descriptor.
 *
 * <p>Concrete subclasses ({@link org.skyve.impl.metadata.behaviour.IfStatement},
 * {@link org.skyve.impl.metadata.behaviour.SetStatement}, etc.) add the
 * statement-specific properties.  Carries an open-ended property map for
 * future extensibility.
 *
 * <p>Threading: not thread-safe.  Instances are populated during JAXB unmarshalling
 * and are read-only once placed in the repository cache.
 *
 * @see org.skyve.impl.metadata.behaviour.IfStatement
 * @see org.skyve.impl.metadata.behaviour.SetStatement
 */
@XmlType(namespace = XMLMetaData.BEHAVIOUR_NAMESPACE, propOrder = {"properties"})
public abstract class StatementMetaData implements DecoratedMetaData {
	private static final long serialVersionUID = 3100286315509858592L;
	
	@XmlElement(namespace = XMLMetaData.BEHAVIOUR_NAMESPACE)
	@XmlJavaTypeAdapter(PropertyMapAdapter.class)
	private Map<String, String> properties = new TreeMap<>();

	/**
	 * Returns decorator properties attached to this statement metadata.
	 *
	 * @return mutable statement property map
	 */
	@Override
	public Map<String, String> getProperties() {
		return properties;
	}

	/**
	 * Executes this statement against the supplied bean.
	 *
	 * @param bean the current document instance for statement evaluation
	 */
	public abstract void execute(Bean bean);
}
