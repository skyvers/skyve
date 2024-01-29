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

@XmlType(namespace = XMLMetaData.BEHAVIOUR_NAMESPACE, propOrder = {"properties"})
public abstract class StatementMetaData implements DecoratedMetaData {
	private static final long serialVersionUID = 3100286315509858592L;
	
	@XmlElement(namespace = XMLMetaData.BEHAVIOUR_NAMESPACE)
	@XmlJavaTypeAdapter(PropertyMapAdapter.class)
	private Map<String, String> properties = new TreeMap<>();

	@Override
	public Map<String, String> getProperties() {
		return properties;
	}

	public abstract void execute(Bean bean);
}
