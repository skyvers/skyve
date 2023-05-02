package org.skyve.impl.metadata.repository.behaviour.statement;

import java.util.Map;
import java.util.TreeMap;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import org.skyve.domain.Bean;
import org.skyve.impl.metadata.repository.PropertyMapAdapter;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.DecoratedMetaData;

@XmlType(namespace = XMLMetaData.BEHAVIOUR_NAMESPACE)
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
