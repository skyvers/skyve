package org.skyve.impl.metadata.repository.behaviour;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.skyve.domain.Bean;
import org.skyve.impl.domain.types.jaxb.CDATAAdapter;
import org.skyve.impl.metadata.behaviour.IfStatement;
import org.skyve.impl.metadata.behaviour.InvokeStatement;
import org.skyve.impl.metadata.behaviour.InvokeStaticStatement;
import org.skyve.impl.metadata.behaviour.SetStatement;
import org.skyve.impl.metadata.repository.ConvertibleMetaData;
import org.skyve.impl.metadata.repository.PropertyMapAdapter;
import org.skyve.impl.metadata.repository.behaviour.statement.StatementMetaData;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.DecoratedMetaData;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.NamedMetaData;
import org.skyve.metadata.ReloadableMetaData;
import org.skyve.metadata.repository.ProvidedRepository;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlElementRef;
import jakarta.xml.bind.annotation.XmlElementRefs;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlTransient;
import jakarta.xml.bind.annotation.XmlType;
import jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

@XmlRootElement(namespace = XMLMetaData.BEHAVIOUR_NAMESPACE, name = "action")
@XmlType(namespace = XMLMetaData.BEHAVIOUR_NAMESPACE, 
			name = "action",
			propOrder = {"name", "documentation", "statements", "properties"})
public class ActionMetaData implements NamedMetaData, ConvertibleMetaData<ActionMetaData>, ReloadableMetaData, DecoratedMetaData {
	private static final long serialVersionUID = 226463757653299558L;

	private String name;
	private String documentation;

	@XmlElementRefs({@XmlElementRef(type = IfStatement.class),
						@XmlElementRef(type = SetStatement.class),
						@XmlElementRef(type = InvokeStatement.class),
						@XmlElementRef(type = InvokeStaticStatement.class)})
	private List<StatementMetaData> statements = new ArrayList<>();

	private long lastModifiedMillis = Long.MAX_VALUE;
	private long lastCheckedMillis = System.currentTimeMillis();
	
	@XmlElement(namespace = XMLMetaData.BEHAVIOUR_NAMESPACE)
	@XmlJavaTypeAdapter(PropertyMapAdapter.class)
	private Map<String, String> properties = new TreeMap<>();

	@Override
	public String getName() {
		return name;
	}

	@XmlAttribute(required = true)
	public void setName(String name) {
		this.name = UtilImpl.processStringValue(name);
	}

	public String getDocumentation() {
		return documentation;
	}
	
	@XmlElement(namespace = XMLMetaData.BEHAVIOUR_NAMESPACE)
	@XmlJavaTypeAdapter(CDATAAdapter.class)
	public void setDocumentation(String documentation) {
		this.documentation = UtilImpl.processStringValue(documentation);
	}

	public List<StatementMetaData> getStatements() {
		return statements;
	}

	@Override
	public long getLastModifiedMillis() {
		return lastModifiedMillis;
	}

	@XmlTransient
	public void setLastModifiedMillis(long lastModifiedMillis) {
		this.lastModifiedMillis = lastModifiedMillis;
	}
	
	@Override
	public long getLastCheckedMillis() {
		return lastCheckedMillis;
	}

	@Override
	@XmlTransient
	public void setLastCheckedMillis(long lastCheckedMillis) {
		this.lastCheckedMillis = lastCheckedMillis;
	}

	@Override
	public ActionMetaData convert(String metaDataName, ProvidedRepository repository) {
		String theName = getName();
		if (theName == null) {
			throw new MetaDataException(metaDataName + " : The action [name] is required for action " + metaDataName);
		}

		// TODO validate statements recursively
		
		return this;
	}
	
	@Override
	public Map<String, String> getProperties() {
		return properties;
	}
	
	/**
	 * Execute this action.
	 */
	public void execute(Bean bean) {
		statements.forEach(s -> s.execute(bean));
	}
}
