package org.skyve.impl.metadata.repository.behaviour;

import java.util.Map;
import java.util.TreeMap;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import org.skyve.impl.domain.types.jaxb.CDATAAdapter;
import org.skyve.impl.metadata.repository.ConvertableMetaData;
import org.skyve.impl.metadata.repository.PropertyMapAdapter;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.DecoratedMetaData;
import org.skyve.metadata.repository.ProvidedRepository;

@XmlRootElement(namespace = XMLMetaData.BEHAVIOUR_NAMESPACE, name = "bizlet")
@XmlType(namespace = XMLMetaData.BEHAVIOUR_NAMESPACE, 
			name = "bizlet",
			propOrder = {"documentation", "properties"})
public class BizletMetaData implements ConvertableMetaData<BizletMetaData>, DecoratedMetaData {
	private static final long serialVersionUID = 4870898727945477449L;

	private String documentation;

	private long lastModifiedMillis = Long.MAX_VALUE;
	
	@XmlElement(namespace = XMLMetaData.BEHAVIOUR_NAMESPACE)
	@XmlJavaTypeAdapter(PropertyMapAdapter.class)
	private Map<String, String> properties = new TreeMap<>();

	public String getDocumentation() {
		return documentation;
	}
	
	@XmlElement(namespace = XMLMetaData.BEHAVIOUR_NAMESPACE)
	@XmlJavaTypeAdapter(CDATAAdapter.class)
	public void setDocumentation(String documentation) {
		this.documentation = UtilImpl.processStringValue(documentation);
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
	public BizletMetaData convert(String metaDataName, ProvidedRepository repository) {
		return this;
	}
	
	@Override
	public Map<String, String> getProperties() {
		return properties;
	}
}
