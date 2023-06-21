package org.skyve.impl.metadata.repository.behaviour;

import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import org.skyve.domain.Bean;
import org.skyve.domain.messages.ValidationException;
import org.skyve.impl.domain.types.jaxb.CDATAAdapter;
import org.skyve.impl.metadata.repository.ConvertableMetaData;
import org.skyve.impl.metadata.repository.PropertyMapAdapter;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.DecoratedMetaData;
import org.skyve.metadata.model.document.Bizlet.DomainValue;
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
	
	@SuppressWarnings("unused")
	public void newInstance(Bean bean) {
		// TODO not implemented yet
	}
	
	@SuppressWarnings("unused")
	public void validate(Bean bean, ValidationException e) {
		// TODO not implemented yet
	}
	
	@SuppressWarnings({"static-method", "unused"})
	public List<DomainValue> getConstantDomainValues(String attributeName) throws Exception {
		// TODO not implemented yet
		return null;
	}
	
	@SuppressWarnings({"static-method", "unused"})
	public List<DomainValue> getVariantDomainValues(String attributeName) throws Exception {
		// TODO not implemented yet
		return null;
	}

	@SuppressWarnings({"static-method", "unused"})
	public List<DomainValue> getDynamicDomainValues(String attributeName, Bean bean) throws Exception {
		// TODO not implemented yet
		return null;
	}
	
	@SuppressWarnings({"static-method", "unused"})
	public List<String> complete(String attributeName, String value, Bean bean) throws Exception {
		// TODO not implemented yet
		return null;
	}
	
	@SuppressWarnings({"static-method", "unused"})
	public Bean resolve(String bizId, Bean conversationBean) throws Exception {
		return null;
	}
	
	@SuppressWarnings("unused")
	public void preSave(Bean bean) throws Exception {
		// TODO not implemented yet
	}
	
	@SuppressWarnings("unused")
	public void postSave(Bean bean) throws Exception {
		// TODO not implemented yet
	}
	
	@SuppressWarnings("unused")
	public void preDelete(Bean bean) throws Exception {
		// TODO not implemented yet
	}
	
	@SuppressWarnings("unused")
	public void postDelete(Bean bean) throws Exception {
		// TODO not implemented yet
	}
	
	@SuppressWarnings("unused")
	public void postLoad(Bean bean) throws Exception {
		// TODO not implemented yet
	}
	
	@SuppressWarnings("unused")
	public void preRerender(String source, Bean bean) throws Exception {
		// TODO not implemented yet
	}
	
	@SuppressWarnings("unused")
	public void postRender(Bean bean) {
		// TODO not implemented yet
	}
}
