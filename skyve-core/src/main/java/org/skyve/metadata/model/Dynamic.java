package org.skyve.metadata.model;

import java.util.Map;
import java.util.TreeMap;

import org.skyve.impl.metadata.repository.document.DynamicClassMapAdapter;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.SerializableMetaData;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;
import jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

@XmlRootElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
@XmlType(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
public class Dynamic implements SerializableMetaData {
	private static final long serialVersionUID = 4085569124380226712L;
	
	private String bizletClassName;
	private String dataFactoryClassName;

	@XmlElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
	@XmlJavaTypeAdapter(DynamicClassMapAdapter.class)
	private Map<String, String> actions = new TreeMap<>();

	@XmlElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
	@XmlJavaTypeAdapter(DynamicClassMapAdapter.class)
	private Map<String, String> images = new TreeMap<>();

	@XmlElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
	@XmlJavaTypeAdapter(DynamicClassMapAdapter.class)
	private Map<String, String> models = new TreeMap<>();

	public String getBizletClassName() {
		return bizletClassName;
	}

	@XmlAttribute(required = false)
	public void setBizletClassName(String className) {
		this.bizletClassName = className;
	}

	public String getDataFactoryClassName() {
		return dataFactoryClassName;
	}
	
	@XmlAttribute(required = false)
	public void setDataFactoryClassName(String dataFactoryClassName) {
		this.dataFactoryClassName = dataFactoryClassName;
	}
	
	public Map<String, String> getActions() {
		return actions;
	}

	public Map<String, String> getImages() {
		return images;
	}
	
	public Map<String, String> getModels() {
		return models;
	}
}
