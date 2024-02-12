package org.skyve.impl.metadata.view.container.form;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.skyve.impl.metadata.repository.PropertyMapAdapter;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.DecoratedMetaData;

import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlElementRef;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;
import jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.VIEW_NAMESPACE)
public class FormRow implements DecoratedMetaData {
	private static final long serialVersionUID = 6724393610453650734L;

	private List<FormItem> items = new ArrayList<>();
	
	@XmlElement(namespace = XMLMetaData.VIEW_NAMESPACE)
	@XmlJavaTypeAdapter(PropertyMapAdapter.class)
	private Map<String, String> properties = new TreeMap<>();

	@XmlElementRef(type = FormItem.class)
	public List<FormItem> getItems() {
		return items;
	}

	@Override
	public Map<String, String> getProperties() {
		return properties;
	}
}
