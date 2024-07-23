package org.skyve.impl.metadata.view.event;

import java.util.Map;
import java.util.TreeMap;

import org.skyve.impl.metadata.view.widget.bound.AbstractBound;
import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;
import jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import org.skyve.impl.metadata.repository.PropertyMapAdapter;

@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.VIEW_NAMESPACE, name = "toggleVisibility")
public class ToggleVisibilityEventAction extends AbstractBound implements EventAction {
	private static final long serialVersionUID = 7698439970713412634L;

	@XmlElement(namespace = XMLMetaData.VIEW_NAMESPACE)
	@XmlJavaTypeAdapter(PropertyMapAdapter.class)
	private Map<String, String> properties = new TreeMap<>();

	@Override
	public Map<String, String> getProperties() {
		return properties;
	}
}
