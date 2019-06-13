package org.skyve.impl.metadata.view.widget.bound.input;

import java.util.Map;
import java.util.TreeMap;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import org.skyve.impl.metadata.repository.PropertyMapAdapter;
import org.skyve.impl.metadata.view.widget.bound.AbstractBound;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.DecoratedMetaData;

@XmlRootElement(namespace = XMLMetaData.VIEW_NAMESPACE, name = "default")
@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE, propOrder = {"properties"})
public class DefaultWidget extends AbstractBound implements DecoratedMetaData {
	private static final long serialVersionUID = 1007637557499176432L;

	@XmlElement(namespace = XMLMetaData.VIEW_NAMESPACE)
	@XmlJavaTypeAdapter(PropertyMapAdapter.class)
	private Map<String, String> properties = new TreeMap<>();

	@Override
	public Map<String, String> getProperties() {
		return properties;
	}
}
