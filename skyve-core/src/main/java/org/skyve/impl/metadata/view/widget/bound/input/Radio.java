package org.skyve.impl.metadata.view.widget.bound.input;

import java.util.Map;
import java.util.TreeMap;

import org.skyve.impl.metadata.repository.PropertyMapAdapter;
import org.skyve.impl.metadata.view.AbsoluteWidth;
import org.skyve.impl.metadata.view.FormItemWidget;
import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;
import jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.VIEW_NAMESPACE)
public final class Radio extends ChangeableInputWidget implements AbsoluteWidth, FormItemWidget {
	private static final long serialVersionUID = -6194966991364516589L;
	
	private Boolean vertical;
	private Integer pixelWidth;

	@XmlElement(namespace = XMLMetaData.VIEW_NAMESPACE)
	@XmlJavaTypeAdapter(PropertyMapAdapter.class)
	private Map<String, String> properties = new TreeMap<>();

	@Override
	public boolean showsLabelByDefault() {
		return true;
	}
	
	@Override
	public Integer getPixelWidth() {
		return pixelWidth;
	}

	@Override
	@XmlAttribute(required = false)
	public void setPixelWidth(Integer pixelWidth) {
		this.pixelWidth = pixelWidth;
	}
	
	public Boolean getVertical() {
		return vertical;
	}

	@XmlAttribute(required = false)
	public void setVertical(Boolean vertical) {
		this.vertical = vertical;
	}

	@Override
	public Map<String, String> getProperties() {
		return properties;
	}
}
