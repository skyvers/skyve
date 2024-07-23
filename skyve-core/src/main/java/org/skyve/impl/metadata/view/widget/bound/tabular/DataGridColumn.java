package org.skyve.impl.metadata.view.widget.bound.tabular;

import java.util.Map;
import java.util.TreeMap;

import org.skyve.impl.metadata.repository.PropertyMapAdapter;
import org.skyve.impl.metadata.view.AbsoluteWidth;
import org.skyve.impl.metadata.view.HorizontalAlignment;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.DecoratedMetaData;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlType;
import jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE, propOrder = {"title", "alignment", "pixelWidth", "properties"})
public abstract class DataGridColumn implements TabularColumn, AbsoluteWidth, DecoratedMetaData {
	private static final long serialVersionUID = -5532364729219436008L;

	private String title;
	private HorizontalAlignment alignment;
	private Integer pixelWidth;

	@XmlElement(namespace = XMLMetaData.VIEW_NAMESPACE)
	@XmlJavaTypeAdapter(PropertyMapAdapter.class)
	private Map<String, String> properties = new TreeMap<>();

	@Override
	public String getTitle() {
		return title;
	}

	@Override
	@XmlAttribute
	public void setTitle(String title) {
		this.title = UtilImpl.processStringValue(title);
	}

	@Override
	public HorizontalAlignment getAlignment() {
		return alignment;
	}

	@Override
	@XmlAttribute
	public void setAlignment(HorizontalAlignment alignment) {
		this.alignment = alignment;
	}

	@Override
	public Integer getPixelWidth() {
		return pixelWidth;
	}

	@Override
	@XmlAttribute
	public void setPixelWidth(Integer pixelWidth) {
		this.pixelWidth = pixelWidth;
	}

	@Override
	public Map<String, String> getProperties() {
		return properties;
	}
}
