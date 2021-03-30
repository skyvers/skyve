package org.skyve.impl.metadata.view.widget.bound.tabular;

import java.util.Map;
import java.util.TreeMap;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import org.skyve.impl.metadata.repository.PropertyMapAdapter;
import org.skyve.impl.metadata.view.HorizontalAlignment;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.DecoratedMetaData;
import org.skyve.metadata.view.TextOutput;

@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE, propOrder = {"title", "alignment", "pixelWidth", "escape", "sanitise", "properties"})
public abstract class DataGridColumn implements TabularColumn, DecoratedMetaData, TextOutput {
	private static final long serialVersionUID = -5532364729219436008L;

	private String title;
	private HorizontalAlignment alignment;
	private Integer pixelWidth;
	private Boolean escape;
	private Sanitisation sanitise;

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

	public Integer getPixelWidth() {
		return pixelWidth;
	}

	@XmlAttribute
	public void setPixelWidth(Integer pixelWidth) {
		this.pixelWidth = pixelWidth;
	}

	@Override
	public Boolean getEscape() {
		return escape;
	}

	@XmlAttribute
	public void setEscape(Boolean escape) {
		this.escape = escape;
	}

	@Override
	public Sanitisation getSanitise() {
		return sanitise;
	}

	@XmlAttribute
	public void setSanitise(Sanitisation sanitise) {
		this.sanitise = sanitise;
	}

	@Override
	public Map<String, String> getProperties() {
		return properties;
	}
}
