package org.skyve.impl.metadata.view.widget.bound.input;

import java.util.Map;
import java.util.TreeMap;

import org.skyve.impl.metadata.repository.PropertyMapAdapter;
import org.skyve.impl.metadata.view.AbsoluteSize;
import org.skyve.impl.metadata.view.FormItemWidget;
import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;
import jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

/**
 * A widget that enables a signature to be drawn and captured.
 */
@XmlRootElement(namespace = XMLMetaData.VIEW_NAMESPACE)
@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE,
			propOrder = {"pixelWidth",
							"pixelHeight",
							"rgbHexBackgroundColour",
							"rgbHexForegroundColour",
							"properties"})
public class ContentSignature extends InputWidget implements AbsoluteSize, FormItemWidget {
	private static final long serialVersionUID = 7902784327466913291L;
	
	private Integer pixelWidth;
	private Integer pixelHeight;
	private String rgbHexBackgroundColour;
	private String rgbHexForegroundColour;
	
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

	@Override
	public Integer getPixelHeight() {
		return pixelHeight;
	}

	@Override
	@XmlAttribute(required = false)
	public void setPixelHeight(Integer pixelHeight) {
		this.pixelHeight = pixelHeight;
	}

	@Override
	public Map<String, String> getProperties() {
		return properties;
	}
	
	public String getRgbHexBackgroundColour() {
		return rgbHexBackgroundColour;
	}

	@XmlAttribute(required = false)
	public void setRgbHexBackgroundColour(String rgbHexBackgroundColour) {
		this.rgbHexBackgroundColour = rgbHexBackgroundColour;
	}
	
	public String getRgbHexForegroundColour() {
		return rgbHexForegroundColour;
	}

	@XmlAttribute(required = false)
	public void setRgbHexForegroundColour(String rgbHexForegroundColour) {
		this.rgbHexForegroundColour = rgbHexForegroundColour;
	}	
}
