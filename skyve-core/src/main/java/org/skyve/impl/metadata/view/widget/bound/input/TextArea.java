package org.skyve.impl.metadata.view.widget.bound.input;

import java.util.Map;
import java.util.TreeMap;

import org.skyve.impl.metadata.repository.PropertyMapAdapter;
import org.skyve.impl.metadata.view.AbsoluteSize;
import org.skyve.impl.metadata.view.FormItemWidget;
import org.skyve.impl.metadata.view.MinimumHeight;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.view.Editable;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;
import jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

@XmlRootElement(namespace = XMLMetaData.VIEW_NAMESPACE)
@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE,
			propOrder = {"wordWrap", "editable", "keyboardType", "pixelWidth", "pixelHeight", "minPixelHeight", "properties"})
public class TextArea extends ChangeableInputWidget implements Editable, AbsoluteSize, MinimumHeight, FormItemWidget {
	private static final long serialVersionUID = 7376326511023184723L;

	private Boolean wordWrap;
	private Boolean editable;
	private Integer pixelWidth;
	private Integer pixelHeight;
	private Integer minPixelHeight;
	private KeyboardType keyboardType;

	@XmlElement(namespace = XMLMetaData.VIEW_NAMESPACE)
	@XmlJavaTypeAdapter(PropertyMapAdapter.class)
	private Map<String, String> properties = new TreeMap<>();

	@Override
	public boolean showsLabelByDefault() {
		return true;
	}
	
	public Boolean getWordWrap() {
		return wordWrap;
	}

	@XmlAttribute(name = "wrap", required = false)
	public void setWordWrap(Boolean wordWrap) {
		this.wordWrap = wordWrap;
	}

	@Override
	public Boolean getEditable() {
		return editable;
	}

	@Override
	@XmlAttribute(name = "editable", required = false)
	public void setEditable(Boolean editable) {
		this.editable = editable;
	}

	public KeyboardType getKeyboardType() {
		return keyboardType;
	}

	@XmlAttribute(name = "keyboardType", required = false)
	public void setKeyboardType(KeyboardType keyboardType) {
		this.keyboardType = keyboardType;
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
	public Integer getMinPixelHeight() {
		return minPixelHeight;
	}

	@Override
	@XmlAttribute(required = false)
	public void setMinPixelHeight(Integer minPixelHeight) {
		this.minPixelHeight = minPixelHeight;
	}

	@Override
	public Map<String, String> getProperties() {
		return properties;
	}
}
