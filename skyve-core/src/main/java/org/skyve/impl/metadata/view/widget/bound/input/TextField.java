package org.skyve.impl.metadata.view.widget.bound.input;

import java.util.Map;
import java.util.TreeMap;

import org.skyve.impl.metadata.repository.PropertyMapAdapter;
import org.skyve.impl.metadata.view.AbsoluteWidth;
import org.skyve.impl.metadata.view.FormItemWidget;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.view.Editable;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;
import jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE,
			propOrder = {"editable", "keyboardType", "complete", "pixelWidth", "properties"})
@XmlRootElement(namespace = XMLMetaData.VIEW_NAMESPACE)
public class TextField extends ChangeableInputWidget implements Editable, AbsoluteWidth, FormItemWidget {
	private static final long serialVersionUID = -5061565177091806441L;

	private Boolean editable;
	private KeyboardType keyboardType;
	private CompleteType complete;
	private Integer pixelWidth;
	
	@XmlElement(namespace = XMLMetaData.VIEW_NAMESPACE)
	@XmlJavaTypeAdapter(PropertyMapAdapter.class)
	private Map<String, String> properties = new TreeMap<>();

	@Override
	public boolean showsLabelByDefault() {
		return true;
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

	public CompleteType getComplete() {
		return complete;
	}

	@XmlAttribute(required = false)
	public void setComplete(CompleteType complete) {
		this.complete = complete;
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
	public Map<String, String> getProperties() {
		return properties;
	}
}
