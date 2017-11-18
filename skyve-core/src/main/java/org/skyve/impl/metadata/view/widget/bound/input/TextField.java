package org.skyve.impl.metadata.view.widget.bound.input;

import java.util.Map;
import java.util.TreeMap;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import org.skyve.impl.metadata.repository.PropertyMapAdapter;
import org.skyve.impl.metadata.view.AbsoluteWidth;
import org.skyve.impl.metadata.view.FormItemWidget;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.view.Editable;
import org.skyve.impl.metadata.view.widget.bound.input.ChangeableInputWidget;

@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE,
			propOrder = {"editable", "previousValues", "pixelWidth", "properties"})
@XmlRootElement(namespace = XMLMetaData.VIEW_NAMESPACE)
public class TextField extends ChangeableInputWidget implements Editable, AbsoluteWidth, FormItemWidget {
	private static final long serialVersionUID = -5061565177091806441L;

	private Boolean editable;
	private Boolean previousValues;
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

	@Override
	public Integer getPixelWidth() {
		return pixelWidth;
	}

	@Override
	@XmlAttribute(required = false)
	public void setPixelWidth(Integer pixelWidth) {
		this.pixelWidth = pixelWidth;
	}

	public Boolean getPreviousValues() {
		return previousValues;
	}

	@XmlAttribute(required = false)
	public void setPreviousValues(Boolean previousValues) {
		this.previousValues = previousValues;
	}

	@Override
	public Map<String, String> getProperties() {
		return properties;
	}
}
