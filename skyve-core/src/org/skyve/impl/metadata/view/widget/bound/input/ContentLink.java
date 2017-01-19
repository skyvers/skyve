package org.skyve.impl.metadata.view.widget.bound.input;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElementWrapper;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.metadata.view.AbsoluteWidth;
import org.skyve.impl.metadata.view.FormItemWidget;
import org.skyve.impl.metadata.view.widget.bound.ParameterImpl;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.view.Editable;
import org.skyve.metadata.view.Parameterizable;
import org.skyve.metadata.view.widget.bound.Parameter;
import org.skyve.impl.metadata.view.widget.bound.input.InputWidget;

/**
 * value is optional - defaults to "Content" or "Empty".
 */
@XmlRootElement(namespace = XMLMetaData.VIEW_NAMESPACE)
@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE,
			propOrder = {"value", "pixelWidth", "parameters"})
public class ContentLink extends InputWidget implements Editable, Parameterizable, AbsoluteWidth, FormItemWidget {
	private static final long serialVersionUID = 7902784327466913291L;
	
	private String value; // the title/label/value (not the href) of the link rendered on the UI
	private Boolean editable;
	private Integer pixelWidth;
	private List<Parameter> parameters = new ArrayList<>();
	
	@Override
	public boolean showsLabelByDefault() {
		return true;
	}
	
	public String getValue() {
		return value;
	}

	@XmlAttribute(required = false)
	public void setValue(String value) {
		this.value = UtilImpl.processStringValue(value);
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

	@Override
	@XmlElementWrapper(namespace = XMLMetaData.VIEW_NAMESPACE, name = "parameters")
	@XmlElement(name = "parameter", type = ParameterImpl.class, required = false)
	public List<Parameter> getParameters() {
		return parameters;
	}
}
