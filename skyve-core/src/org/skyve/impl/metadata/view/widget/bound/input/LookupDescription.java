package org.skyve.impl.metadata.view.widget.bound.input;

import java.util.LinkedHashSet;
import java.util.Set;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElementWrapper;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.metadata.view.AbsoluteWidth;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.view.Editable;
import org.skyve.impl.metadata.view.widget.bound.input.Lookup;

@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.VIEW_NAMESPACE)
public class LookupDescription extends Lookup implements Editable, AbsoluteWidth {
	private static final long serialVersionUID = -6196902681383778156L;

	private String descriptionBinding;
	private Boolean editable;
	private Integer pixelWidth;
	private Set<String> dropDownColumns = new LinkedHashSet<>();
	
	public String getDescriptionBinding() {
		return descriptionBinding;
	}

	@XmlAttribute(required = true)
	public void setDescriptionBinding(String descriptionBinding) {
		this.descriptionBinding = descriptionBinding;
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
	
	@XmlElementWrapper(namespace = XMLMetaData.VIEW_NAMESPACE, name = "dropDown")
	@XmlElement(namespace = XMLMetaData.VIEW_NAMESPACE, name = "column", required = false)
	public Set<String> getDropDownColumns() {
		return dropDownColumns;
	}
}
