package org.skyve.impl.metadata.view.widget.bound.input;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.skyve.impl.metadata.repository.PropertyMapAdapter;
import org.skyve.impl.metadata.view.AbsoluteWidth;
import org.skyve.impl.metadata.view.FormItemWidget;
import org.skyve.impl.metadata.view.widget.bound.ParameterImpl;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.view.Editable;
import org.skyve.metadata.view.Parameterizable;
import org.skyve.metadata.view.widget.bound.Parameter;
import org.skyve.util.Util;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlElementWrapper;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;
import jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

/**
 * Content-link widget that renders a clickable link for a
 * bound content attribute.
 *
 * <p>Supports a configurable display value, editability, request parameters,
 * and absolute width.
 * 
 * [value] is optional - defaults to "Content" or "Empty".
 *
 * <p>Threading: not thread-safe. Read-only after JAXB unmarshalling.
 */
@XmlRootElement(namespace = XMLMetaData.VIEW_NAMESPACE)
@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE,
			propOrder = {"value", "pixelWidth", "parameters", "properties"})
public class ContentLink extends InputWidget implements Editable, Parameterizable, AbsoluteWidth, FormItemWidget {
	private static final long serialVersionUID = 7902784327466913291L;
	
	private String value; // the title/label/value (not the href) of the link rendered on the UI
	private Boolean editable;
	private Integer pixelWidth;
	private List<Parameter> parameters = new ArrayList<>();
	
	@XmlElement(namespace = XMLMetaData.VIEW_NAMESPACE)
	@XmlJavaTypeAdapter(PropertyMapAdapter.class)
	private Map<String, String> properties = new TreeMap<>();

	/**
	 * Indicates that this widget renders with a form label by default.
	 */
	@Override
	public boolean showsLabelByDefault() {
		return true;
	}
	
	/**
	 * Returns the configured display value for the rendered link text.
	 */
	public String getValue() {
		return value;
	}
	
	/**
	 * Returns the display value resolved through the i18n message source.
	 *
	 * @return the localised link label, or {@code null} when no value is configured
	 */
	public String getLocalisedValue() {
		return Util.i18n(value);
	}

	/**
	 * Sets the display value after trimming and empty-string normalisation.
	 */
	@XmlAttribute(required = false)
	public void setValue(String value) {
		this.value = UtilImpl.processStringValue(value);
	}

	/**
	 * Returns whether link editing is explicitly enabled.
	 */
	@Override
	public Boolean getEditable() {
		return editable;
	}

	/**
	 * Sets whether link editing is enabled.
	 */
	@Override
	@XmlAttribute(name = "editable", required = false)
	public void setEditable(Boolean editable) {
		this.editable = editable;
	}

	/**
	 * Returns the absolute pixel width, or {@code null} when renderer defaults apply.
	 */
	@Override
	public Integer getPixelWidth() {
		return pixelWidth;
	}

	/**
	 * Sets the absolute pixel width.
	 */
	@Override
	@XmlAttribute(required = false)
	public void setPixelWidth(Integer pixelWidth) {
		this.pixelWidth = pixelWidth;
	}

	/**
	 * Returns the mutable parameter list forwarded when resolving link content.
	 */
	@Override
	@XmlElementWrapper(namespace = XMLMetaData.VIEW_NAMESPACE, name = "parameters")
	@XmlElement(name = "parameter", type = ParameterImpl.class, required = false)
	public List<Parameter> getParameters() {
		return parameters;
	}

	/**
	 * Returns the mutable decorator property map for this widget.
	 */
	@Override
	public Map<String, String> getProperties() {
		return properties;
	}
}
