package org.skyve.impl.metadata.view.widget.bound.input;

import java.util.LinkedHashMap;
import java.util.Map;

import org.skyve.impl.metadata.repository.PropertyMapAdapter;
import org.skyve.impl.metadata.view.AbsoluteSize;
import org.skyve.impl.metadata.view.FormItemWidget;
import org.skyve.impl.metadata.view.MinimumHeight;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.view.Editable;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;
import jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

/**
 * JAXB-annotated multi-line text-area input widget bound to a String attribute.
 *
 * <p>Renders a resizable text area with configurable word-wrap, editability,
 * keyboard type, and pixel dimensions.
 *
 * <p>Threading: not thread-safe.  Read-only after JAXB unmarshalling.
 */
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
	private Map<String, String> properties = new LinkedHashMap<>();

	@Override
	public boolean showsLabelByDefault() {
		return true;
	}
	
	public @Nullable Boolean getWordWrap() {
		return wordWrap;
	}

	@XmlAttribute(name = "wrap", required = false)
	public void setWordWrap(@Nullable Boolean wordWrap) {
		this.wordWrap = wordWrap;
	}

	@Override
	public @Nullable Boolean getEditable() {
		return editable;
	}

	@Override
	@XmlAttribute(name = "editable", required = false)
	public void setEditable(@Nullable Boolean editable) {
		this.editable = editable;
	}

	public @Nullable KeyboardType getKeyboardType() {
		return keyboardType;
	}

	@XmlAttribute(name = "keyboardType", required = false)
	public void setKeyboardType(@Nullable KeyboardType keyboardType) {
		this.keyboardType = keyboardType;
	}

	@Override
	public @Nullable Integer getPixelWidth() {
		return pixelWidth;
	}

	@Override
	@XmlAttribute(required = false)
	public void setPixelWidth(@Nullable Integer pixelWidth) {
		this.pixelWidth = pixelWidth;
	}

	@Override
	public @Nullable Integer getPixelHeight() {
		return pixelHeight;
	}

	@Override
	@XmlAttribute(required = false)
	public void setPixelHeight(@Nullable Integer pixelHeight) {
		this.pixelHeight = pixelHeight;
	}

	@Override
	public @Nullable Integer getMinPixelHeight() {
		return minPixelHeight;
	}

	@Override
	@XmlAttribute(required = false)
	public void setMinPixelHeight(@Nullable Integer minPixelHeight) {
		this.minPixelHeight = minPixelHeight;
	}

	@Override
	public @Nonnull Map<String, String> getProperties() {
		return properties;
	}
}
