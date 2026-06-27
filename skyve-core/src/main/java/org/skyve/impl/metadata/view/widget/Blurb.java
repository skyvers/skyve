package org.skyve.impl.metadata.view.widget;

import java.util.Map;
import java.util.TreeMap;

import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.domain.types.jaxb.CDATAAdapter;
import org.skyve.impl.metadata.view.AbsoluteSize;
import org.skyve.impl.metadata.view.ContentSpecifiedWidth;
import org.skyve.impl.metadata.view.FormItemWidget;
import org.skyve.impl.metadata.view.HorizontalAlignment;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.view.Invisible;
import org.skyve.metadata.view.TextOutput;
import org.skyve.util.Util;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;
import jakarta.xml.bind.annotation.XmlValue;
import jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

/**
 * Renders static or localisable markup content in a view form item.
 *
 * <p>The markup value is stored as CDATA for XML metadata round-tripping.
 * Supports optional absolute sizing, text alignment, visibility conditions,
 * and text escaping/sanitisation controls.
 * 
 * If a label width/height is not specified, it sizes to fit its contents.
 *
 * <p>Threading: not thread-safe. Read-only after JAXB unmarshalling.
 */
@XmlRootElement(namespace = XMLMetaData.VIEW_NAMESPACE)
@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE,
			propOrder = {"markup",
							"pixelWidth", 
							"pixelHeight", 
							"textAlignment",
							"invisibleConditionName",
							"visibleConditionName",
							"escape", 
							"sanitise"})
							//"properties"})
// Blurb markup value cant be an XML value and have properties.
public class Blurb implements Invisible, AbsoluteSize, ContentSpecifiedWidth, FormItemWidget, TextOutput {
	private static final long serialVersionUID = -1234525506006033853L;

	/**
	 * The content to display in the blurb
	 */
	private String markup;

	private Integer pixelWidth;
	private Integer pixelHeight;
	
	private String invisibleConditionName;

	/**
	 * Default alignment is left.
	 */
	private HorizontalAlignment textAlignment = null;
	
	private Boolean escape;
	private Sanitisation sanitise;

//	@XmlElement(namespace = XMLMetaData.VIEW_NAMESPACE)
//	@XmlJavaTypeAdapter(PropertyMapAdapter.class)
	private Map<String, String> properties = new TreeMap<>();

	/**
	 * Indicates that this widget does not render an external form label by default.
	 */
	@Override
	public boolean showsLabelByDefault() {
		return false;
	}
	
	/**
	 * Returns the configured markup content.
	 */
	public String getMarkup() {
		return markup;
	}

	/**
	 * Returns the markup localised for the current user locale.
	 */
	public String getLocalisedMarkup() {
		return Util.i18n(markup);
	}
	
	/**
	 * Sets the markup content after trimming and empty-string normalisation.
	 */
	@XmlValue
	@XmlJavaTypeAdapter(CDATAAdapter.class)
	public void setMarkup(String markup) {
		this.markup = UtilImpl.processStringValue(markup);
	}

	/**
	 * Returns the absolute pixel width, or {@code null} when content-driven sizing is used.
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
	 * Returns the absolute pixel height, or {@code null} when content-driven sizing is used.
	 */
	@Override
	public Integer getPixelHeight() {
		return pixelHeight;
	}

	/**
	 * Sets the absolute pixel height.
	 */
	@Override
	@XmlAttribute(required = false)
	public void setPixelHeight(Integer pixelHeight) {
		this.pixelHeight = pixelHeight;
	}

	/**
	 * Returns the condition expression that hides this widget.
	 */
	@Override
	public String getInvisibleConditionName() {
		return invisibleConditionName;
	}

	/**
	 * Sets the condition expression that hides this widget.
	 */
	@Override
	@XmlAttribute(name = "invisible", required = false)
	public void setInvisibleConditionName(String invisibleConditionName) {
		this.invisibleConditionName = UtilImpl.processStringValue(invisibleConditionName);
	}

	// to enable JAXB XML marshaling
	@SuppressWarnings("static-method")
	String getVisibleConditionName() {
		return null;
	}

	/**
	 * Sets the visible condition by storing its negation as the internal invisible condition.
	 */
	@Override
	@XmlAttribute(name = "visible", required = false)
	public void setVisibleConditionName(String visibleConditionName) {
		this.invisibleConditionName = BindUtil.negateCondition(UtilImpl.processStringValue(visibleConditionName));
	}

	/**
	 * Returns the configured text alignment override.
	 *
	 * <p>When {@code null}, renderer defaults apply.
	 */
	public HorizontalAlignment getTextAlignment() {
		return textAlignment;
	}

	/**
	 * Sets the text alignment override.
	 */
	@XmlAttribute(name = "textAlignment", required = false)
	public void setTextAlignment(HorizontalAlignment textAlignment) {
		this.textAlignment = textAlignment;
	}

	/**
	 * Returns whether output escaping is explicitly configured.
	 */
	@Override
	public Boolean getEscape() {
		return escape;
	}

	/**
	 * Sets explicit output escaping behaviour.
	 */
	@XmlAttribute
	public void setEscape(Boolean escape) {
		this.escape = escape;
	}

	/**
	 * Returns the configured sanitisation mode.
	 */
	@Override
	public Sanitisation getSanitise() {
		return sanitise;
	}

	/**
	 * Sets the sanitisation mode for markup rendering.
	 */
	@XmlAttribute
	public void setSanitise(Sanitisation sanitise) {
		this.sanitise = sanitise;
	}
	
	/**
	 * Returns the mutable decorator property map.
	 */
	@Override
	public Map<String, String> getProperties() {
		return properties;
	}
}
