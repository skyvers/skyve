package org.skyve.impl.metadata.view.widget;

import java.util.Map;
import java.util.TreeMap;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.XmlValue;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

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

/**
 * If a label width/height is not specified, it sizes to fit its contents.
 * 
 * @author mike
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

	@Override
	public boolean showsLabelByDefault() {
		return false;
	}
	
	public String getMarkup() {
		return markup;
	}

	public String getLocalisedMarkup() {
		return Util.i18n(markup);
	}
	
	@XmlValue
	@XmlJavaTypeAdapter(CDATAAdapter.class)
	public void setMarkup(String markup) {
		this.markup = UtilImpl.processStringValue(markup);
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
	public String getInvisibleConditionName() {
		return invisibleConditionName;
	}

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

	@Override
	@XmlAttribute(name = "visible", required = false)
	public void setVisibleConditionName(String visibleConditionName) {
		this.invisibleConditionName = BindUtil.negateCondition(UtilImpl.processStringValue(visibleConditionName));
	}

	public HorizontalAlignment getTextAlignment() {
		return textAlignment;
	}

	@XmlAttribute(name = "textAlignment", required = false)
	public void setTextAlignment(HorizontalAlignment textAlignment) {
		this.textAlignment = textAlignment;
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
