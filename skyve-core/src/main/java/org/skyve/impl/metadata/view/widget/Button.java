package org.skyve.impl.metadata.view.widget;

import java.util.Map;
import java.util.TreeMap;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import org.skyve.impl.metadata.repository.PropertyMapAdapter;
import org.skyve.impl.metadata.view.AbsoluteSize;
import org.skyve.impl.metadata.view.ConstrainableHeight;
import org.skyve.impl.metadata.view.ContentSpecifiedWidth;
import org.skyve.impl.metadata.view.FormItemWidget;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;

/**
 * If button width/height is not specified it sizes to fit it's contents - the label and icon.
 * 
 * @author mike
 */
@XmlRootElement(namespace = XMLMetaData.VIEW_NAMESPACE)
@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE,
			propOrder = {"actionName", 
							"pixelWidth", 
							"pixelHeight",
							"minPixelHeight",
							"maxPixelHeight",
							"properties"})
public class Button implements AbsoluteSize, ContentSpecifiedWidth, ConstrainableHeight, FormItemWidget {
	private static final long serialVersionUID = -2344473519207771461L;

	private String actionName;
	private Integer pixelWidth;
	private Integer pixelHeight;
	private Integer minPixelHeight;
	private Integer maxPixelHeight;

	@XmlElement(namespace = XMLMetaData.VIEW_NAMESPACE)
	@XmlJavaTypeAdapter(PropertyMapAdapter.class)
	private Map<String, String> properties = new TreeMap<>();

	@Override
	public boolean showsLabelByDefault() {
		return false;
	}
	
	public String getActionName() {
		return actionName;
	}

	@XmlAttribute(required = true)
	public void setActionName(String actionName) {
		this.actionName = UtilImpl.processStringValue(actionName);
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
	public Integer getMaxPixelHeight() {
		return maxPixelHeight;
	}

	@Override
	@XmlAttribute(required = false)
	public void setMaxPixelHeight(Integer maxPixelHeight) {
		this.maxPixelHeight = maxPixelHeight;
	}

	@Override
	public Map<String, String> getProperties() {
		return properties;
	}
}
