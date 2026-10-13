package org.skyve.impl.metadata.view.widget;

import java.util.LinkedHashMap;
import java.util.Map;

import org.skyve.impl.metadata.repository.PropertyMapAdapter;
import org.skyve.impl.metadata.view.AbsoluteSize;
import org.skyve.impl.metadata.view.ConstrainableHeight;
import org.skyve.impl.metadata.view.ContentSpecifiedWidth;
import org.skyve.impl.metadata.view.FormItemWidget;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.view.Action.ActionShow;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;
import jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

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
							"show",
							"properties"})
public class Button implements AbsoluteSize, ContentSpecifiedWidth, ConstrainableHeight, FormItemWidget {
	private static final long serialVersionUID = -2344473519207771461L;

	private String actionName;
	private Integer pixelWidth;
	private Integer pixelHeight;
	private Integer minPixelHeight;
	private Integer maxPixelHeight;
	private ActionShow show;

	@XmlElement(namespace = XMLMetaData.VIEW_NAMESPACE)
	@XmlJavaTypeAdapter(PropertyMapAdapter.class)
	private Map<String, String> properties = new LinkedHashMap<>();

	@Override
	public boolean showsLabelByDefault() {
		return false;
	}
	
	public @Nullable String getActionName() {
		return actionName;
	}

	@XmlAttribute(name = "action", required = true)
	public void setActionName(@Nullable String actionName) {
		this.actionName = UtilImpl.processStringValue(actionName);
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
	public @Nullable Integer getMaxPixelHeight() {
		return maxPixelHeight;
	}

	@Override
	@XmlAttribute(required = false)
	public void setMaxPixelHeight(@Nullable Integer maxPixelHeight) {
		this.maxPixelHeight = maxPixelHeight;
	}

	public @Nullable ActionShow getShow() {
		return show;
	}

	@XmlAttribute
	public void setShow(@Nullable ActionShow show) {
		this.show = show;
	}

	@Override
	public @Nonnull Map<String, String> getProperties() {
		return properties;
	}
}
