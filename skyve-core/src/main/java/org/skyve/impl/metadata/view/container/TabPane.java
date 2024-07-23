package org.skyve.impl.metadata.view.container;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.repository.PropertyMapAdapter;
import org.skyve.impl.metadata.view.Identifiable;
import org.skyve.impl.metadata.view.RelativeSize;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.DecoratedMetaData;
import org.skyve.metadata.view.Disableable;
import org.skyve.metadata.view.Invisible;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;
import jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

@XmlRootElement(namespace = XMLMetaData.VIEW_NAMESPACE)
@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE,
			propOrder = {"widgetId",
							"pixelWidth",
							"responsiveWidth",
							"sm",
							"md",
							"lg",
							"xl",
							"percentageWidth",
							"minPixelWidth", 
							"maxPixelWidth", 
							"pixelHeight",
							"percentageHeight",
							"minPixelHeight", 
							"maxPixelHeight", 
							"disabledConditionName", 
							"enabledConditionName",
							"invisibleConditionName", 
							"visibleConditionName",
							"selectedTabIndexBinding",
							"tabs",
							"properties"})
public final class TabPane implements DecoratedMetaData, Identifiable, RelativeSize, Disableable, Invisible {
	private static final long serialVersionUID = -3490366758123216975L;

	private String widgetId;
	
	private Integer pixelWidth;
	private Integer responsiveWidth;
	private Integer sm;
	private Integer md;
	private Integer lg;
	private Integer xl;
	private Integer percentageWidth;
	private Integer minPixelWidth;
	private Integer maxPixelWidth;

	private Integer pixelHeight;
	private Integer percentageHeight;
	private Integer minPixelHeight;
	private Integer maxPixelHeight;
	
	private String disabledConditionName;
	private String invisibleConditionName;
	private String selectedTabIndexBinding;
	private List<Tab> tabs = new ArrayList<>();

	@XmlElement(namespace = XMLMetaData.VIEW_NAMESPACE)
	@XmlJavaTypeAdapter(PropertyMapAdapter.class)
	private Map<String, String> properties = new TreeMap<>();

	@Override
	public String getWidgetId() {
		return widgetId;
	}

	@XmlAttribute(required = false)
	public void setWidgetId(String widgetId) {
		this.widgetId = UtilImpl.processStringValue(widgetId);
	}

	@XmlElement(namespace = XMLMetaData.VIEW_NAMESPACE, name = "tab", required = true)
	public List<Tab> getTabs() {
		return tabs;
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
	public Integer getResponsiveWidth() {
		return responsiveWidth;
	}

	@Override
	@XmlAttribute(required = false)
	public void setResponsiveWidth(Integer responsiveWidth) {
		this.responsiveWidth = responsiveWidth;
	}

	@Override
	public Integer getSm() {
		return sm;
	}
	
	@Override
	@XmlAttribute(required = false)
	public void setSm(Integer sm) {
		this.sm = sm;
	}

	@Override
	public Integer getMd() {
		return md;
	}
	
	@Override
	@XmlAttribute(required = false)
	public void setMd(Integer md) {
		this.md = md;
	}
	
	@Override
	public Integer getLg() {
		return lg;
	}
	
	@Override
	@XmlAttribute(required = false)
	public void setLg(Integer lg) {
		this.lg = lg;
	}

	@Override
	public Integer getXl() {
		return xl;
	}
	
	@Override
	@XmlAttribute(required = false)
	public void setXl(Integer xl) {
		this.xl = xl;
	}

	@Override
	public Integer getPercentageWidth() {
		return percentageWidth;
	}

	@Override
	@XmlAttribute(required = false)
	public void setPercentageWidth(Integer percentageWidth) {
		this.percentageWidth = percentageWidth;
	}

	@Override
	public Integer getMinPixelWidth() {
		return minPixelWidth;
	}

	@Override
	@XmlAttribute(required = false)
	public void setMinPixelWidth(Integer minPixelWidth) {
		this.minPixelWidth = minPixelWidth;
	}

	@Override
	public Integer getMaxPixelWidth() {
		return maxPixelWidth;
	}

	@Override
	@XmlAttribute(required = false)
	public void setMaxPixelWidth(Integer maxPixelWidth) {
		this.maxPixelWidth = maxPixelWidth;
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
	public Integer getPercentageHeight() {
		return percentageHeight;
	}

	@Override
	@XmlAttribute(required = false)
	public void setPercentageHeight(Integer percentageHeight) {
		this.percentageHeight = percentageHeight;
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
	public String getDisabledConditionName() {
		return disabledConditionName;
	}

	@Override
	@XmlAttribute(name = "disabled", required = false)
	public void setDisabledConditionName(String disabledConditionName) {
		this.disabledConditionName = disabledConditionName;
	}

	// to enable JAXB XML marshaling
	@SuppressWarnings("static-method")
	String getEnabledConditionName() {
		return null;
	}

	@Override
	@XmlAttribute(name = "enabled", required = false)
	public void setEnabledConditionName(String enabledConditionName) {
		this.disabledConditionName = BindUtil.negateCondition(UtilImpl.processStringValue(enabledConditionName));
	}

	@Override
	public String getInvisibleConditionName() {
		return invisibleConditionName;
	}

	@Override
	@XmlAttribute(name = "invisible", required = false)
	public void setInvisibleConditionName(String invisibleConditionName) {
		this.invisibleConditionName = invisibleConditionName;
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

	public String getSelectedTabIndexBinding() {
		return selectedTabIndexBinding;
	}

	@XmlAttribute(required = false)
	public void setSelectedTabIndexBinding(String selectedTabIndexBinding) {
		this.selectedTabIndexBinding = UtilImpl.processStringValue(selectedTabIndexBinding);
	}

	@Override
	public Map<String, String> getProperties() {
		return properties;
	}
}
