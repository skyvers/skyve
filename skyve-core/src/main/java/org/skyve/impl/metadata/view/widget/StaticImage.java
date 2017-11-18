package org.skyve.impl.metadata.view.widget;

import java.util.Map;
import java.util.TreeMap;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.repository.PropertyMapAdapter;
import org.skyve.impl.metadata.view.FormItemWidget;
import org.skyve.impl.metadata.view.RelativeSize;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.view.Invisible;

@XmlRootElement(namespace = XMLMetaData.VIEW_NAMESPACE)
@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE,
			propOrder = {"relativeFile", 
						"pixelWidth", 
						"responsiveWidth",
						"percentageWidth",
						"minPixelWidth", 
						"maxPixelWidth", 
						"pixelHeight", 
						"percentageHeight",
						"minPixelHeight", 
						"maxPixelHeight", 
						"invisibleConditionName",
						"visibleConditionName",
						"properties"})
public class StaticImage implements RelativeSize, Invisible, FormItemWidget {
	private static final long serialVersionUID = 314857374179338882L;

	private String relativeFile;
	
	private Integer pixelWidth;
	private Integer responsiveWidth;
	private Integer percentageWidth;
	private Integer minPixelWidth;
	private Integer maxPixelWidth;

	private Integer pixelHeight;
	private Integer percentageHeight;
	private Integer minPixelHeight;
	private Integer maxPixelHeight;
	
	private String invisibleConditionName;

	@XmlElement(namespace = XMLMetaData.VIEW_NAMESPACE)
	@XmlJavaTypeAdapter(PropertyMapAdapter.class)
	private Map<String, String> properties = new TreeMap<>();

	@Override
	public boolean showsLabelByDefault() {
		return false;
	}
	
	public String getRelativeFile() {
		return relativeFile;
	}

	@XmlAttribute(required = false)
	public void setRelativeFile(String relativeFile) {
		this.relativeFile = UtilImpl.processStringValue(relativeFile);
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
	
	@Override
	public Map<String, String> getProperties() {
		return properties;
	}
}
