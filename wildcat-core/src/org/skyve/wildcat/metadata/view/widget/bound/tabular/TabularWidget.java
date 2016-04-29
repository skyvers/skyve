package org.skyve.wildcat.metadata.view.widget.bound.tabular;

import java.util.List;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlType;

import org.skyve.metadata.view.Invisible;
import org.skyve.wildcat.bind.BindUtil;
import org.skyve.wildcat.metadata.view.RelativeSize;
import org.skyve.wildcat.metadata.view.widget.bound.AbstractBound;
import org.skyve.wildcat.util.UtilImpl;
import org.skyve.wildcat.util.XMLUtil;

@XmlType(namespace = XMLUtil.VIEW_NAMESPACE,
			propOrder = {"title", 
							"pixelWidth",
							"percentageWidth",
							"minPixelWidth", 
							"maxPixelWidth", 
							"pixelHeight",
							"percentageHeight",
							"minPixelHeight", 
							"maxPixelHeight", 
							"invisibleConditionName",
							"visibleConditionName"})
public abstract class TabularWidget extends AbstractBound implements RelativeSize, Invisible {
	private static final long serialVersionUID = 8143928198512212919L;

	private String title;
	
	private Integer pixelWidth;
	private Integer percentageWidth;
	private Integer minPixelWidth;
	private Integer maxPixelWidth;

	private Integer pixelHeight;
	private Integer percentageHeight;
	private Integer minPixelHeight;
	private Integer maxPixelHeight;
	
	private String invisibleConditionName;

	public abstract List<? extends TabularColumn> getColumns();

	public String getTitle() {
		return title;
	}

	@XmlAttribute(required = false)
	public void setTitle(String title) {
		this.title = UtilImpl.processStringValue(title);
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
}
