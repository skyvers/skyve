package org.skyve.impl.metadata.view.widget.bound.tabular;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.metadata.view.HorizontalAlignment;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.impl.metadata.view.widget.bound.tabular.TabularColumn;

@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE, propOrder = {"title", "alignment", "pixelWidth"})
public abstract class DataGridColumn implements TabularColumn {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = -5532364729219436008L;

	private String title;
	private HorizontalAlignment alignment;
	private Integer pixelWidth;

	@Override
	public String getTitle() {
		return title;
	}

	@Override
	@XmlAttribute
	public void setTitle(String title) {
		this.title = UtilImpl.processStringValue(title);
	}

	@Override
	public HorizontalAlignment getAlignment() {
		return alignment;
	}

	@Override
	@XmlAttribute
	public void setAlignment(HorizontalAlignment alignment) {
		this.alignment = alignment;
	}

	public Integer getPixelWidth() {
		return pixelWidth;
	}

	@XmlAttribute
	public void setPixelWidth(Integer pixelWidth) {
		this.pixelWidth = pixelWidth;
	}
}
