package org.skyve.impl.metadata.view.widget.bound.input;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.metadata.view.AbsoluteWidth;
import org.skyve.impl.metadata.view.FormItemWidget;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.impl.metadata.view.widget.bound.input.ChangeableInputWidget;

@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.VIEW_NAMESPACE)
public final class ColourPicker extends ChangeableInputWidget implements AbsoluteWidth, FormItemWidget {
	private static final long serialVersionUID = 873627021901504703L;
	
	private Integer pixelWidth;

	@Override
	public boolean showsLabelByDefault() {
		return true;
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
}
