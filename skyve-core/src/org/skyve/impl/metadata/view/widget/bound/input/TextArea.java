package org.skyve.impl.metadata.view.widget.bound.input;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.metadata.view.AbsoluteSize;
import org.skyve.impl.metadata.view.FormItemWidget;
import org.skyve.impl.metadata.view.MinimumHeight;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.view.Editable;
import org.skyve.impl.metadata.view.widget.bound.input.ChangeableInputWidget;

@XmlRootElement(namespace = XMLMetaData.VIEW_NAMESPACE)
@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE,
			propOrder = {"wordWrap", "editable", "pixelWidth", "pixelHeight", "minPixelHeight"})
public class TextArea extends ChangeableInputWidget implements Editable, AbsoluteSize, MinimumHeight, FormItemWidget {
	private static final long serialVersionUID = 7376326511023184723L;

	private Boolean wordWrap;
	private Boolean editable;
	private Integer pixelWidth;
	private Integer pixelHeight;
	private Integer minPixelHeight;

	@Override
	public boolean showsLabelByDefault() {
		return true;
	}
	
	public Boolean getWordWrap() {
		return wordWrap;
	}

	@XmlAttribute(name = "wrap", required = false)
	public void setWordWrap(Boolean wordWrap) {
		this.wordWrap = wordWrap;
	}

	@Override
	public Boolean getEditable() {
		return editable;
	}

	@Override
	@XmlAttribute(name = "editable", required = false)
	public void setEditable(Boolean editable) {
		this.editable = editable;
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
}
