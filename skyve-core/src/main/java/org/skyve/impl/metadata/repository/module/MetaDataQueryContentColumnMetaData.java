package org.skyve.impl.metadata.repository.module;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;

@XmlRootElement(namespace = XMLMetaData.MODULE_NAMESPACE, name = "content")
@XmlType(namespace = XMLMetaData.MODULE_NAMESPACE, 
			propOrder = {"display", "pixelHeight", "emptyThumbnailRelativeFile"})
public class MetaDataQueryContentColumnMetaData extends MetaDataQueryColumnMetaData {
	private static final long serialVersionUID = 7831641243591117311L;

	@XmlType(namespace = XMLMetaData.MODULE_NAMESPACE)
	public static enum DisplayType {
		thumbnail, link;
	}
	
	private DisplayType display;
	private Integer pixelHeight;
	private String emptyThumbnailRelativeFile;
	
	public DisplayType getDisplay() {
		return display;
	}

	@XmlAttribute(required = true)
	public void setDisplay(DisplayType display) {
		this.display = display;
	}

	public Integer getPixelHeight() {
		return pixelHeight;
	}

	@XmlAttribute
	public void setPixelHeight(Integer pixelHeight) {
		this.pixelHeight = pixelHeight;
	}

	public String getEmptyThumbnailRelativeFile() {
		return emptyThumbnailRelativeFile;
	}

	@XmlAttribute
	public void setEmptyThumbnailRelativeFile(String emptyThumbnailRelativeFile) {
		this.emptyThumbnailRelativeFile = UtilImpl.processStringValue(emptyThumbnailRelativeFile);
	}
}
