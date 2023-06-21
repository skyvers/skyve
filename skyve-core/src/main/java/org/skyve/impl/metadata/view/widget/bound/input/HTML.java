package org.skyve.impl.metadata.view.widget.bound.input;

import java.util.Map;
import java.util.TreeMap;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import org.skyve.impl.metadata.repository.PropertyMapAdapter;
import org.skyve.impl.metadata.view.AbsoluteSize;
import org.skyve.impl.metadata.view.FormItemWidget;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.view.TextOutput.Sanitisation;

@XmlRootElement(namespace = XMLMetaData.VIEW_NAMESPACE)
@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE,
			propOrder = {"pixelWidth", "pixelHeight", "sanitise", "mentionMarkers", "properties"})
public class HTML extends InputWidget implements AbsoluteSize, FormItemWidget {
	private static final long serialVersionUID = -2155059200252882977L;

	private Integer pixelWidth;
	private Integer pixelHeight;
	private Sanitisation sanitise;
	private String mentionMarkers;

	@XmlElement(namespace = XMLMetaData.VIEW_NAMESPACE)
	@XmlJavaTypeAdapter(PropertyMapAdapter.class)
	private Map<String, String> properties = new TreeMap<>();

	@Override
	public boolean showsLabelByDefault() {
		return true;
	}
	
	@Override
	public Integer getPixelWidth() {
		return pixelWidth;
	}

	@Override
	@XmlAttribute(required = true)
	public void setPixelWidth(Integer pixelWidth) {
		this.pixelWidth = pixelWidth;
	}

	@Override
	public Integer getPixelHeight() {
		return pixelHeight;
	}

	@Override
	@XmlAttribute(required = true)
	public void setPixelHeight(Integer pixelHeight) {
		this.pixelHeight = pixelHeight;
	}

	public String getMentionMarkers() {
		return mentionMarkers;
	}

	/**
	 * A list of start (and option end) markers to invoke suggestions within the editor.
	 * @param mentionMarkers A comma separated list of 1 or 2 chars such as '{}' or '@' - eg {},@,#
	 */
	@XmlAttribute
	public void setMentionMarkers(String mentionMarkers) {
		this.mentionMarkers = UtilImpl.processStringValue(mentionMarkers);
	}

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
