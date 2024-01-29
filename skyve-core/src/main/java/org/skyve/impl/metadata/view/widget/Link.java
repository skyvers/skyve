package org.skyve.impl.metadata.view.widget;

import java.util.Map;
import java.util.TreeMap;

import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.repository.PropertyMapAdapter;
import org.skyve.impl.metadata.view.AbsoluteWidth;
import org.skyve.impl.metadata.view.FormItemWidget;
import org.skyve.impl.metadata.view.reference.ActionReference;
import org.skyve.impl.metadata.view.reference.ContentReference;
import org.skyve.impl.metadata.view.reference.DefaultListViewReference;
import org.skyve.impl.metadata.view.reference.EditViewReference;
import org.skyve.impl.metadata.view.reference.ExternalReference;
import org.skyve.impl.metadata.view.reference.ImplicitActionReference;
import org.skyve.impl.metadata.view.reference.QueryListViewReference;
import org.skyve.impl.metadata.view.reference.Reference;
import org.skyve.impl.metadata.view.reference.ReferenceTarget;
import org.skyve.impl.metadata.view.reference.ReportReference;
import org.skyve.impl.metadata.view.reference.ResourceReference;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.view.Invisible;
import org.skyve.util.Util;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlElementRef;
import jakarta.xml.bind.annotation.XmlElementRefs;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;
import jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

/**
 * The link can be used to link to a web page.
 */
@XmlRootElement(namespace = XMLMetaData.VIEW_NAMESPACE)
@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE,
			propOrder = {"reference", 
							"target",
							"value",
							"pixelWidth",
							"invisibleConditionName",
							"visibleConditionName",
							"properties"})
public class Link implements Invisible, AbsoluteWidth, FormItemWidget {
	private static final long serialVersionUID = 2694545058785836920L;

	private Reference reference;
	private ReferenceTarget target;
	private String value; // the title/label/value (not the href) of the link rendered on the UI
	private Integer pixelWidth;
	
	private String invisibleConditionName;
	
	@XmlElement(namespace = XMLMetaData.VIEW_NAMESPACE)
	@XmlJavaTypeAdapter(PropertyMapAdapter.class)
	private Map<String, String> properties = new TreeMap<>();

	@Override
	public boolean showsLabelByDefault() {
		return false;
	}
	
	public Reference getReference() {
		return reference;
	}

	@XmlElementRefs({@XmlElementRef(type = ActionReference.class),
						@XmlElementRef(type = ContentReference.class),
						@XmlElementRef(type = DefaultListViewReference.class),
						@XmlElementRef(type = EditViewReference.class),
						@XmlElementRef(type = ExternalReference.class),
						@XmlElementRef(type = ImplicitActionReference.class),
						@XmlElementRef(type = QueryListViewReference.class),
						@XmlElementRef(type = ReportReference.class),
						@XmlElementRef(type = ResourceReference.class)})
	public void setReference(Reference reference) {
		this.reference = reference;
	}

	public ReferenceTarget getTarget() {
		return target;
	}

	@XmlElement(namespace = XMLMetaData.VIEW_NAMESPACE)
	public void setTarget(ReferenceTarget target) {
		this.target = target;
	}

	public String getValue() {
		return value;
	}

	public String getLocalisedValue() {
		return Util.i18n(value);
	}
	
	@XmlAttribute(required = false)
	public void setValue(String value) {
		this.value = UtilImpl.processStringValue(value);
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

	@Override
	public Map<String, String> getProperties() {
		return properties;
	}
}
