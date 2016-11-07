package org.skyve.impl.metadata.view.widget;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElementRef;
import javax.xml.bind.annotation.XmlElementRefs;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.view.AbsoluteWidth;
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
import org.skyve.metadata.MetaData;
import org.skyve.metadata.view.Invisible;

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
							"visibleConditionName"})
public class Link implements MetaData, Invisible, AbsoluteWidth {
	private static final long serialVersionUID = 2694545058785836920L;

	private Reference reference;
	private ReferenceTarget target;
	private String value; // the title/label/value (not the href) of the link rendered on the UI
	private Integer pixelWidth;
	
	private String invisibleConditionName;
	
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
}
