package org.skyve.impl.metadata.model;

import org.skyve.domain.Bean;
import org.skyve.impl.domain.types.jaxb.CDATAAdapter;
import org.skyve.impl.metadata.repository.NamedMetaData;
import org.skyve.impl.metadata.view.WidgetReference;
import org.skyve.impl.metadata.view.widget.bound.input.CheckBox;
import org.skyve.impl.metadata.view.widget.bound.input.ColourPicker;
import org.skyve.impl.metadata.view.widget.bound.input.Combo;
import org.skyve.impl.metadata.view.widget.bound.input.ContentImage;
import org.skyve.impl.metadata.view.widget.bound.input.ContentLink;
import org.skyve.impl.metadata.view.widget.bound.input.Geometry;
import org.skyve.impl.metadata.view.widget.bound.input.InputWidget;
import org.skyve.impl.metadata.view.widget.bound.input.LookupDescription;
import org.skyve.impl.metadata.view.widget.bound.input.RichText;
import org.skyve.impl.metadata.view.widget.bound.input.TextArea;
import org.skyve.impl.metadata.view.widget.bound.input.TextField;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.document.DomainType;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlTransient;
import jakarta.xml.bind.annotation.XmlType;
import jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

@XmlType(namespace = XMLMetaData.DOCUMENT_NAMESPACE,
			propOrder = {"documentation",
							"displayName", 
							"description", 
							"defaultWidgetReference",
							"deprecatedBool",
							"trackChangesBool",
							"auditedBool",
							"transientBool"})
public abstract class AbstractAttribute extends NamedMetaData implements Attribute {
	private static final long serialVersionUID = -6632233770237276819L;

	private String displayName;
	private AttributeType attributeType;
	private UsageType usage;
	private String description;
	protected DomainType domainType;
	private InputWidget defaultInputWidget;
	private WidgetReference defaultWidgetReference;
	private boolean deprecated;
	private Boolean trackChanges;
	private boolean audited = true;
	private Boolean auditedBool = null;
	private boolean tranzient;
	private String documentation;
	
	@Override
	public String getDisplayName() {
		return displayName;
	}

	@XmlElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE, required = true)
	public void setDisplayName(String displayName) {
		this.displayName = UtilImpl.processStringValue(displayName);
	}

	@Override
	public String getDescription() {
		return description;
	}

	@XmlElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
	@XmlJavaTypeAdapter(CDATAAdapter.class)
	public void setDescription(String description) {
		this.description = UtilImpl.processStringValue(description);
	}

	@Override
	public AttributeType getAttributeType() {
		return attributeType;
	}

	@XmlTransient
	// only used by references which don't need this property
	public void setAttributeType(AttributeType fieldType) {
		this.attributeType = fieldType;
	}

	@Override
	public UsageType getUsage() {
		return usage;
	}
	
	@XmlAttribute
	public void setUsage(UsageType usage) {
		this.usage = usage;
	}
	
	@Override
	public InputWidget getDefaultInputWidget() {
		if (defaultInputWidget == null) {
			if ((domainType != null) || (AttributeType.enumeration.equals(attributeType))) {
				defaultInputWidget = new Combo();
				defaultInputWidget.setBinding(getName());
			}
			else if (AttributeType.association.equals(attributeType) || 
						AttributeType.inverseOne.equals(attributeType)) {
				LookupDescription lookup = new LookupDescription();
				lookup.setBinding(getName());
				lookup.setDescriptionBinding(Bean.BIZ_KEY);
				defaultInputWidget = lookup;
			}
			else if (AttributeType.bool.equals(attributeType)) {
				defaultInputWidget = new CheckBox();
				defaultInputWidget.setBinding(getName());
			}
			else if (AttributeType.memo.equals(attributeType)) {
				defaultInputWidget = new TextArea();
				defaultInputWidget.setBinding(getName());
			}
			else if (AttributeType.markup.equals(attributeType)) {
				defaultInputWidget = new RichText();
				defaultInputWidget.setBinding(getName());
			}
			else if (AttributeType.colour.equals(attributeType)) {
				defaultInputWidget = new ColourPicker();
				defaultInputWidget.setBinding(getName());
			}
			else if (AttributeType.content.equals(attributeType)) {
				defaultInputWidget = new ContentLink();
				defaultInputWidget.setBinding(getName());
			}
			else if (AttributeType.image.equals(attributeType)) {
				defaultInputWidget = new ContentImage();
				defaultInputWidget.setBinding(getName());
			}
			else if (AttributeType.geometry.equals(attributeType)) {
				defaultInputWidget = new Geometry();
				defaultInputWidget.setBinding(getName());
			}
			else {
				defaultInputWidget = new TextField();
				defaultInputWidget.setBinding(getName());
			}
		}
		
		return defaultInputWidget;
	}

	@XmlTransient
	public void setDefaultInputWidget(InputWidget defaultInputWidget) {
		this.defaultInputWidget = defaultInputWidget;
	}
	
	public WidgetReference getDefaultWidgetReference() {
		return defaultWidgetReference;
	}

	@XmlElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE, name="defaultWidget")
	public void setDefaultWidgetReference(WidgetReference widgetRef) {
		this.defaultWidgetReference = widgetRef;

		if (widgetRef != null) {
			defaultInputWidget = widgetRef.getWidget();
			defaultInputWidget.setBinding(getName());
		}
	}

	@Override
	public boolean isDeprecated() {
		return deprecated;
	}

	@XmlTransient
	public void setDeprecated(boolean deprecated) {
		this.deprecated = deprecated;
	}

	public Boolean getDeprecatedBool() {
		return Boolean.valueOf(deprecated);
	}

	@XmlAttribute(name="deprecated", required = false)
	public void setDeprecatedBool(Boolean deprecated) {
		this.deprecated = deprecated.booleanValue();
	}
	
	@Override
	public boolean isTrackChanges() {
		if (trackChanges != null) {
			return trackChanges.booleanValue();
		}

		return ((! AttributeType.inverseOne.equals(attributeType)) && (! AttributeType.inverseMany.equals(attributeType)));
	}

	@XmlTransient
	public void setTrackChanges(boolean trackChanges) {
		this.trackChanges = Boolean.valueOf(trackChanges);
	}

	public Boolean getTrackChangesBool() {
		return trackChanges;
	}
	
	@XmlAttribute(name="trackChanges", required = false)
	public void setTrackChangesBool(Boolean trackChanges) {
		this.trackChanges = trackChanges;
	}

	@Override
	public boolean isAudited() {
		return audited;
	}

	@XmlTransient
	public void setAudited(boolean audited) {
		this.audited = audited;
	}

	public Boolean getAuditedBool() {
		return auditedBool;
	}
	
	@XmlAttribute(name="audited", required = false)
	public void setAuditedBool(Boolean audited) {
		this.auditedBool = audited;
		this.audited = audited.booleanValue();
	}

	@Override
	public boolean isTransient() {
		return tranzient;
	}

	@XmlTransient
	public void setTransient(boolean value) {
		tranzient = value;
	}

	public Boolean getTransientBool() {
		return Boolean.valueOf(tranzient);
	}

	@XmlElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE, name="transient", required = false)
	public void setTransientBool(Boolean value) {
		tranzient = value.booleanValue();
	}

	@Override
	public String getDocumentation() {
		return documentation;
	}

	@XmlElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
	@XmlJavaTypeAdapter(CDATAAdapter.class)
	public void setDocumentation(String documentation) {
		this.documentation = UtilImpl.processStringValue(documentation);
	}
}
