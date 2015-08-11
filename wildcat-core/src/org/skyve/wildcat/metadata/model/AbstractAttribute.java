package org.skyve.wildcat.metadata.model;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.XmlType;

import org.skyve.domain.Bean;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.document.DomainType;
import org.skyve.wildcat.metadata.view.WidgetReference;
import org.skyve.wildcat.metadata.view.widget.bound.input.CheckBox;
import org.skyve.wildcat.metadata.view.widget.bound.input.ColourPicker;
import org.skyve.wildcat.metadata.view.widget.bound.input.Combo;
import org.skyve.wildcat.metadata.view.widget.bound.input.ContentLink;
import org.skyve.wildcat.metadata.view.widget.bound.input.Geometry;
import org.skyve.wildcat.metadata.view.widget.bound.input.InputWidget;
import org.skyve.wildcat.metadata.view.widget.bound.input.LookupDescription;
import org.skyve.wildcat.metadata.view.widget.bound.input.RichText;
import org.skyve.wildcat.metadata.view.widget.bound.input.TextArea;
import org.skyve.wildcat.metadata.view.widget.bound.input.TextField;
import org.skyve.wildcat.util.UtilImpl;
import org.skyve.wildcat.util.XMLUtil;

@XmlType(namespace = XMLUtil.DOCUMENT_NAMESPACE,
			propOrder = {"documentation",
							"displayName", 
							"description", 
							"defaultWidgetReference",
							"deprecatedBool",
							"trackChangesBool"})
public abstract class AbstractAttribute extends org.skyve.wildcat.metadata.repository.NamedMetaData implements Attribute {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = -6632233770237276819L;

	private String displayName;
	private AttributeType attributeType;
	private String description;
	protected DomainType domainType;
	private InputWidget defaultInputWidget;
	private boolean deprecated;
	private boolean trackChanges = true;
	private String documentation;
	
	@Override
	public String getDisplayName() {
		return displayName;
	}

	@XmlElement(namespace = XMLUtil.DOCUMENT_NAMESPACE, required = true)
	public void setDisplayName(String displayName) {
		this.displayName = UtilImpl.processStringValue(displayName);
	}

	@Override
	public String getDescription() {
		return description;
	}

	@XmlElement(namespace = XMLUtil.DOCUMENT_NAMESPACE)
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
	public InputWidget getDefaultInputWidget() {
		if (defaultInputWidget == null) {
			if ((domainType != null) || (AttributeType.enumeration.equals(attributeType))) {
				defaultInputWidget = new Combo();
				defaultInputWidget.setBinding(getName());
			}
			else if (AttributeType.association.equals(attributeType)) {
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
	
	@XmlElement(namespace = XMLUtil.DOCUMENT_NAMESPACE, name="defaultWidget")
	public void setDefaultWidgetReference(WidgetReference widgetRef) {
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

	@XmlAttribute(name="deprecated", required = false)
	public void setDeprecatedBool(Boolean deprecated) {
		this.deprecated = deprecated.booleanValue();
	}
	
	@Override
	public boolean isTrackChanges() {
		return trackChanges;
	}

	@XmlTransient
	public void setTrackChanges(boolean trackChanges) {
		this.trackChanges = trackChanges;
	}

	@XmlAttribute(name="trackChanges", required = false)
	public void setTrackChangesBool(Boolean trackChanges) {
		this.trackChanges = trackChanges.booleanValue();
	}

	@Override
	public String getDocumentation() {
		return documentation;
	}

	@XmlElement(namespace = XMLUtil.DOCUMENT_NAMESPACE)
	public void setDocumentation(String documentation) {
		this.documentation = UtilImpl.processStringValue(documentation);
	}
}
