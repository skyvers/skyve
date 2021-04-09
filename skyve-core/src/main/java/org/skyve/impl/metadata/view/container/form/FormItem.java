package org.skyve.impl.metadata.view.container.form;

import java.util.Map;
import java.util.TreeMap;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElementRef;
import javax.xml.bind.annotation.XmlElementRefs;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import org.skyve.impl.metadata.repository.PropertyMapAdapter;
import org.skyve.impl.metadata.view.HorizontalAlignment;
import org.skyve.impl.metadata.view.Inject;
import org.skyve.impl.metadata.view.widget.Blurb;
import org.skyve.impl.metadata.view.widget.Button;
import org.skyve.impl.metadata.view.widget.DialogButton;
import org.skyve.impl.metadata.view.widget.Link;
import org.skyve.impl.metadata.view.widget.Spacer;
import org.skyve.impl.metadata.view.widget.StaticImage;
import org.skyve.impl.metadata.view.widget.bound.Label;
import org.skyve.impl.metadata.view.widget.bound.ProgressBar;
import org.skyve.impl.metadata.view.widget.bound.input.CheckBox;
import org.skyve.impl.metadata.view.widget.bound.input.ColourPicker;
import org.skyve.impl.metadata.view.widget.bound.input.Combo;
import org.skyve.impl.metadata.view.widget.bound.input.ContentImage;
import org.skyve.impl.metadata.view.widget.bound.input.ContentLink;
import org.skyve.impl.metadata.view.widget.bound.input.DefaultWidget;
import org.skyve.impl.metadata.view.widget.bound.input.Geometry;
import org.skyve.impl.metadata.view.widget.bound.input.GeometryMap;
import org.skyve.impl.metadata.view.widget.bound.input.HTML;
import org.skyve.impl.metadata.view.widget.bound.input.Lookup;
import org.skyve.impl.metadata.view.widget.bound.input.LookupDescription;
import org.skyve.impl.metadata.view.widget.bound.input.Password;
import org.skyve.impl.metadata.view.widget.bound.input.Radio;
import org.skyve.impl.metadata.view.widget.bound.input.RichText;
import org.skyve.impl.metadata.view.widget.bound.input.Slider;
import org.skyve.impl.metadata.view.widget.bound.input.Spinner;
import org.skyve.impl.metadata.view.widget.bound.input.TextArea;
import org.skyve.impl.metadata.view.widget.bound.input.TextField;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.DecoratedMetaData;
import org.skyve.metadata.MetaData;
import org.skyve.util.Util;

@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.VIEW_NAMESPACE, name = "item")
public class FormItem implements DecoratedMetaData {
	private static final long serialVersionUID = 8914657809022150728L;

	private MetaData widget;
	private Integer colspan;
	private Integer rowspan;
	private HorizontalAlignment horizontalAlignment;
	// label override (based on binding on widget)
	private String label;
	// is the label rendered?
	private Boolean showLabel;
	private HorizontalAlignment labelHorizontalAlignment;
	// is the help rendered?
	private Boolean showHelp;
	// override help to be different from an attribute's description
	private String help;
	// is the field required or not - regardless of whether its required in data
	private Boolean required;
	
	@XmlElement(namespace = XMLMetaData.VIEW_NAMESPACE)
	@XmlJavaTypeAdapter(PropertyMapAdapter.class)
	private Map<String, String> properties = new TreeMap<>();

	public MetaData getWidget() {
		return widget;
	}
	
	@XmlElementRefs({@XmlElementRef(type = DefaultWidget.class),
						@XmlElementRef(type = ContentImage.class),
						@XmlElementRef(type = ContentLink.class), 
						@XmlElementRef(type = Button.class),
						@XmlElementRef(type = DialogButton.class),
						@XmlElementRef(type = Geometry.class),
						@XmlElementRef(type = GeometryMap.class),
						@XmlElementRef(type = HTML.class),
						@XmlElementRef(type = Label.class),
						@XmlElementRef(type = Blurb.class),
						@XmlElementRef(type = ProgressBar.class), 
						@XmlElementRef(type = CheckBox.class),
						@XmlElementRef(type = ColourPicker.class), 
						@XmlElementRef(type = Combo.class), 
						@XmlElementRef(type = Radio.class),
						@XmlElementRef(type = Lookup.class), 
						@XmlElementRef(type = LookupDescription.class),
						@XmlElementRef(type = Password.class), 
						@XmlElementRef(type = RichText.class), 
						@XmlElementRef(type = Slider.class),
						@XmlElementRef(type = Spacer.class),
						@XmlElementRef(type = Spinner.class),
						@XmlElementRef(type = StaticImage.class),
						@XmlElementRef(type = Link.class),
						@XmlElementRef(type = TextArea.class),
						@XmlElementRef(type = TextField.class),
						@XmlElementRef(type = Inject.class)})
	public void setWidget(MetaData widget) {
		this.widget = widget;
	}
	
	public Integer getColspan() {
		return colspan;
	}

	@XmlAttribute(required = false)
	public void setColspan(Integer colspan) {
		this.colspan = colspan;
	}
	
	public Integer getRowspan() {
		return rowspan;
	}

	@XmlAttribute(required = false)
	public void setRowspan(Integer rowspan) {
		this.rowspan = rowspan;
	}

	public HorizontalAlignment getHorizontalAlignment() {
		return horizontalAlignment;
	}

	@XmlAttribute(name = "align", required = false)
	public void setHorizontalAlignment(HorizontalAlignment horizontalAlignment) {
		this.horizontalAlignment = horizontalAlignment;
	}

	@XmlAttribute(name = "labelAlign", required = false)
	public HorizontalAlignment getLabelHorizontalAlignment() {
		return labelHorizontalAlignment;
	}

	public void setLabelHorizontalAlignment(
			HorizontalAlignment labelHorizontalAlignment) {
		this.labelHorizontalAlignment = labelHorizontalAlignment;
	}

	public String getLabel() {
		return label;
	}

	public String getLocalisedLabel() {
		return Util.i18n(label);
	}
	
	@XmlAttribute(required = false)
	public void setLabel(String label) {
		this.label = label;
	}

	public Boolean getShowLabel() {
		return showLabel;
	}

	@XmlAttribute(required = false)
	public void setShowLabel(Boolean showLabel) {
		this.showLabel = showLabel;
	}

	public Boolean getShowHelp() {
		return showHelp;
	}

	@XmlAttribute(required = false)
	public void setShowHelp(Boolean showHelp) {
		this.showHelp = showHelp;
	}
	
	public String getHelp() {
		return help;
	}

	public String getLocalisedHelp() {
		return Util.i18n(help);
	}
	
	@XmlAttribute(required = false)
	public void setHelp(String help) {
		this.help = help;
	}

	public Boolean getRequired() {
		return required;
	}

	@XmlAttribute(required = false)
	public void setRequired(Boolean required) {
		this.required = required;
	}

	@Override
	public Map<String, String> getProperties() {
		return properties;
	}
}
