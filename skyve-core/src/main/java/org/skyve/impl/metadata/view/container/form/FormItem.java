package org.skyve.impl.metadata.view.container.form;

import java.util.Map;
import java.util.TreeMap;

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
import org.skyve.impl.metadata.view.widget.bound.ZoomIn;
import org.skyve.impl.metadata.view.widget.bound.input.CheckBox;
import org.skyve.impl.metadata.view.widget.bound.input.ColourPicker;
import org.skyve.impl.metadata.view.widget.bound.input.Combo;
import org.skyve.impl.metadata.view.widget.bound.input.ContentSignature;
import org.skyve.impl.metadata.view.widget.bound.input.ContentUpload;
import org.skyve.impl.metadata.view.widget.bound.input.DefaultWidget;
import org.skyve.impl.metadata.view.widget.bound.input.Geometry;
import org.skyve.impl.metadata.view.widget.bound.input.GeometryMap;
import org.skyve.impl.metadata.view.widget.bound.input.HTML;
import org.skyve.impl.metadata.view.widget.bound.input.LookupDescription;
import org.skyve.impl.metadata.view.widget.bound.input.Password;
import org.skyve.impl.metadata.view.widget.bound.input.Radio;
import org.skyve.impl.metadata.view.widget.bound.input.RichText;
import org.skyve.impl.metadata.view.widget.bound.input.Slider;
import org.skyve.impl.metadata.view.widget.bound.input.Spinner;
import org.skyve.impl.metadata.view.widget.bound.input.TextArea;
import org.skyve.impl.metadata.view.widget.bound.input.TextField;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.DecoratedMetaData;
import org.skyve.metadata.MetaData;
import org.skyve.util.Util;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlElementRef;
import jakarta.xml.bind.annotation.XmlElementRefs;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;
import jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

/**
 * JAXB-annotated descriptor for a single labelled field entry within a
 * {@link FormRow}.
 *
 * <p>Pairs a label with a widget (input field, static text, etc.) and carries
 * spanning, alignment, and visibility settings for its cell in the form grid.
 *
 * <p>Threading: not thread-safe.  Read-only after JAXB unmarshalling.
 *
 * @see FormRow
 * @see Form
 */
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
	// override the required message to be different from the attribute
	private String requiredMessage;
	
	@XmlElement(namespace = XMLMetaData.VIEW_NAMESPACE)
	@XmlJavaTypeAdapter(PropertyMapAdapter.class)
	private Map<String, String> properties = new TreeMap<>();

	/**
	 * Returns the configured widget metadata for this form cell.
	 *
	 * @return the widget descriptor, or {@code null} when not configured
	 */
	public MetaData getWidget() {
		return widget;
	}
	
	/**
	 * Sets the widget metadata for this form cell.
	 *
	 * <p>Only one widget subtype from the declared JAXB element-ref set is valid.
	 *
	 * @param widget  the widget descriptor to render in this cell; may be {@code null}
	 */
	@XmlElementRefs({@XmlElementRef(type = DefaultWidget.class),
						@XmlElementRef(type = ContentUpload.class),
						@XmlElementRef(type = ContentSignature.class),
						@XmlElementRef(type = Button.class),
						@XmlElementRef(type = ZoomIn.class),
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
	
	/**
	 * Returns how many form columns this item spans.
	 *
	 * @return column span, or {@code null} when default spanning is used
	 */
	public Integer getColspan() {
		return colspan;
	}

	/**
	 * Sets how many form columns this item spans.
	 *
	 * @param colspan  column span value; may be {@code null}
	 */
	@XmlAttribute(required = false)
	public void setColspan(Integer colspan) {
		this.colspan = colspan;
	}
	
	/**
	 * Returns how many form rows this item spans.
	 *
	 * @return row span, or {@code null} when default spanning is used
	 */
	public Integer getRowspan() {
		return rowspan;
	}

	/**
	 * Sets how many form rows this item spans.
	 *
	 * @param rowspan  row span value; may be {@code null}
	 */
	@XmlAttribute(required = false)
	public void setRowspan(Integer rowspan) {
		this.rowspan = rowspan;
	}

	/**
	 * Returns the horizontal alignment for this item's widget cell.
	 *
	 * @return widget-cell alignment, or {@code null} when renderer defaults are used
	 */
	public HorizontalAlignment getHorizontalAlignment() {
		return horizontalAlignment;
	}

	/**
	 * Sets the horizontal alignment for this item's widget cell.
	 *
	 * @param horizontalAlignment  widget-cell alignment to apply; may be {@code null}
	 */
	@XmlAttribute(name = "align", required = false)
	public void setHorizontalAlignment(HorizontalAlignment horizontalAlignment) {
		this.horizontalAlignment = horizontalAlignment;
	}

	/**
	 * Returns the horizontal alignment for this item's label cell.
	 *
	 * @return label-cell alignment, or {@code null} when default label alignment is used
	 */
	@XmlAttribute(name = "labelAlign", required = false)
	public HorizontalAlignment getLabelHorizontalAlignment() {
		return labelHorizontalAlignment;
	}

	/**
	 * Sets the horizontal alignment for this item's label cell.
	 *
	 * @param labelHorizontalAlignment  label-cell alignment to apply; may be {@code null}
	 */
	public void setLabelHorizontalAlignment(
			HorizontalAlignment labelHorizontalAlignment) {
		this.labelHorizontalAlignment = labelHorizontalAlignment;
	}

	/**
	 * Returns the label override text for this item.
	 *
	 * @return label override text, or {@code null} when no override is configured
	 */
	public String getLabel() {
		return label;
	}

	/**
	 * Returns the field label translated for the current locale.
	 *
	 * @return the localised label, or {@code null} when no label override is configured
	 */
	public String getLocalisedLabel() {
		return Util.i18n(label);
	}
	
	/**
	 * Sets the label override text for this item.
	 *
	 * <p>Side effects: trims and normalises the supplied value before storage.
	 *
	 * @param label  label override text; may be {@code null} or blank
	 */
	@XmlAttribute(required = false)
	public void setLabel(String label) {
		this.label = UtilImpl.processStringValue(label);
	}

	/**
	 * Returns whether the label should be rendered.
	 *
	 * @return {@code true} to render the label, {@code false} to hide it, or {@code null} to use defaults
	 */
	public Boolean getShowLabel() {
		return showLabel;
	}

	/**
	 * Sets whether the label should be rendered.
	 *
	 * @param showLabel  label visibility flag; may be {@code null}
	 */
	@XmlAttribute(required = false)
	public void setShowLabel(Boolean showLabel) {
		this.showLabel = showLabel;
	}

	/**
	 * Returns whether help text should be rendered.
	 *
	 * @return {@code true} to render help, {@code false} to hide it, or {@code null} to use defaults
	 */
	public Boolean getShowHelp() {
		return showHelp;
	}

	/**
	 * Sets whether help text should be rendered.
	 *
	 * @param showHelp  help visibility flag; may be {@code null}
	 */
	@XmlAttribute(required = false)
	public void setShowHelp(Boolean showHelp) {
		this.showHelp = showHelp;
	}
	
	/**
	 * Returns the help-text override for this item.
	 *
	 * @return help-text override, or {@code null} when no override is configured
	 */
	public String getHelp() {
		return help;
	}

	/**
	 * Returns the help text translated for the current locale.
	 *
	 * @return the localised help text, or {@code null} when no help override is configured
	 */
	public String getLocalisedHelp() {
		return Util.i18n(help);
	}
	
	/**
	 * Sets the help-text override for this item.
	 *
	 * <p>Side effects: trims and normalises the supplied value before storage.
	 *
	 * @param help  help-text override; may be {@code null} or blank
	 */
	@XmlAttribute(required = false)
	public void setHelp(String help) {
		this.help = UtilImpl.processStringValue(help);
	}

	/**
	 * Returns whether this item should be treated as required.
	 *
	 * @return {@code true} when required, {@code false} when optional, or {@code null} to inherit defaults
	 */
	public Boolean getRequired() {
		return required;
	}

	/**
	 * Sets whether this item should be treated as required.
	 *
	 * @param required  required flag; may be {@code null}
	 */
	@XmlAttribute(required = false)
	public void setRequired(Boolean required) {
		this.required = required;
	}

	/**
	 * Returns the required-message override text.
	 *
	 * @return required-message override text, or {@code null} when no override is configured
	 */
	public String getRequiredMessage() {
		return requiredMessage;
	}

	/**
	 * Returns the required-message override translated for the current locale.
	 *
	 * @return the localised required message, or {@code null} when no override is configured
	 */
	public String getLocalisedRequiredMessage() {
		return Util.i18n(requiredMessage);
	}

	/**
	 * Sets the required-message override text.
	 *
	 * <p>Side effects: trims and normalises the supplied value before storage.
	 *
	 * @param requiredMessage  required-message override text; may be {@code null} or blank
	 */
	@XmlAttribute(required = false)
	public void setRequiredMessage(String requiredMessage) {
		this.requiredMessage = UtilImpl.processStringValue(requiredMessage);
	}

	/**
	 * Returns the decorator property map for this form item.
	 *
	 * @return a mutable property map; never {@code null}
	 */
	@Override
	public Map<String, String> getProperties() {
		return properties;
	}
}
