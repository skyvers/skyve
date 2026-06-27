package org.skyve.impl.metadata.view.widget.bound;

import java.util.Map;
import java.util.TreeMap;

import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.repository.PropertyMapAdapter;
import org.skyve.impl.metadata.view.AbsoluteSize;
import org.skyve.impl.metadata.view.ContentSpecifiedWidth;
import org.skyve.impl.metadata.view.FormItemWidget;
import org.skyve.impl.metadata.view.HorizontalAlignment;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.view.Invisible;
import org.skyve.metadata.view.TextOutput;
import org.skyve.util.Util;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;
import jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

/**
 * If a label width/height is not specified, it sizes to fit its contents.
 * 
 * @author mike
 */
@XmlRootElement(namespace = XMLMetaData.VIEW_NAMESPACE)
@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE,
			propOrder = {"value", 
						"for", 
						"pixelWidth", 
						"pixelHeight", 
						"invisibleConditionName", 
						"visibleConditionName",
						"formatted",
						"escape",
						"sanitise",
						"properties"})
public class Label extends AbstractBound implements Invisible, AbsoluteSize, ContentSpecifiedWidth, FormItemWidget, TextOutput {
	private static final long serialVersionUID = -1713640318580531970L;

	/**
	 * A literal value to display in the label
	 */
	private String value;

	/**
	 * The binding to a document attribute to display the Display Name of.
	 */
	private String forBinding;

	private Integer pixelWidth;
	private Integer pixelHeight;
	
	private String invisibleConditionName;

	/**
	 * Keep the carriage returns etc in the label value. 
	 * This is useful for when a memo field needs to be displayed.
	 */
	private Boolean formatted = Boolean.FALSE;

	/**
	 * Default alignment is left.
	 */
	private HorizontalAlignment textAlignment = null;
	
	private Boolean escape;
	private Sanitisation sanitise;

	@XmlElement(namespace = XMLMetaData.VIEW_NAMESPACE)
	@XmlJavaTypeAdapter(PropertyMapAdapter.class)
	private Map<String, String> properties = new TreeMap<>();

	/**
	 * Indicates labels do not render an additional field label by default.
	 *
	 * @return {@code false} because label widgets render their own output text
	 */
	@Override
	public boolean showsLabelByDefault() {
		return false;
	}
	
	/**
	 * Returns the configured literal label value.
	 *
	 * @return the raw label value expression
	 */
	public String getValue() {
		return value;
	}
	
	/**
	 * Returns the label value localised for the current user locale.
	 *
	 * @return the localised label value
	 */
	public String getLocalisedValue() {
		return Util.i18n(value);
	}

	/**
	 * Sets the literal label value after trimming and empty-string normalisation.
	 *
	 * @param value the label value expression
	 */
	@XmlAttribute(required = false)
	public void setValue(String value) {
		this.value = UtilImpl.processStringValue(value);
	}

	/**
	 * Returns the binding used to resolve the target attribute display name.
	 *
	 * @return the target binding expression
	 */
	public String getFor() {
		return forBinding;
	}

	/**
	 * Sets the target binding used by the {@code for} attribute.
	 *
	 * @param forBinding the target binding expression
	 */
	@XmlAttribute(name = "for", required = false)
	public void setFor(String forBinding) {
		this.forBinding = UtilImpl.processStringValue(forBinding);
	}

	/**
	 * Returns the absolute pixel width, or {@code null} when auto-sized.
	 *
	 * @return the configured pixel width
	 */
	@Override
	public Integer getPixelWidth() {
		return pixelWidth;
	}

	/**
	 * Sets the absolute pixel width.
	 *
	 * @param pixelWidth the width in pixels
	 */
	@Override
	@XmlAttribute(required = false)
	public void setPixelWidth(Integer pixelWidth) {
		this.pixelWidth = pixelWidth;
	}

	/**
	 * Returns the absolute pixel height, or {@code null} when auto-sized.
	 *
	 * @return the configured pixel height
	 */
	@Override
	public Integer getPixelHeight() {
		return pixelHeight;
	}

	/**
	 * Sets the absolute pixel height.
	 *
	 * @param pixelHeight the height in pixels
	 */
	@Override
	@XmlAttribute(required = false)
	public void setPixelHeight(Integer pixelHeight) {
		this.pixelHeight = pixelHeight;
	}

	/**
	 * Returns the internal invisible condition expression.
	 *
	 * @return the invisible condition expression
	 */
	@Override
	public String getInvisibleConditionName() {
		return invisibleConditionName;
	}

	/**
	 * Sets the invisible condition expression directly.
	 *
	 * @param invisibleConditionName the invisible condition expression
	 */
	@Override
	@XmlAttribute(name = "invisible", required = false)
	public void setInvisibleConditionName(String invisibleConditionName) {
		this.invisibleConditionName = UtilImpl.processStringValue(invisibleConditionName);
	}

	/**
	 * JAXB-only placeholder for {@code visible}; value is derived via {@link #setVisibleConditionName(String)}.
	 *
	 * @return always {@code null}; JAXB writes via {@code setVisibleConditionName}
	 */
	// to enable JAXB XML marshaling
	@SuppressWarnings("static-method")
	String getVisibleConditionName() {
		return null;
	}

	/**
	 * Sets the visible condition by storing its negation as the internal invisible condition.
	 *
	 * @param visibleConditionName a condition expression that enables visibility
	 */
	@Override
	@XmlAttribute(name = "visible", required = false)
	public void setVisibleConditionName(String visibleConditionName) {
		this.invisibleConditionName = BindUtil.negateCondition(UtilImpl.processStringValue(visibleConditionName));
	}

	/**
	 * Returns whether line breaks and spacing are preserved in rendered output.
	 *
	 * @return {@code true} when formatted output is enabled
	 */
	public Boolean getFormatted() {
		return formatted;
	}

	/**
	 * Sets whether line breaks and spacing are preserved in rendered output.
	 *
	 * @param formatted {@code true} to preserve formatting
	 */
	@XmlAttribute(name = "formatted", required = false)
	public void setFormatted(Boolean formatted) {
		this.formatted = formatted;
	}

	/**
	 * Returns the configured horizontal text alignment.
	 *
	 * @return the text alignment, or {@code null} for renderer default
	 */
	public HorizontalAlignment getTextAlignment() {
		return textAlignment;
	}

	/**
	 * Sets the horizontal text alignment.
	 *
	 * @param textAlignment the alignment to apply
	 */
	@XmlAttribute(name = "textAlignment", required = false)
	public void setTextAlignment(HorizontalAlignment textAlignment) {
		this.textAlignment = textAlignment;
	}

	/**
	 * Returns whether output should be HTML-escaped.
	 *
	 * @return {@code true} when escaping is enabled
	 */
	@Override
	public Boolean getEscape() {
		return escape;
	}

	/**
	 * Sets whether output should be HTML-escaped.
	 *
	 * @param escape {@code true} to escape output
	 */
	@XmlAttribute
	public void setEscape(Boolean escape) {
		this.escape = escape;
	}

	/**
	 * Returns the output sanitisation strategy.
	 *
	 * @return the sanitisation strategy, or {@code null} when default behavior applies
	 */
	@Override
	public Sanitisation getSanitise() {
		return sanitise;
	}

	/**
	 * Sets the output sanitisation strategy.
	 *
	 * @param sanitise the sanitisation strategy to apply
	 */
	@XmlAttribute
	public void setSanitise(Sanitisation sanitise) {
		this.sanitise = sanitise;
	}

	/**
	 * Returns the mutable decorator property map for this label widget.
	 *
	 * @return mutable decorator property map keyed by property name
	 */
	@Override
	public Map<String, String> getProperties() {
		return properties;
	}
}
