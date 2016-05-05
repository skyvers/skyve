package org.skyve.impl.metadata.view.widget.bound;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.view.AbsoluteSize;
import org.skyve.impl.metadata.view.ContentSpecifiedWidth;
import org.skyve.impl.metadata.view.HorizontalAlignment;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.view.Invisible;
import org.skyve.impl.metadata.view.widget.bound.AbstractBound;

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
						"formatted"})
public class Label extends AbstractBound implements Invisible, AbsoluteSize, ContentSpecifiedWidth {
	/**
	 * For Serialization
	 */
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
	
	public String getValue() {
		return value;
	}

	@XmlAttribute(required = false)
	public void setValue(String value) {
		this.value = UtilImpl.processStringValue(value);
	}

	public String getFor() {
		return forBinding;
	}

	@XmlAttribute(name = "for", required = false)
	public void setFor(String forBinding) {
		this.forBinding = UtilImpl.processStringValue(forBinding);
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
	public String getInvisibleConditionName() {
		return invisibleConditionName;
	}

	@Override
	@XmlAttribute(name = "invisible", required = false)
	public void setInvisibleConditionName(String invisibleConditionName) {
		this.invisibleConditionName = UtilImpl.processStringValue(invisibleConditionName);
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

	public Boolean isFormatted() {
		return formatted;
	}

	@XmlAttribute(name = "formatted", required = false)
	public void setFormatted(Boolean formatted) {
		this.formatted = formatted;
	}

	public HorizontalAlignment getTextAlignment() {
		return textAlignment;
	}

	@XmlAttribute(name = "textAlignment", required = false)
	public void setTextAlignment(HorizontalAlignment textAlignment) {
		this.textAlignment = textAlignment;
	}
}
