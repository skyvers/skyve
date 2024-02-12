package org.skyve.impl.metadata.view.widget.bound.tabular;

import org.skyve.impl.metadata.view.WidgetReference;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.FormatterName;
import org.skyve.metadata.view.FormattedText;
import org.skyve.metadata.view.TextOutput;
import org.skyve.metadata.view.widget.bound.Bound;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlTransient;
import jakarta.xml.bind.annotation.XmlType;

@XmlRootElement(namespace = XMLMetaData.VIEW_NAMESPACE, name = "boundColumn")
@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE,
			propOrder = {"binding", "editable", "inputWidget", "escape", "sanitise", "formatterName", "customFormatterName"})
public class DataGridBoundColumn extends DataGridColumn implements Bound, TextOutput, FormattedText {
	private static final long serialVersionUID = -26924109323814766L;

	private String binding;
	private WidgetReference inputWidget;
	private Boolean editable;
	private Boolean escape;
	private Sanitisation sanitise;
	private FormatterName formatterName;
	private String customFormatterName;
	
	@Override
	public String getBinding() {
		return binding;
	}

	@Override
	@XmlAttribute
	public void setBinding(String binding) {
		this.binding = UtilImpl.processStringValue(binding);
	}

	/**
	 * Return the event source string = binding.
	 */
	@Override
	@XmlTransient
	public String getSource() {
		return binding;
	}

	public WidgetReference getInputWidget() {
		return inputWidget;
	}

	@XmlElement(namespace = XMLMetaData.VIEW_NAMESPACE, name = "input")
	public void setInputWidget(WidgetReference inputWidget) {
		this.inputWidget = inputWidget;
	}

	public Boolean getEditable() {
		return editable;
	}

	@XmlAttribute
	public void setEditable(Boolean editable) {
		this.editable = editable;
	}
	
	@Override
	public Boolean getEscape() {
		return escape;
	}

	@XmlAttribute
	public void setEscape(Boolean escape) {
		this.escape = escape;
	}

	@Override
	public Sanitisation getSanitise() {
		return sanitise;
	}

	@XmlAttribute
	public void setSanitise(Sanitisation sanitise) {
		this.sanitise = sanitise;
	}

	@Override
	public FormatterName getFormatterName() {
		return formatterName;
	}

	@XmlAttribute(name = "formatter")
	public void setFormatterName(FormatterName formatterName) {
		this.formatterName = formatterName;
	}

	@Override
	public String getCustomFormatterName() {
		return customFormatterName;
	}

	@XmlAttribute(name = "customFormatter")
	public void setCustomFormatterName(String customFormatterName) {
		this.customFormatterName = UtilImpl.processStringValue(customFormatterName);
	}
}
