package org.skyve.impl.metadata.view.widget.bound.tabular;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.metadata.view.WidgetReference;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.view.widget.bound.Bound;

@XmlRootElement(namespace = XMLMetaData.VIEW_NAMESPACE, name = "boundColumn")
@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE,
			propOrder = {"binding", "editable", "inputWidget"})
public class DataGridBoundColumn extends DataGridColumn implements Bound {
	private static final long serialVersionUID = -26924109323814766L;

	private String binding;
	private WidgetReference inputWidget;
	private Boolean editable;

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
}
