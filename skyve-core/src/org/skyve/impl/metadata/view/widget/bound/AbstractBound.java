package org.skyve.impl.metadata.view.widget.bound;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.view.widget.bound.Bound;

@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE, propOrder = {"binding"})
public abstract class AbstractBound implements Bound {
	private static final long serialVersionUID = 8278612580977136162L;

	private String binding;

	@Override
	public String getBinding() {
		return binding;
	}

	@Override
	@XmlAttribute(required = false)
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
}
