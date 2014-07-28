package org.skyve.wildcat.metadata.view.widget.bound;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlType;

import org.skyve.metadata.view.widget.bound.Bound;
import org.skyve.wildcat.util.UtilImpl;
import org.skyve.wildcat.util.XMLUtil;

@XmlType(namespace = XMLUtil.VIEW_NAMESPACE, propOrder = {"binding"})
public abstract class AbstractBound implements Bound {
	/**
	 * For Serialization
	 */
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
}
