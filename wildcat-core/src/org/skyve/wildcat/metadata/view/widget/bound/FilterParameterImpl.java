package org.skyve.wildcat.metadata.view.widget.bound;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlType;

import org.skyve.metadata.FilterOperator;
import org.skyve.metadata.view.widget.bound.FilterParameter;
import org.skyve.wildcat.util.XMLUtil;

@XmlType(namespace = XMLUtil.VIEW_NAMESPACE, propOrder = {"operator"})
public final class FilterParameterImpl extends ParameterImpl implements FilterParameter {
	private static final long serialVersionUID = -4191066734972894505L;

	private FilterOperator operator;
	
	@Override
	public FilterOperator getOperator() {
		return operator;
	}

	@XmlAttribute(required = true)
	public void setOperator(FilterOperator operator) {
		this.operator = operator;
	}
}
