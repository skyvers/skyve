package org.skyve.wildcat.metadata.repository.view.actions;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.metadata.view.Parameterizable;
import org.skyve.metadata.view.widget.bound.Parameter;
import org.skyve.wildcat.metadata.view.ActionImpl;
import org.skyve.wildcat.metadata.view.widget.bound.ParameterImpl;
import org.skyve.wildcat.util.XMLUtil;

@XmlType(namespace = XMLUtil.VIEW_NAMESPACE)
public abstract class ParameterizableAction extends PositionableAction implements Parameterizable {
	private List<Parameter> parameters = new ArrayList<>();

	@Override
	@XmlElement(namespace = XMLUtil.VIEW_NAMESPACE, 
					name = "parameter",
					type = ParameterImpl.class,
					required = false)
	public List<Parameter> getParameters() {
		return parameters;
	}

	@Override
	public ActionImpl toMetaDataAction() {
		ActionImpl result = super.toMetaDataAction();
		result.getParameters().addAll(parameters);
		return result;
	}
}
