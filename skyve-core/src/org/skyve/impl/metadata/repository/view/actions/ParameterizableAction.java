package org.skyve.impl.metadata.repository.view.actions;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.metadata.view.ActionImpl;
import org.skyve.impl.metadata.view.widget.bound.ParameterImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.view.Parameterizable;
import org.skyve.metadata.view.widget.bound.Parameter;

@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE)
public abstract class ParameterizableAction extends PositionableAction implements Parameterizable {
	private List<Parameter> parameters = new ArrayList<>();

	@Override
	@XmlElement(namespace = XMLMetaData.VIEW_NAMESPACE, 
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
