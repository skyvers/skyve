package org.skyve.impl.metadata.repository.view.actions;

import java.util.ArrayList;
import java.util.List;

import org.skyve.impl.metadata.view.ActionImpl;
import org.skyve.impl.metadata.view.widget.bound.ParameterImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.view.Parameterizable;
import org.skyve.metadata.view.widget.bound.Parameter;

import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * Abstract JAXB base for action descriptors that accept runtime parameters.
 *
 * <p>Extends {@link PositionableAction} with the {@link Parameterizable}
 * contract, allowing the view author to supply a list of named parameters
 * passed to the action handler at runtime.
 *
 * <p>Threading: not thread-safe.  Read-only after JAXB unmarshalling.
 *
 * @see ReportAction
 */
@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE)
public abstract class ParameterizableAction extends PositionableAction implements Parameterizable {
	private static final long serialVersionUID = -9222943585683812109L;

	private List<Parameter> parameters = new ArrayList<>();

	/**
	 * Returns parameters declared for this action descriptor.
	 *
	 * @return mutable parameter list in declaration order
	 */
	@Override
	@XmlElement(namespace = XMLMetaData.VIEW_NAMESPACE, 
					name = "parameter",
					type = ParameterImpl.class,
					required = false)
	public List<Parameter> getParameters() {
		return parameters;
	}

	/**
	 * Converts this descriptor to runtime metadata including declared parameters.
	 *
	 * @return runtime action metadata with parameters appended in declaration order
	 */
	@Override
	public ActionImpl toMetaDataAction() {
		ActionImpl result = super.toMetaDataAction();
		result.getParameters().addAll(parameters);
		return result;
	}
}
