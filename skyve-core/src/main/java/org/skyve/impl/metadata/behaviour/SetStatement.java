package org.skyve.impl.metadata.behaviour;

import org.skyve.domain.Bean;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.repository.behaviour.statement.StatementMetaData;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.util.ExpressionEvaluator;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * JAXB-annotated assignment statement for the Skyve behaviour (business rules) DSL.
 *
 * <p>A {@code SetStatement} evaluates an {@code expression} (MVEL or Skyve EL) and
 * assigns the result to the attribute identified by {@code binding} on the current
 * {@link Bean} context.  The binding is a dot-separated Skyve binding path.
 *
 * <p>Precondition: {@code binding} must be a valid, writable Skyve binding path
 * on the current document; {@code expression} must evaluate to a type compatible
 * with the target attribute.
 *
 * <p>Threading: not thread-safe.  Instances are populated during JAXB unmarshalling
 * and are read-only once placed in the repository cache.
 *
 * @see IfStatement
 * @see org.skyve.impl.metadata.repository.behaviour.statement.StatementMetaData
 */
@XmlRootElement(namespace = XMLMetaData.BEHAVIOUR_NAMESPACE, name = "set")
@XmlType(namespace = XMLMetaData.BEHAVIOUR_NAMESPACE,
			name ="set",
			propOrder = {"binding", "expression"})
public class SetStatement extends StatementMetaData {
	private static final long serialVersionUID = 7250058837673126971L;

	private String binding;
	private String expression;

	public String getBinding() {
		return binding;
	}
	
	@XmlAttribute(required = true)
	public void setBinding(String binding) {
		this.binding = UtilImpl.processStringValue(binding);
	}
	
	public String getExpression() {
		return expression;
	}
	
	@XmlAttribute(required = true)
	public void setExpression(String expression) {
		this.expression = UtilImpl.processStringValue(expression);
	}

	@Override
	public void execute(Bean bean) {
		BindUtil.set(bean, binding, ExpressionEvaluator.evaluate(expression, bean));
	}
}
