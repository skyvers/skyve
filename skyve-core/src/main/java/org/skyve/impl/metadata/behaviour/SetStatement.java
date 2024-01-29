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
