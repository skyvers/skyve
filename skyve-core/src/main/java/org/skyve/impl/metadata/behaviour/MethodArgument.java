package org.skyve.impl.metadata.behaviour;

import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;

import jakarta.annotation.Nullable;
import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * Encapsulates an argument to a Java method call with a type and an expression to evaluate to get a value. 
 */
@XmlRootElement(namespace = XMLMetaData.BEHAVIOUR_NAMESPACE, name = "methodArgument")
@XmlType(namespace = XMLMetaData.BEHAVIOUR_NAMESPACE,
			name ="methodArgument",
			propOrder = {"typeName", "expression"})
public class MethodArgument {

	// The fully qualified Java class name of the argument type.
	private String typeName;

	// A Skyve expression to evaluate to get the argument value
	private String expression;

	public @Nullable String getTypeName() {
		return typeName;
	}
	
	@XmlAttribute(required = true)
	public void setTypeName(@Nullable String typeName) {
		this.typeName = UtilImpl.processStringValue(typeName);
	}
	
	public @Nullable String getExpression() {
		return expression;
	}
	
	@XmlAttribute(required = true)
	public void setExpression(@Nullable String expression) {
		this.expression = UtilImpl.processStringValue(expression);
	}
}
