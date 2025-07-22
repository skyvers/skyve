package org.skyve.impl.metadata.behaviour;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;

import org.skyve.domain.Bean;
import org.skyve.domain.messages.DomainException;
import org.skyve.impl.metadata.repository.behaviour.statement.StatementMetaData;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.util.ExpressionEvaluator;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlElementWrapper;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * Allows method invocation on the current bean (within the conversation).
 */
@XmlRootElement(namespace = XMLMetaData.BEHAVIOUR_NAMESPACE, name = "invoke")
@XmlType(namespace = XMLMetaData.BEHAVIOUR_NAMESPACE,
			name ="invoke",
			propOrder = {"methodName", "arguments"})
public class InvokeStatement extends StatementMetaData {
	private static final long serialVersionUID = 6404644142542860330L;

	// The method name on the current bean to call.
	private String methodName;
	
	@XmlElementWrapper(namespace = XMLMetaData.BEHAVIOUR_NAMESPACE, name = "arguments", required = true)
	@XmlElement(namespace = XMLMetaData.BEHAVIOUR_NAMESPACE, name = "argument")
	private List<MethodArgument> arguments = new ArrayList<>();

	public String getMethodName() {
		return methodName;
	}
	
	@XmlAttribute(name="method", required = true)
	public void setMethodName(String methodName) {
		this.methodName = UtilImpl.processStringValue(methodName);
	}

	public List<MethodArgument> getArguments() {
		return arguments;
	}

	@Override
	public void execute(Bean bean) {
		execute(bean.getClass(), bean);
	}
	
	protected void execute(Class<?> type, Bean bean) {
		int numberOfArguments = arguments.size();
		Class<?>[] argumentTypes = new Class[numberOfArguments];
		Object[] argumentValues = new Object[numberOfArguments];

		ClassLoader classLoader = Thread.currentThread().getContextClassLoader();
		
		try {
			for (int i = 0; i < numberOfArguments; i++) {
				MethodArgument argument = arguments.get(i);
				argumentTypes[i] = classLoader.loadClass(argument.getTypeName());
				argumentValues[i] = ExpressionEvaluator.evaluate(argument.getExpression(), bean);
			}
	
			Method m = type.getMethod(methodName, argumentTypes);
			m.invoke(bean, argumentValues);
		}
		catch (Exception e) {
			throw new DomainException("Error invoking method " + methodName, e);
		}
	}
}
