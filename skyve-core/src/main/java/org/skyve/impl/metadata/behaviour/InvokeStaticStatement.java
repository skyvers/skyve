package org.skyve.impl.metadata.behaviour;

import org.skyve.domain.Bean;
import org.skyve.domain.messages.DomainException;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * Allows static method invocation on any Java class.
 */
@XmlRootElement(namespace = XMLMetaData.BEHAVIOUR_NAMESPACE, name = "invokeStatic")
@XmlType(namespace = XMLMetaData.BEHAVIOUR_NAMESPACE,
			name ="invokeStatic",
			propOrder = {"className"})
public class InvokeStaticStatement extends InvokeStatement {
	private static final long serialVersionUID = -444107237499417055L;

	// The fully qualified Java class name of the class with the static method to call.
	private String className;

	public String getClassName() {
		return className;
	}
	
	@XmlAttribute(required = true)
	public void setClassName(String className) {
		this.className = UtilImpl.processStringValue(className);
	}

	@Override
	public void execute(Bean bean) {
		ClassLoader classLoader = Thread.currentThread().getContextClassLoader();

		try {
			super.execute(classLoader.loadClass(className), bean);
		}
		catch (Exception e) {
			throw new DomainException("Error invoking static method " + className + '.' + getMethodName(), e);
		}
	}
}
