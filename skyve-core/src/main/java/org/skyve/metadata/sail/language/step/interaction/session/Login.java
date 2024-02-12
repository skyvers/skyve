package org.skyve.metadata.sail.language.step.interaction.session;

import org.skyve.metadata.sail.execution.Executor;
import org.skyve.metadata.sail.language.Step;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

import org.skyve.impl.sail.execution.AutomationContext;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;

/**
 * Login using customer name (optional), username and password credential.
 * 
 * @author mike
 */
@XmlType(namespace = XMLMetaData.SAIL_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.SAIL_NAMESPACE)
public class Login implements Step {
	private String customer;
	private String user;
	private String password;


	public String getCustomer() {
		return customer;
	}

	@XmlAttribute
	public void setCustomer(String customer) {
		this.customer = UtilImpl.processStringValue(customer);
	}

	public String getUser() {
		return user;
	}

	@XmlAttribute(required = true)
	public void setUser(String user) {
		this.user = UtilImpl.processStringValue(user);
	}

	public String getPassword() {
		return password;
	}

	@XmlAttribute(required = true)
	public void setPassword(String password) {
		this.password = UtilImpl.processStringValue(password);
	}

	@Override
	public void execute(Executor executor) {
		executor.executeLogin(this);
	}
	
	@Override
	public String getIdentifier(AutomationContext context) {
		return null;
	}
}
