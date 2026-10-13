package org.skyve.metadata.sail.language.step.interaction.session;

import org.skyve.impl.sail.execution.AutomationContext;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.sail.execution.Executor;
import org.skyve.metadata.sail.language.Step;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * Authenticates a session with the supplied {@code user} and {@code password}
 * credentials, optionally within the named {@code customer} tenant.
 *
 * <p>Side effects: the executor establishes a Skyve web session for the authenticated
 * user; all subsequent steps run in the context of that session until a
 * {@link Logout} step is reached or the procedure ends.
 *
 * @see Logout
 * @see org.skyve.metadata.sail.execution.Executor#executeLogin
 */
@XmlType(namespace = XMLMetaData.SAIL_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.SAIL_NAMESPACE)
public class Login implements Step {

	private String customer;
	private String user;
	private String password;

	/**
	 * Returns the customer.
	 * @return the result
	 */
	public @Nullable String getCustomer() {
		return customer;
	}

	/**
	 * Sets the customer.
	 * @param customer the customer
	 */
	@XmlAttribute
	public void setCustomer(@Nullable String customer) {
		this.customer = UtilImpl.processStringValue(customer);
	}

	/**
	 * Returns the user.
	 * @return the result
	 */
	public @Nullable String getUser() {
		return user;
	}

	/**
	 * Sets the user.
	 * @param user the user
	 */
	@XmlAttribute(required = true)
	public void setUser(@Nullable String user) {
		this.user = UtilImpl.processStringValue(user);
	}

	/**
	 * Returns the password.
	 * @return the result
	 */
	public @Nullable String getPassword() {
		return password;
	}

	/**
	 * Sets the password.
	 * @param password the password
	 */
	@XmlAttribute(required = true)
	public void setPassword(@Nullable String password) {
		this.password = UtilImpl.processStringValue(password);
	}

	/**
	 * Executes execute.
	 * @param executor the executor
	 */
	@Override
	public void execute(@Nonnull Executor executor) {
		executor.executeLogin(this);
	}
	
	/**
	 * Returns the identifier.
	 * @param context the context
	 * @return the result
	 */
	@Override
	public @Nullable String getIdentifier(@Nonnull AutomationContext context) {
		return null;
	}
}
