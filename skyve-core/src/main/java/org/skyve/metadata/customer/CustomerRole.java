package org.skyve.metadata.customer;

import org.skyve.metadata.DecoratedMetaData;
import org.skyve.metadata.NamedMetaData;

/**
 * A customer-level aggregate security role.
 *
 * <p>Customer roles combine one or more module-level roles into a named role that
 * can be assigned to users in the admin module. They are declared in the customer XML
 * under {@code <roles>}.
 *
 * @see Customer#getRoles()
 */
public interface CustomerRole extends NamedMetaData, DecoratedMetaData {
	/**
	 * Returns a short human-readable description of this role.
	 *
	 * @return the description; may be {@code null}
	 */
	public String getDescription();

	/**
	 * Returns extended documentation (HTML or plain text) describing the purpose and
	 * permissions associated with this role.
	 *
	 * @return the documentation; may be {@code null}
	 */
	public String getDocumentation();
}
