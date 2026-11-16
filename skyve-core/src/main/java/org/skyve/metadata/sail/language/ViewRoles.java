package org.skyve.metadata.sail.language;

import java.util.ArrayList;
import java.util.List;

import org.skyve.impl.util.XMLMetaData;
import org.skyve.util.Util;

import jakarta.annotation.Nonnull;
import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlType;
import jakarta.xml.bind.annotation.adapters.CollapsedStringAdapter;
import jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

/**
 * The roles that a {@link Suite}'s runner builds the views with for one user that its scripts sign in as, for example
 * <pre>
 * &lt;viewRoles user="sail{run}"&gt;
 * 	&lt;role&gt;admin.BasicUser&lt;/role&gt;
 * &lt;/viewRoles&gt;
 * </pre>
 *
 * <p>An interpreted runner rebuilds views in the test JVM to locate widgets, by default as a user with every role. A
 * script that signs in as a user with fewer roles registers that user's roles here, so that the runner locates only
 * the widgets the user sees. {@code user} is the login's user as the script writes it, and can hold {@code {run}} and
 * {@code {var:name}}. A role is {@code module.Role}, or a customer role's name.
 *
 * <p>Not thread-safe: built, or unmarshalled, before a suite runs.
 *
 * @see Suite#getViewRoles()
 */
@XmlType(namespace = XMLMetaData.SAIL_NAMESPACE, propOrder = {"user", "roles"})
public class ViewRoles {
	private String user;
	private final List<String> roles = new ArrayList<>();

	/**
	 * Returns the user name as the suite's scripts sign in with it.
	 *
	 * @return the user name; never {@code null} in a suite that passed {@link Suite#validate(String)}
	 */
	public @Nonnull String getUser() {
		return user;
	}

	/**
	 * Sets the user name as the suite's scripts sign in with it.
	 *
	 * @param user	the user name, which can hold {@code {run}} and {@code {var:name}}; a blank value is stored as
	 * 				{@code null}, which {@link Suite#validate(String)} rejects
	 */
	@XmlAttribute(name = "user", required = true)
	public void setUser(@Nonnull String user) {
		this.user = Util.processStringValue(user);
	}

	/**
	 * Returns the user's roles, each {@code module.Role} or a customer role's name. Unmarshalling collapses the white
	 * space in each role (an {@code xs:token}), so a role written over several lines still matches.
	 *
	 * @return the roles, in the order written (a live list; at least one in a valid suite)
	 */
	@XmlElement(namespace = XMLMetaData.SAIL_NAMESPACE, name = "role", required = true)
	@XmlJavaTypeAdapter(CollapsedStringAdapter.class)
	public @Nonnull List<String> getRoles() {
		return roles;
	}
}
