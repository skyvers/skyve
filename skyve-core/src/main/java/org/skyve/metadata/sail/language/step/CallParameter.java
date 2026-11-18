package org.skyve.metadata.sail.language.step;

import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;

import jakarta.annotation.Nonnull;
import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlType;

/**
 * One {@code <parameter name="…" value="…"/>} of a {@link Call}, which the call's
 * {@link org.skyve.metadata.sail.execution.SailCall} receives by name.
 *
 * <p>{@code {run}} and {@code {var:name}} in the value are substituted when the call runs; the name is not. Both are
 * required, and a name may appear only once in a call: {@link Call#execute(org.skyve.metadata.sail.execution.Executor)}
 * checks this.
 *
 * <p>Not thread-safe: built, or unmarshalled, before the automation runs.
 */
@XmlType(namespace = XMLMetaData.SAIL_NAMESPACE, propOrder = {"name", "value"})
public class CallParameter {
	private String name;
	private String value;

	/**
	 * Returns the parameter's name.
	 * @return the name; {@code null} only on a parameter built without one, which the call rejects
	 */
	public @Nonnull String getName() {
		return name;
	}

	/**
	 * Sets the parameter's name.
	 * @param name the name, trimmed; blank means none
	 */
	@XmlAttribute(name = "name", required = true)
	public void setName(@Nonnull String name) {
		this.name = UtilImpl.processStringValue(name);
	}

	/**
	 * Returns the parameter's value, before token substitution.
	 * @return the value; {@code null} only on a parameter built without one, which the call rejects
	 */
	public @Nonnull String getValue() {
		return value;
	}

	/**
	 * Sets the parameter's value.
	 * @param value the value, trimmed, in which {@code {run}} and {@code {var:name}} are substituted; blank means none
	 */
	@XmlAttribute(name = "value", required = true)
	public void setValue(@Nonnull String value) {
		this.value = UtilImpl.processStringValue(value);
	}
}
