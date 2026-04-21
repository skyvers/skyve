package org.skyve.impl.util;

import java.io.Serializable;
import java.util.Objects;

/**
 * A single MFA method entry in the per-user MFA configuration JSON array.
 */
public class MfaOption implements Serializable {

	private static final long serialVersionUID = 5510114501888272179L;

	private String method;
	private boolean enabled;

	/**
	 * Default constructor for JSON deserialisation.
	 */
	public MfaOption() {
		// for JSON deserialisation
	}

	/**
	 * Create an MFA option.
	 *
	 * @param method the MFA method name (for example {@code EMAIL})
	 * @param enabled whether this method is enabled for the user
	 */
	public MfaOption(String method, boolean enabled) {
		this.method = UtilImpl.processStringValue(method);
		this.enabled = enabled;
	}

	/**
	 * @return the MFA method name
	 */
	public String getMethod() {
		return method;
	}

	/**
	 * @param method the MFA method name
	 */
	public void setMethod(String method) {
		this.method = UtilImpl.processStringValue(method);
	}

	/**
	 * @return whether the method is enabled
	 */
	public boolean isEnabled() {
		return enabled;
	}

	/**
	 * @param enabled whether the method is enabled
	 */
	public void setEnabled(boolean enabled) {
		this.enabled = enabled;
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public int hashCode() {
		return Objects.hash(Boolean.valueOf(enabled), method);
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (!(obj instanceof MfaOption)) {
			return false;
		}
		MfaOption other = (MfaOption) obj;
		return (enabled == other.enabled) && Objects.equals(method, other.method);
	}
}
