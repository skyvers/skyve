package org.skyve.metadata.view.widget.bound;

import org.skyve.metadata.NamedMetaData;

/**
 * A named binding-to-value mapping used to pass a value into a child view or action.
 *
 * <p>A {@code Parameter} maps a destination binding name to either a literal string
 * value ({@link #getValue()}) or a source binding on the current bean
 * ({@link #getValueBinding()}). The name (from {@link org.skyve.metadata.NamedMetaData})
 * identifies the destination binding in the target context.
 *
 * @see org.skyve.metadata.view.Parameterizable
 * @see Bound
 */
public interface Parameter extends NamedMetaData {
	/**
	 * Returns the literal value to pass, or {@code null} if a value binding is used.
	 *
	 * @return the literal parameter value, or {@code null}
	 */
	public String getValue();

	/**
	 * Returns the binding path on the current bean whose value is passed as the
	 * parameter, or {@code null} if a literal value is used.
	 *
	 * @return the value binding path, or {@code null}
	 */
	public String getValueBinding();
}
