package org.skyve.metadata.view;

import org.skyve.metadata.SerializableMetaData;

/**
 * Mixin interface for view elements that can be conditionally disabled.
 *
 * <p>The disabled state is driven by a named {@link org.skyve.metadata.model.document.Condition}
 * evaluated on the current document bean. The convenience method
 * {@link #setEnabledConditionName} accepts an enable condition and negates it
 * (by prepending {@code not}) to produce the stored disabled condition name.
 *
 * @see Invisible
 * @see Editable
 */
public interface Disableable extends SerializableMetaData {
	/**
	 * A condition name to evaluate to determine if this is disabled.
	 * @return
	 */
	public String getDisabledConditionName();
	
	/**
	 * Set a condition name to evaluate to determine if this is disabled.
	 * @param disabledConditionName
	 */
	public void setDisabledConditionName(String disabledConditionName);

	/**
	 * Set a condition name to evaluate to determine if this is enabled.
	 * This will negate the condition name and set the disabled condition name.
	 * @param disabledConditionName
	 */
	public void setEnabledConditionName(String disabledConditionName);
}
