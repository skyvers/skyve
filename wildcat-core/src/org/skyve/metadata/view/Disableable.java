package org.skyve.metadata.view;

/**
 * 
 */
public interface Disableable {
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
