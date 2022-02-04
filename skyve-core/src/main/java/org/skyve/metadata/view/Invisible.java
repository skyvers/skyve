package org.skyve.metadata.view;

import org.skyve.metadata.SerializableMetaData;

/**
 * 
 */
public interface Invisible extends SerializableMetaData {
	/**
	 * A condition name to evaluate to determine if this is invisible.
	 * @return
	 */
	public String getInvisibleConditionName();
	
	/**
	 * Set a condition name to evaluate to determine if this is invisible.
	 * @param invisibleConditionName
	 */
	public void setInvisibleConditionName(String invisibleConditionName);

	/**
	 * Set a condition name to evaluate to determine if this is visible.
	 * This will negate the condition name and set the invisible condition name.
	 * @param visibleConditionName
	 */
	public void setVisibleConditionName(String visibleConditionName);
}
