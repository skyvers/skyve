package org.skyve.metadata.view;

import org.skyve.metadata.SerializableMetaData;

/**
 * Mixin interface for view elements that can be conditionally hidden.
 *
 * <p>The invisible state is driven by a named {@link org.skyve.metadata.model.document.Condition}
 * evaluated on the current document bean. The convenience method
 * {@link #setVisibleConditionName} accepts a visible condition and negates it
 * (by prepending {@code not}) to produce the stored invisible condition name.
 *
 * @see Disableable
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
