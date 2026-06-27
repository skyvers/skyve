package org.skyve.impl.metadata.view.widget.bound.tabular;

/**
 * Mixin interface for CRUD grid widgets that support condition-based disabling
 * of the add, edit, zoom-in, and remove operations.
 *
 * <p>Each CRUD operation can be independently disabled by a condition name,
 * giving fine-grained control over what actions a user may perform in a grid.
 */
public interface DisableableCRUDGrid {

	public String getDisableAddConditionName();
	public void setDisableAddConditionName(String disableAddConditionName);
	public void setEnableAddConditionName(String eableAddConditionName);
	public String getDisableZoomConditionName();
	public void setDisableZoomConditionName(String disableZoomConditionName);
	public void setEnableZoomConditionName(String enableZoomConditionName);
	public String getDisableEditConditionName();
	public void setDisableEditConditionName(String disableEditConditionName);
	public void setEnableEditConditionName(String enableEditConditionName);
	public String getDisableRemoveConditionName();
	public void setDisableRemoveConditionName(String disableRemoveConditionName);
	public void setEnableRemoveConditionName(String enableRemoveConditionName);
}