package org.skyve.wildcat.metadata.view.widget.bound.tabular;

public interface DisableableCRUDGrid {

	public String getDisableAddConditionName();
	public void setDisableAddConditionName(String disableAddConditionName);
	public String getDisableZoomConditionName();
	public void setDisableZoomConditionName(String disableZoomConditionName);
	public String getDisableEditConditionName();
	public void setDisableEditConditionName(String disableEditConditionName);
	public String getDisableRemoveConditionName();
	public void setDisableRemoveConditionName(String disableRemoveConditionName);
}