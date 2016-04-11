package org.skyve.wildcat.metadata.view.widget.bound.tabular;

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