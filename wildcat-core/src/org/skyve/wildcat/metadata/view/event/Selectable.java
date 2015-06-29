package org.skyve.wildcat.metadata.view.event;

import java.util.List;

public interface Selectable {
	public List<EventAction> getSelectedActions();
	public String getSelectedIdBinding();
	public void setSelectedIdBinding(String selectedIdBinding);
}
