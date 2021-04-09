package org.skyve.impl.metadata.view.event;

import java.util.List;

public interface Selectable extends EventSource {
	public List<EventAction> getSelectedActions();
	public String getSelectedIdBinding();
	public void setSelectedIdBinding(String selectedIdBinding);
}
