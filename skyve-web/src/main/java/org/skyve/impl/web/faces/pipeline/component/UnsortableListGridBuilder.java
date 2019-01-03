package org.skyve.impl.web.faces.pipeline.component;

import javax.faces.component.UIComponent;

import org.primefaces.component.column.Column;
import org.skyve.domain.Bean;
import org.skyve.impl.metadata.view.widget.bound.tabular.ListGrid;
import org.skyve.metadata.view.model.list.ListModel;

public class UnsortableListGridBuilder extends NoOpComponentBuilder {
	@Override
	public UIComponent listGrid(UIComponent component,
									String modelDocumentName,
									String modelName,
									ListModel<? extends Bean> model,
									String title,
									ListGrid listGrid,
									boolean canCreateDocument) {
		if (component != null) {
			for (UIComponent child : component.getChildren()) {
				if (child instanceof Column) {
					((Column) child).setSortable(false);
				}
			}
		}
		
		return component;
	}
}
