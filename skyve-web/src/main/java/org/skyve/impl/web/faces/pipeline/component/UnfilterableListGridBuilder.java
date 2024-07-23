package org.skyve.impl.web.faces.pipeline.component;

import org.primefaces.component.column.Column;
import org.skyve.domain.Bean;
import org.skyve.impl.metadata.view.widget.bound.tabular.ListGrid;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.view.model.list.ListModel;

import jakarta.faces.component.UIComponent;

public class UnfilterableListGridBuilder extends NoOpComponentBuilder {
	@Override
	public UIComponent listGrid(UIComponent component,
									String moduleName,
									String modelDocumentName,
									String modelName,
									String uxui,
									ListModel<Bean> model,
									Document owningDocument,
									String title,
									ListGrid listGrid,
									boolean aggregateQuery) {
		if (component != null) {
			for (UIComponent child : component.getChildren()) {
				if (child instanceof Column) {
					((Column) child).setFilterable(false);
				}
			}
		}
		
		return component;
	}
}
