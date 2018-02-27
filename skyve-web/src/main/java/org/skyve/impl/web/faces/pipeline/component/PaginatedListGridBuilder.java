package org.skyve.impl.web.faces.pipeline.component;

import javax.faces.component.UIComponent;

import org.primefaces.component.api.UIData;
import org.skyve.domain.Bean;
import org.skyve.impl.metadata.view.widget.bound.tabular.ListGrid;
import org.skyve.metadata.view.model.list.ListModel;

public class PaginatedListGridBuilder extends NoOpComponentBuilder {
	@Override
	public UIComponent listGrid(UIComponent component,
									String modelDocumentName,
									String modelName,
									ListModel<? extends Bean> model,
									ListGrid listGrid,
									boolean canCreateDocument) {
		if (component != null) {
			UIData dt = (UIData) component;
			dt.setPaginator(true);
	    	dt.setRowsPerPageTemplate("25,50,75,100");
	    	dt.setRows(50);
	    	dt.setPaginatorAlwaysVisible(false);
		}
		
		return component;
	}
}
