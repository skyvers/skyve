package org.skyve.impl.web.faces.pipeline.component;

import javax.faces.component.UIComponent;

import org.primefaces.component.api.UIPageableData;
import org.skyve.domain.Bean;
import org.skyve.impl.metadata.view.widget.bound.tabular.ListGrid;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.view.model.list.ListModel;

public class PaginatedListGridBuilder extends NoOpComponentBuilder {
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
			UIPageableData dt = (UIPageableData) component;
			dt.setPaginator(true);
	    	dt.setRowsPerPageTemplate("25,50,75,100");
	    	dt.setPaginatorAlwaysVisible(false);
		}
		
		return component;
	}
}
