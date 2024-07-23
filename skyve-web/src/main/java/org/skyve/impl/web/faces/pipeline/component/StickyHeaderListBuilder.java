package org.skyve.impl.web.faces.pipeline.component;

import java.util.List;

import org.primefaces.component.datatable.DataTable;
import org.skyve.domain.Bean;
import org.skyve.impl.metadata.view.widget.bound.tabular.ListGrid;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.view.model.list.ListModel;
import org.skyve.metadata.view.widget.FilterParameter;
import org.skyve.metadata.view.widget.bound.Parameter;

import jakarta.faces.component.UIComponent;

public class StickyHeaderListBuilder extends NoOpComponentBuilder {
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
			((DataTable) component).setStickyHeader(true);
		}
		return component;
	}
	
	@Override
	public UIComponent listRepeater(UIComponent component,
										String modelDocumentName,
										String modelName,
										String uxui,
										ListModel<Bean> model,
										List<FilterParameter> filterParameters,
										List<Parameter> parameters,
										String title,
										boolean showColumnHeaders,
										boolean showGrid) {
		if (component != null) {
			((DataTable) component).setStickyHeader(true);
		}
		return component;
	}
}
