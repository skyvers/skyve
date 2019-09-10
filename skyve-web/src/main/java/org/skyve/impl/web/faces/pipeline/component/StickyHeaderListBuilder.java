package org.skyve.impl.web.faces.pipeline.component;

import java.util.List;

import javax.faces.component.UIComponent;

import org.primefaces.component.datatable.DataTable;
import org.skyve.domain.Bean;
import org.skyve.impl.metadata.view.widget.bound.tabular.ListGrid;
import org.skyve.metadata.view.model.list.ListModel;
import org.skyve.metadata.view.widget.bound.FilterParameter;
import org.skyve.metadata.view.widget.bound.Parameter;

public class StickyHeaderListBuilder extends NoOpComponentBuilder {
	@Override
	public UIComponent listGrid(UIComponent component,
									String moduleName,
									String modelDocumentName,
									String modelName,
									ListModel<? extends Bean> model,
									String title,
									ListGrid listGrid,
									boolean canCreateDocument,
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
										ListModel<? extends Bean> model,
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
