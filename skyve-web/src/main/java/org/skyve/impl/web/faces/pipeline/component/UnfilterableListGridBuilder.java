package org.skyve.impl.web.faces.pipeline.component;

import org.primefaces.component.column.Column;
import org.skyve.domain.Bean;
import org.skyve.impl.metadata.view.widget.bound.tabular.ListGrid;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.view.model.list.ListModel;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.faces.component.UIComponent;

public class UnfilterableListGridBuilder extends NoOpComponentBuilder {
	@Override
	public @Nullable UIComponent listGrid(@Nullable UIComponent component,
									@Nonnull String moduleName,
									@Nullable String modelDocumentName,
									@Nullable String modelName,
									@Nonnull String uxui,
									@Nonnull ListModel<Bean> model,
									@Nullable Document owningDocument,
									@Nonnull ListGrid listGrid,
									@Nullable String stickyHeaderAnchorSelector,
									boolean aggregateQuery) {
		if (component != null) {
			for (UIComponent child : component.getChildren()) {
				if (child instanceof Column column) {
					column.setFilterable(false);
				}
			}
		}
		
		return component;
	}
}
