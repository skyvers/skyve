package org.skyve.metadata.view.model.chart;

import org.skyve.domain.Bean;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;
import org.skyve.metadata.view.model.list.DocumentQueryListModel;
import org.skyve.persistence.DocumentQuery;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

public class ListGridChartListModel extends DocumentQueryListModel<Bean> {
	public ListGridChartListModel(@Nonnull MetaDataQueryDefinition query) {
		super(query);
	}
	
	public @Nullable DocumentQuery getDocumentQuery() {
		return getDetailQuery();
	}
}
