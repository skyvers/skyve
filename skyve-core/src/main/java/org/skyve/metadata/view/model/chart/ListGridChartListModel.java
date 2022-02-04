package org.skyve.metadata.view.model.chart;

import org.skyve.domain.Bean;
import org.skyve.metadata.view.model.list.DocumentQueryListModel;
import org.skyve.persistence.DocumentQuery;

public class ListGridChartListModel extends DocumentQueryListModel<Bean> {
	public DocumentQuery getDocumentQuery() {
		return getDetailQuery();
	}
}
