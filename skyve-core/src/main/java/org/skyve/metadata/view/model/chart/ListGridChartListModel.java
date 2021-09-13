package org.skyve.metadata.view.model.chart;

import org.skyve.domain.Bean;
import org.skyve.metadata.view.model.list.DocumentQueryListModel;
import org.skyve.persistence.DocumentQuery;

public class ListGridChartListModel extends DocumentQueryListModel<Bean> {
	private static final long serialVersionUID = -3572919565643542970L;

	public DocumentQuery getDocumentQuery() {
		return getDetailQuery();
	}
}
