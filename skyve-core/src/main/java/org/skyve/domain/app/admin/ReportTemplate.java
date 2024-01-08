package org.skyve.domain.app.admin;

import java.util.List;

import org.skyve.domain.PersistentBean;

public interface ReportTemplate extends PersistentBean {
	String getTemplate();
	List<? extends ReportParameter> getParameters();
	List<? extends ReportDataset> getDatasets();
}
