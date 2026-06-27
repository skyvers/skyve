package org.skyve.domain.app.admin;

import java.util.List;

import org.skyve.domain.PersistentBean;

/**
 * Domain contract for report template metadata and generated output settings.
 */
public interface ReportTemplate extends PersistentBean {
	String getTemplate();
	List<? extends ReportParameter> getParameters();
	List<? extends ReportDataset> getDatasets();
}
