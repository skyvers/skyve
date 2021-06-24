package org.skyve.domain.app.admin;

import java.util.List;

import org.skyve.domain.PersistentBean;

public interface ReportTemplate extends PersistentBean {
	public static final String MODULE_NAME = "admin";
	public static final String DOCUMENT_NAME = "ReportTemplate";

	public static final String templateNamePropertyName = "templateName";
	public static final String enabledPropertyName = "enabled";
	
	String getTemplate();
	List<? extends ReportParameter> getParameters();
	List<? extends ReportDataset> getDatasets();
}
