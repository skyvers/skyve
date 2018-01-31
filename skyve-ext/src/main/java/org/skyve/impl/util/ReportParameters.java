package org.skyve.impl.util;

import java.util.Map;

import org.skyve.domain.Bean;
import org.skyve.metadata.model.document.Document;

public class ReportParameters {
	private final Document document;
	private final String reportName;
	private final Map<String, Object> parameters;
	private final Bean bean;

	public ReportParameters(Document document, String reportName, Map<String, Object> parameters) {
		this(document, reportName, parameters, null);
	}

	public ReportParameters(Document document, String reportName, Map<String, Object> parameters, Bean bean) {
		this.document = document;
		this.reportName = reportName;
		this.parameters = parameters;
		this.bean = bean;
	}

	public Document getDocument() {
		return document;
	}

	public String getReportName() {
		return reportName;
	}

	public Map<String, Object> getParameters() {
		return parameters;
	}

	public Bean getBean() {
		return bean;
	}
}
