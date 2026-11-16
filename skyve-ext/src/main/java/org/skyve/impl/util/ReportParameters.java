package org.skyve.impl.util;

import java.util.Map;

import org.skyve.domain.Bean;
import org.skyve.metadata.model.document.Document;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Bundles one report execution request for batch report rendering APIs.
 *
 * <p>Each instance provides the report-owning document, report name, parameter map,
 * and optional bean context.
 */
public class ReportParameters {
	private final Document document;
	private final String reportName;
	private final Map<String, Object> parameters;
	private final Bean bean;

	/**
	 * Creates report parameters without a bean context.
	 *
	 * @param document Report-owning document metadata
	 * @param reportName Report template name
	 * @param parameters Named report parameters
	 */
	public ReportParameters(@Nonnull Document document, @Nonnull String reportName, @Nonnull Map<String, Object> parameters) {
		this(document, reportName, parameters, null);
	}

	/**
	 * Creates report parameters.
	 *
	 * @param document Report-owning document metadata
	 * @param reportName Report template name
	 * @param parameters Named report parameters
	 * @param bean Optional bean context for bean-backed reports
	 */
	public ReportParameters(@Nonnull Document document, @Nonnull String reportName, @Nonnull Map<String, Object> parameters, @Nullable Bean bean) {
		this.document = document;
		this.reportName = reportName;
		this.parameters = parameters;
		this.bean = bean;
	}

	/**
	 * Returns report-owning document metadata.
	 */
	public @Nonnull Document getDocument() {
		return document;
	}

	/**
	 * Returns report template name.
	 */
	public @Nonnull String getReportName() {
		return reportName;
	}

	/**
	 * Returns named report parameters.
	 */
	public @Nonnull Map<String, Object> getParameters() {
		return parameters;
	}

	/**
	 * Returns optional bean context.
	 */
	public @Nullable Bean getBean() {
		return bean;
	}
}
