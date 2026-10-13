package org.skyve.domain.app.admin;

import java.util.List;

import org.skyve.domain.PersistentBean;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Domain contract for report template metadata and generated output settings.
 */
public interface ReportTemplate extends PersistentBean {
	@Nullable String getTemplate();
	@Nonnull List<? extends ReportParameter> getParameters();
	@Nonnull List<? extends ReportDataset> getDatasets();
}
