package modules.admin.ReportTemplate;

import org.skyve.domain.app.admin.ReportDataset.DatasetType;
import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.ValidationException;

import jakarta.enterprise.inject.Default;
import modules.admin.ReportDataset.ReportDatasetExtension;
import modules.admin.ReportParameter.ReportParameterExtension;
import modules.admin.domain.ReportTemplate.ReportType;

/**
 * This class acts as a service layer to encapsulate domain logic.
 *
 * Add this line to classes that wish to use it: @Inject private transient ReportTemplateService reportTemplateService;
 */
@Default
public class ReportTemplateService {
	/**
	 * Validates that all ReportParameters for this template are used by at least one ReportDataset query.
	 * 
	 * @param bean The ReportTemplate to validate
	 * @param e The ValidationException any errors will be added to
	 */
	@SuppressWarnings("static-method")
	public void validateReportParameters(ReportTemplateExtension bean, ValidationException e) {
		// skip validation for jasper reports
		if (bean.getReportType() == ReportType.jasper) {
			return;
		}

		for (ReportParameterExtension param : bean.getParameters()) {
			boolean inUse = false;

			for (ReportDatasetExtension dataset : bean.getDatasets()) {
				// skip constants as they can't accept parameters
				if (dataset.getDatasetType() == DatasetType.constant) {
					continue;
				}

				// class datasets always inject all parameters
				if (dataset.getDatasetType() == DatasetType.classValue) {
					inUse = true;
					break;
				}

				// check bizQL or SQL datasets for all parameters
				if (dataset.getDatasetType() == DatasetType.bizQL || dataset.getDatasetType() == DatasetType.SQL) {
					if (dataset.containsParameter(param)) {
						inUse = true;
						break;
					}
				}
			}

			if (inUse == false) {
				e.getMessages()
						.add(new Message(String.format(
								"Parameter %s is not in use by any dataset. Please include it in a dataset or remove it.",
								param.getName())));
			}
		}
	}
}
