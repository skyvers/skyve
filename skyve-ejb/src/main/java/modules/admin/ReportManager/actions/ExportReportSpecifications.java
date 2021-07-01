package modules.admin.ReportManager.actions;

import java.io.File;

import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.ValidationException;
import org.skyve.metadata.controller.Download;
import org.skyve.metadata.controller.DownloadAction;
import org.skyve.util.BeanValidator;
import org.skyve.util.FileUtil;
import org.skyve.web.WebContext;

import modules.admin.ReportManager.ReportManagerExtension;
import modules.admin.domain.ReportTemplate;

/**
 * Download the report specification as JSON
 * 
 * @author RBB
 *
 */
public class ExportReportSpecifications extends DownloadAction<ReportManagerExtension> {

	private static final long serialVersionUID = 6741549541099049576L;

	/**
	 * Prepare the zip for download
	 */
	@Override
	public void prepare(ReportManagerExtension bean, WebContext webContext) throws Exception {

		if (bean.getCurrentReports().size() == 0) {
			throw new ValidationException(new Message("Please select at least one report to export"));
		}

		// validate all reports first
		validateReports(bean);

		File outdir = bean.getTemporaryPreparationFolder();
		bean.setPathToZip(outdir.getAbsolutePath());

		// write each ReportTemplate to a file within the folder
		for (ReportTemplate report : bean.getCurrentReports()) {
			bean.marshallReportBean(report, report.getName());
		}

	}

	/**
	 * Checks that all reports pass validation before exporting
	 * 
	 * @param bean The ReportManager with all the selected reports to validate
	 * @throws ValidationException if any reports fail validation
	 */
	private void validateReports(ReportManagerExtension bean) throws ValidationException {
		ValidationException e = new ValidationException();

		for (ReportTemplate report : bean.getCurrentReports()) {
			try {
				BeanValidator.validateBeanAgainstDocument(report);
				BeanValidator.validateBeanAgainstBizlet(report);
			} catch (ValidationException veT) {
				e.getMessages().add(new Message(
						"The report " + report.getName() + " is not valid - ensure the report is valid before exporting"));
			}
		}

		if (e.getMessages().size() > 0) {
			throw e;
		}
	}

	/**
	 * Marshall a json version of each report
	 * Save to a temporary folder and then zip and provide as a download
	 */
	@Override
	public Download download(ReportManagerExtension bean, WebContext webContext) throws Exception {

		Download download = FileUtil.prepareZipDownload(bean.getPathToZip(), bean.getZipName(), webContext);

		bean.cleanUpTemporaryFiles();

		return download;
	}
}
