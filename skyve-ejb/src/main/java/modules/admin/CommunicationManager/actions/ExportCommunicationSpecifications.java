package modules.admin.CommunicationManager.actions;

import java.io.File;

import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.ValidationException;
import org.skyve.metadata.controller.Download;
import org.skyve.metadata.controller.DownloadAction;
import org.skyve.util.BeanValidator;
import org.skyve.util.FileUtil;
import org.skyve.web.WebContext;

import modules.admin.CommunicationManager.CommunicationManagerExtension;
import modules.admin.domain.Communication;

/**
 * Download the Communication specification as JSON
 * 
 * @author RBB
 *
 */
public class ExportCommunicationSpecifications extends DownloadAction<CommunicationManagerExtension> {
	/**
	 * Prepare the zip for download
	 */
	@Override
	public void prepare(CommunicationManagerExtension bean, WebContext webContext) throws Exception {

		if (bean.getCurrentCommunications().size() == 0) {
			throw new ValidationException(new Message("Please select at least one Communication to export"));
		}

		// validate all Communications first
		validateCommunications(bean);

		File outdir = CommunicationManagerExtension.getTemporaryPreparationFolder();
		bean.setPathToZip(outdir.getAbsolutePath());

		// write each CommunicationTemplate to a file within the folder
		for (Communication communication : bean.getCurrentCommunications()) {
			bean.marshallCommunicationBean(communication, communication.getDescription());
		}

	}

	/**
	 * Checks that all Communications pass validation before exporting
	 * 
	 * @param bean The CommunicationManager with all the selected Communications to validate
	 * @throws ValidationException if any Communications fail validation
	 */
	private static void validateCommunications(CommunicationManagerExtension bean) throws ValidationException {
		ValidationException e = new ValidationException();

		for (Communication communication : bean.getCurrentCommunications()) {
			try {
				BeanValidator.validateBeanAgainstDocument(communication);
				BeanValidator.validateBeanAgainstBizlet(communication);
			} catch (@SuppressWarnings("unused") ValidationException veT) {
				e.getMessages().add(new Message(
						"The Communication " + communication.getSubject()
								+ " is not valid - ensure the Communication is valid before exporting"));
			}
		}

		if (e.getMessages().size() > 0) {
			throw e;
		}
	}

	/**
	 * Marshall a json version of each Communication
	 * Save to a temporary folder and then zip and provide as a download
	 */
	@Override
	public Download download(CommunicationManagerExtension bean, WebContext webContext) throws Exception {

		Download download = FileUtil.prepareZipDownload(bean.getPathToZip(), CommunicationManagerExtension.getZipName(),
				webContext);

		CommunicationManagerExtension.cleanUpTemporaryFiles();

		return download;
	}
}
