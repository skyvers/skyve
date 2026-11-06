package modules.kitchensink.ActionsFixture.actions;

import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.UploadException;
import org.skyve.domain.messages.UploadException.Problem;
import org.skyve.domain.messages.ValidationException;
import org.skyve.metadata.controller.Upload;
import org.skyve.metadata.controller.UploadAction;
import org.skyve.web.WebContext;

import modules.kitchensink.domain.ActionsFixture;

/**
 * Records that it ran, which is all the actions fixture needs to put a condition on an upload action.
 * It refuses a file named {@value #REFUSED_FILE_NAME} with an {@link UploadException}, as an application refuses a file
 * that already exists (as admin's Upload Backup does), and one named {@value #INVALID_FILE_NAME} with a
 * {@link ValidationException} (as some applications' upload actions do), so that SAIL can test an upload that is refused
 * both ways.
 */
public class ActionsUpload extends UploadAction<ActionsFixture> {
	/** The name of the file that this action refuses. */
	public static final String REFUSED_FILE_NAME = "sail-refused.txt";
	/** The name of the file that this action refuses with a validation. */
	public static final String INVALID_FILE_NAME = "sail-invalid.txt";

	/**
	 * Records in the outcome that the upload ran, or refuses a file named {@value #REFUSED_FILE_NAME} or
	 * {@value #INVALID_FILE_NAME}.
	 *
	 * @param bean	The fixture.
	 * @param upload	The uploaded file.
	 * @param exception	The problems of the upload.
	 * @param webContext	The current web context.
	 * @return	The fixture.
	 * @throws UploadException	if the file is named {@value #REFUSED_FILE_NAME}; the outcome is left as it was
	 * @throws ValidationException	if the file is named {@value #INVALID_FILE_NAME}; the outcome is left as it was
	 */
	@Override
	public ActionsFixture upload(ActionsFixture bean, Upload upload, UploadException exception, WebContext webContext) {
		if (REFUSED_FILE_NAME.equals(upload.getFileName())) {
			exception.addError(new Problem("The file " + REFUSED_FILE_NAME + " already exists", null));
			throw exception;
		}
		if (INVALID_FILE_NAME.equals(upload.getFileName())) {
			throw new ValidationException(new Message("The file " + INVALID_FILE_NAME + " is not a valid upload"));
		}
		bean.setOutcome("Actions Upload ran");
		return bean;
	}
}
