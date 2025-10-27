package modules.admin.ReportManager;

import java.io.File;
import java.io.IOException;
import java.util.UUID;

import org.skyve.CORE;
import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.ValidationException;
import org.skyve.util.FileUtil;
import org.skyve.util.Util;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import jakarta.enterprise.inject.Default;

/**
 * This class acts as a service layer to encapsulate domain logic.
 *
 * Add this line to classes that wish to use it: @Inject private transient ReportManagerService reportManagerService;
 */
@Default
public class ReportManagerService {
	private static final Logger LOGGER = LoggerFactory.getLogger(ReportManagerService.class);

	/**
	 * Construct the name of the report management folder
	 * 
	 * @return The base path for report management operations as a String
	 */
	@SuppressWarnings("static-method")
	public String getBasePath() {
		StringBuilder sb = new StringBuilder();
		sb.append(Util.getContentDirectory());
		sb.append(ReportManagerUtil.REPORTS_BATCH_PREFIX).append(CORE.getUser().getCustomerName());
		return sb.toString();
	}

	/**
	 * Construct a safe name for a temporary folder for preparation of exports and imports
	 * 
	 * @return A File object representing the created temporary preparation folder
	 */
	public File getTemporaryPreparationFolder() {

		// make a temporary directory for the JSON files to be zipped
		StringBuilder sb = new StringBuilder(getBasePath());
		sb.append(File.separator).append(UUID.randomUUID());

		File temp = new File(sb.toString());
		try {
			temp.mkdirs();
		} catch (SecurityException e) {
			e.printStackTrace();
			throw new ValidationException(new Message("Export file preparation failed"));
		}
		return temp;
	}

	/**
	 * Construct a safe zip File for preparation
	 * 
	 * @return A File object representing the zip file location
	 */
	public File getZipFile() {
		StringBuilder zipPath = new StringBuilder(getBasePath());
		zipPath.append(File.separator).append(ReportManagerUtil.getZipName());
		return new File(zipPath.toString());
	}

	/**
	 * delete temporary files and folders
	 */
	public void cleanUpTemporaryFiles() {
		// clean up temporary folder
		try {
			FileUtil.delete(new File(getBasePath()));
		} catch (IOException e) {
			LOGGER.warn("FAILED to clean up temporary files at {}", getBasePath(), e);
		}
	}
}
