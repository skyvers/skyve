package modules.admin.ReportManager;

import java.io.File;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.UUID;

import org.skyve.CORE;
import org.skyve.content.MimeType;
import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.ValidationException;
import org.skyve.domain.types.DateTime;
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
	private static final String REPORTS_BATCH_PREFIX = "reportManager_";
	private static final String ZIP_NAME_FORMAT = "yyyyMMddHHmmss";
	private static final Logger LOGGER = LoggerFactory.getLogger(ReportManagerService.class);

	/**
	 * Construct the name of the report management folder
	 * 
	 * @return
	 */
	public static String getBasePath() {
		StringBuilder sb = new StringBuilder();
		sb.append(Util.getContentDirectory());
		sb.append(REPORTS_BATCH_PREFIX).append(CORE.getUser().getCustomerName());
		return sb.toString();
	}

	/**
	 * Construct a safe name for a temporary folder for preparation of exports and imports
	 * 
	 * @return the folder
	 */
	@SuppressWarnings("static-method")
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
	 * Construct a useful name for the download zip file
	 */
	@SuppressWarnings("static-method")
	public String getZipName() {
		SimpleDateFormat sdf = new SimpleDateFormat(ZIP_NAME_FORMAT);
		StringBuilder zipName = new StringBuilder();
		zipName.append(REPORTS_BATCH_PREFIX)
				.append(sdf.format(new DateTime()))
				.append(".")
				.append(MimeType.zip.getStandardFileSuffix());
		return zipName.toString();
	}

	/**
	 * Construct a safe zip File for preparation
	 * 
	 * @return
	 */
	public File getZipFile() {
		StringBuilder zipPath = new StringBuilder(getBasePath());
		zipPath.append(File.separator).append(getZipName());
		return new File(zipPath.toString());
	}

	/**
	 * delete temporary files and folders
	 */
	@SuppressWarnings("static-method")
	public void cleanUpTemporaryFiles() {
		// clean up temporary folder
		try {
			FileUtil.delete(new File(getBasePath()));
		} catch (IOException e) {
			LOGGER.info("FAILED to clean up temporary files at {}", getBasePath(), e);
		}
	}
}
