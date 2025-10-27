package modules.admin.ReportManager;

import java.text.SimpleDateFormat;

import org.skyve.content.MimeType;
import org.skyve.domain.types.DateTime;

/**
 * Utility class for Report Manager functionality.
 * 
 * <p>
 * This class provides common utilities and constants for managing reports.
 * </p>
 * 
 */
public class ReportManagerUtil {

	/**
	 * Standard prefix used for report batch file names.
	 */
	public static final String REPORTS_BATCH_PREFIX = "reportManager_";

	/**
	 * Date format pattern used for generating timestamped file names.
	 * 
	 * <p>
	 * Format: yyyyMMddHHmmss (Year, Month, Day, Hour, Minute, Second)
	 * Example: 20251027143022 for October 27, 2025 at 14:30:22
	 * </p>
	 */
	public static final String ZIP_NAME_FORMAT = "yyyyMMddHHmmss";

	/**
	 * Constructs a timestamped zip file name for report downloads.
	 * 
	 * <p>
	 * Generates a unique file name using the current date and time. The format follows the pattern:
	 * {@code reportManager_yyyyMMddHHmmss.zip}
	 * </p>
	 * 
	 * <p>
	 * <strong>Example:</strong> {@code reportManager_20251027143022.zip}
	 * </p>
	 * 
	 * @return A timestamped zip file name as a String, including the .zip extension
	 */
	public static String getZipName() {
		SimpleDateFormat sdf = new SimpleDateFormat(ZIP_NAME_FORMAT);
		StringBuilder zipName = new StringBuilder();
		zipName.append(REPORTS_BATCH_PREFIX)
				.append(sdf.format(new DateTime()))
				.append(".")
				.append(MimeType.zip.getStandardFileSuffix());
		return zipName.toString();
	}
}
