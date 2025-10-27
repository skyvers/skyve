package modules.admin.ReportManager;

import java.text.SimpleDateFormat;

import org.skyve.content.MimeType;
import org.skyve.domain.types.DateTime;

public class ReportManagerUtil {
	public static final String REPORTS_BATCH_PREFIX = "reportManager_";
	public static final String ZIP_NAME_FORMAT = "yyyyMMddHHmmss";
	
	/**
	 * Construct a useful name for the download zip file
	 * 
	 * @return A timestamped zip file name as a String
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
