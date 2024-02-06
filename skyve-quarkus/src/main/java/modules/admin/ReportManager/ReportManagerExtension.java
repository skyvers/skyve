package modules.admin.ReportManager;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.nio.charset.Charset;
import java.text.SimpleDateFormat;
import java.util.UUID;

import org.skyve.CORE;
import org.skyve.content.MimeType;
import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.ValidationException;
import org.skyve.domain.types.DateTime;
import org.skyve.util.FileUtil;
import org.skyve.util.JSON;
import org.skyve.util.Util;

import modules.admin.domain.ReportManager;
import modules.admin.domain.ReportTemplate;

public class ReportManagerExtension extends ReportManager {

	/**
	 * 
	 */
	private static final long serialVersionUID = 4946402450868738555L;

	private static final String REPORTS_BATCH_PREFIX = "reportManager_";
	private static final String ZIP_NAME_FORMAT = "yyyyMMddHHmmss";

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
	public static File getTemporaryPreparationFolder() {

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
	public static String getZipName() {
		SimpleDateFormat sdf = new SimpleDateFormat(ZIP_NAME_FORMAT);
		StringBuilder zipName = new StringBuilder();
		zipName.append(REPORTS_BATCH_PREFIX).append(sdf.format(new DateTime())).append(".").append(MimeType.zip.getStandardFileSuffix());
		return zipName.toString();
	}

	/**
	 * Construct a safe zip File for preparation
	 * 
	 * @return
	 */
	public static File getZipFile() {
		StringBuilder zipPath = new StringBuilder(getBasePath());
		zipPath.append(File.separator).append(getZipName());
		return new File(zipPath.toString());
	}

	/**
	 * delete temporary files and folders
	 */
	public static void cleanUpTemporaryFiles() {
		// clean up temporary folder
		try {
			FileUtil.delete(new File(getBasePath()));
		} catch (IOException e) {
			Util.LOGGER.info("FAILED to clean up temporary files at " + getBasePath());
			e.printStackTrace();
		}
	}

	public void marshallReportBean(final ReportTemplate report, final String name) {
		// marshall each report to a JSON string
		String json = JSON.marshall(CORE.getCustomer(), report);
		byte[] jsonBytes = json.toString().getBytes(Charset.forName("UTF-8"));

		// save the json string to a file
		StringBuilder reportFileName = new StringBuilder(getPathToZip());
		reportFileName.append(File.separator).append(FileUtil.safeFileName(name)).append(".").append(MimeType.json.getStandardFileSuffix());

		File targetFile = new File(reportFileName.toString());
		try (OutputStream outStream = new FileOutputStream(targetFile)) {

			outStream.write(jsonBytes);

		} catch (Exception e) {
			e.printStackTrace();
			cleanUpTemporaryFiles();
			throw new ValidationException(String.format("The report %s could not be written.", name));
		}

	}
}
