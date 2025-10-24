package modules.admin.ReportManager;

import java.io.File;
import java.io.FileOutputStream;
import java.io.OutputStream;
import java.nio.charset.Charset;

import org.skyve.CORE;
import org.skyve.content.MimeType;
import org.skyve.domain.messages.ValidationException;
import org.skyve.util.FileUtil;
import org.skyve.util.JSON;

import jakarta.inject.Inject;
import modules.admin.domain.ReportManager;
import modules.admin.domain.ReportTemplate;

public class ReportManagerExtension extends ReportManager {
	@Inject
	private transient ReportManagerService reportManagerService;
	private static final long serialVersionUID = 4946402450868738555L;

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
			reportManagerService.cleanUpTemporaryFiles();
			throw new ValidationException(String.format("The report %s could not be written.", name));
		}

	}
}
