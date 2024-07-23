package modules.admin.Communication;

import java.io.File;
import java.util.Date;

import org.skyve.CORE;
import org.skyve.util.FileUtil;
import org.skyve.util.Util;

import modules.admin.domain.Communication;

public class CommunicationExtension extends Communication {
	private static final long serialVersionUID = 4916984359227145959L;

	private static final String BATCH_FOLDER_PREFIX = "batch_";
	private static final String BATCH_FORMAT = "yyyyMMddHHmmss";
	private static Date batchDateTime = null;

	@Override
	public String getBasePath() {

		StringBuilder sb = new StringBuilder();

		if (getDescription() != null) {
			String customerName = CORE.getUser().getCustomerName();
			sb.append(Util.getContentDirectory());
			sb.append(BATCH_FOLDER_PREFIX).append(FileUtil.safeFileName(customerName));
			sb.append(File.separator).append(FileUtil.safeFileName(getDescription())).append(File.separator);

			// Util.LOGGER.info("BASE PATH " + sb.toString());
		}

		return sb.toString();
	}

	@Override
	public String getBatch() {
		// cache the datetime that this batch was started so that all emails end up in the same folder
		if (batchDateTime == null) {
			batchDateTime = new Date();
		}
		return CORE.getDateFormat(BATCH_FORMAT).format(batchDateTime);
	}

	@Override
	public void setTemplate(org.skyve.domain.app.admin.CommunicationTemplate template) {
		setTemplate((modules.admin.domain.CommunicationTemplate) template);
	}
}
