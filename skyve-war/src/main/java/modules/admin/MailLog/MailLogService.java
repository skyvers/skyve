package modules.admin.MailLog;

import java.util.Optional;

import org.skyve.impl.archive.support.ArchiveRetriever;
import org.skyve.impl.util.UtilImpl.ArchiveConfig.ArchiveDocConfig;
import org.skyve.util.Util;

import modules.admin.domain.MailLog;

/**
 * Service for archive-related MailLog operations.
 */
public class MailLogService {
	private ArchiveRetriever retriever = ArchiveRetriever.getInstance();

	/**
	 * Retrieve the requested MailLog from configured archives, if present.
	 */
	public MailLog retrieveFromArchives(String bizId) {
		Optional<ArchiveDocConfig> config = mailLogDocConfig();
		if (config.isEmpty()) {
			return null;
		}

		return retriever.<MailLog>retrieveByBizId(config.get(), bizId)
						.orElse(null);
	}

	/**
	 * Find MailLog archive configuration, if configured.
	 */
	public Optional<ArchiveDocConfig> mailLogDocConfig() {
		return Util.getArchiveConfig()
					.findArchiveDocConfig(MailLog.MODULE_NAME, MailLog.DOCUMENT_NAME);
	}
}
