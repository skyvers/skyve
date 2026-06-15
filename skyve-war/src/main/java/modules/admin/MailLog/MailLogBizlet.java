package modules.admin.MailLog;

import org.skyve.domain.Bean;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.web.WebContext;

import jakarta.inject.Inject;
import modules.admin.domain.MailLog;

/**
 * Resolves admin mail log records from archive storage for list and detail views.
 */
public class MailLogBizlet extends Bizlet<MailLog> {
	@Inject
	@SuppressWarnings("java:S6813") // allow member injection
	private transient MailLogService mailLogService;

	/**
	 * Resolves a mail log by business identifier from configured archive storage.
	 *
	 * @param bizId the identifier of the mail log to resolve
	 * @param conversationBean the parent conversation bean context
	 * @param webContext the current web request context
	 * @return the archived mail log, or {@code null} when it is not available
	 */
	@Override
	public MailLog resolve(String bizId, Bean conversationBean, WebContext webContext) {
		return mailLogService.retrieveFromArchives(bizId);
	}
}
