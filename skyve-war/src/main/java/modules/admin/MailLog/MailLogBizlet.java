package modules.admin.MailLog;

import org.skyve.domain.Bean;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.web.WebContext;

import jakarta.inject.Inject;
import modules.admin.domain.MailLog;

public class MailLogBizlet extends Bizlet<MailLog> {
	@Inject
	private transient MailLogService mailLogService;

	@Override
	public MailLog resolve(String bizId, Bean conversationBean, WebContext webContext) {
		MailLog result = mailLogService.retrieveFromArchives(bizId);
		if (result == null) {
			LOGGER.trace("Unable to resolve MailLog {} from archives", bizId);
		}
		else {
			LOGGER.trace("Resolved {} for {} from archives", result, bizId);
		}
		return result;
	}
}
