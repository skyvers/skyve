package modules.admin.MailLogList;

import jakarta.inject.Inject;
import modules.admin.MailLog.MailLogService;
import modules.admin.domain.MailLogList;

public class MailLogListExtension extends MailLogList {

	private static final long serialVersionUID = 4043844475789272707L;

	@Inject
	private transient MailLogService mailLogService;

	/**
	 * Show the archived mail logs tab if there is a mail log document archive configured in the application's json.
	 */
	@Override
	public boolean isShowArchived() {
		return mailLogService.mailLogDocConfig().isPresent();
	}
}
