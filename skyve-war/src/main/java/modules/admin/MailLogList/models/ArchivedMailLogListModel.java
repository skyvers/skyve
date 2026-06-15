package modules.admin.MailLogList.models;

import org.skyve.domain.Bean;

/**
 * Admin MailLogList-facing alias for the archived mail log list model.
 * <p>
 * This wrapper keeps model wiring under the MailLogList namespace while reusing
 * the shared archived Mail Log list implementation.
 * </p>
 *
 * @param <U> The bean type.
 */
@SuppressWarnings("java:S2176") // Metadata model aliases intentionally mirror the shared admin model names.
public class ArchivedMailLogListModel<U extends Bean> extends modules.admin.MailLog.models.ArchivedMailLogListModel<U> {
	//
}
