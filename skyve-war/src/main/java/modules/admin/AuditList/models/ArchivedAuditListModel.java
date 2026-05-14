package modules.admin.AuditList.models;

import org.skyve.domain.Bean;

/**
 * Admin AuditList-facing alias for the archived audit list model.
 * <p>
 * This wrapper keeps model wiring under the AuditList namespace while reusing the
 * shared archived audit list implementation.
 * </p>
 *
 * @param <U> The bean type.
 */
public class ArchivedAuditListModel<U extends Bean> extends modules.admin.Audit.models.ArchivedAuditListModel<U> {
    //
}
