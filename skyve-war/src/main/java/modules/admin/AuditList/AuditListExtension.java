package modules.admin.AuditList;

import java.util.Optional;

import org.skyve.impl.util.UtilImpl.ArchiveConfig.ArchiveDocConfig;
import org.skyve.util.Util;

import modules.admin.domain.Audit;
import modules.admin.domain.AuditList;

public class AuditListExtension extends AuditList {

    @Override
    public boolean isShowArchived() {

        return auditDocConfig().isPresent();
    }

    private Optional<ArchiveDocConfig> auditDocConfig() {

        return Util.getArchiveConfig()
                   .findArchiveDocConfig(Audit.MODULE_NAME, Audit.DOCUMENT_NAME);
    }
}
