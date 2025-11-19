package modules.admin.DataMaintenance.actions;

import org.skyve.EXT;
import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.MessageSeverity;
import org.skyve.domain.messages.ValidationException;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.metadata.model.document.Document;
import org.skyve.util.Util;
import org.skyve.web.WebContext;

import modules.admin.domain.DataMaintenance;

public class Restore implements ServerSideAction<DataMaintenance> {
	@Override
	public ServerSideActionResult<DataMaintenance> execute(DataMaintenance bean, WebContext webContext)
			throws Exception {
		if (bean.getContentRestoreOption() == null) {
			Document d = bean.getDocumentMetaData();
			@SuppressWarnings("null")
			String desc = d.getAttribute(DataMaintenance.contentRestoreOptionPropertyName).getLocalisedDisplayName();
			String msg = Util.nullSafeI18n("admin.dataMaintenance.actions.restore.selectContentRestoreOptionException", desc);
			throw new ValidationException(DataMaintenance.contentRestoreOptionPropertyName, msg);
		}
		if (bean.getRestoreIndexingOption() == null) {
			Document d = bean.getDocumentMetaData();
			@SuppressWarnings("null")
			String desc = d.getAttribute(DataMaintenance.restoreIndexingOptionPropertyName).getLocalisedDisplayName();
			String msg = Util.nullSafeI18n("admin.dataMaintenance.actions.restore.selectRestoreIndexingOptionException", desc);
			throw new ValidationException(DataMaintenance.restoreIndexingOptionPropertyName, msg);
		}

		if (bean.getRestorePreProcess() == null) {
			Document d = bean.getDocumentMetaData();
			@SuppressWarnings("null")
			String desc = d.getAttribute(DataMaintenance.restorePreProcessPropertyName).getLocalisedDisplayName();
			String msg = Util.nullSafeI18n("admin.dataMaintenance.actions.restore.selectPreProcessException", desc);
			throw new ValidationException(new Message(DataMaintenance.restorePreProcessPropertyName, msg));
		}

		EXT.getJobScheduler().restore(bean);
		webContext.growl(MessageSeverity.info, Util.nullSafeI18n("admin.dataMaintenance.actions.restore.restoreJobCommenced"));

		return new ServerSideActionResult<>(bean);
	}
}
