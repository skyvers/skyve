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

/**
 * Restores persisted data from a selected backup according to maintenance options.
 */
public class Restore implements ServerSideAction<DataMaintenance> {
	/**
	 * Performs the execute operation.
	 * @param bean the bean value
	 * @param webContext the webContext value
	 * @return the operation result
	 * @throws Exception if the operation fails
	 */
	@Override
	public ServerSideActionResult<DataMaintenance> execute(DataMaintenance bean, WebContext webContext)
			throws Exception {
		if (bean.getContentRestoreOption() == null) {
			String msg = i18n("admin.dataMaintenance.actions.restore.selectContentRestoreOptionException",
					attributeDisplayName(bean, DataMaintenance.contentRestoreOptionPropertyName));
			throw new ValidationException(DataMaintenance.contentRestoreOptionPropertyName, msg);
		}
		if (bean.getRestoreIndexingOption() == null) {
			String msg = i18n("admin.dataMaintenance.actions.restore.selectRestoreIndexingOptionException",
					attributeDisplayName(bean, DataMaintenance.restoreIndexingOptionPropertyName));
			throw new ValidationException(DataMaintenance.restoreIndexingOptionPropertyName, msg);
		}

		if (bean.getRestorePreProcess() == null) {
			String msg = i18n("admin.dataMaintenance.actions.restore.selectPreProcessException",
					attributeDisplayName(bean, DataMaintenance.restorePreProcessPropertyName));
			throw new ValidationException(new Message(DataMaintenance.restorePreProcessPropertyName, msg));
		}

		restore(bean);
		growl(webContext);

		return new ServerSideActionResult<>(bean);
	}

	@SuppressWarnings("static-method") // test seam
	protected String attributeDisplayName(DataMaintenance bean, String attributeName) {
		Document d = bean.getDocumentMetaData();
		@SuppressWarnings("null")
		String desc = d.getAttribute(attributeName).getLocalisedDisplayName();
		return desc;
	}

	@SuppressWarnings("static-method") // test seam
	protected String i18n(String key, String... args) {
		return Util.nullSafeI18n(key, args);
	}

	@SuppressWarnings("static-method") // test seam
	protected void restore(DataMaintenance bean) {
		EXT.getJobScheduler().restore(bean);
	}

	@SuppressWarnings("static-method") // test seam
	protected void growl(WebContext webContext) {
		webContext.growl(MessageSeverity.info, Util.nullSafeI18n("admin.dataMaintenance.actions.restore.restoreJobCommenced"));
	}
}
