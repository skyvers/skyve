package modules.admin.ControlPanel.actions;

import org.skyve.CORE;
import org.skyve.content.MimeType;
import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.ValidationException;
import org.skyve.impl.sail.recording.SailRecording;
import org.skyve.metadata.controller.Download;
import org.skyve.metadata.controller.DownloadAction;
import org.skyve.util.Util;
import org.skyve.web.WebContext;

import jakarta.annotation.Nullable;
import modules.admin.ControlPanel.ControlPanelExtension;

/**
 * Downloads the script of this session's stopped SAIL recording as XML. The recording stays in the session, so it can
 * be downloaded again until the next recording starts or the session ends.
 */
public class DownloadSAILRecording extends DownloadAction<ControlPanelExtension> {
	/**
	 * Checks that there is a script to download.
	 *
	 * @throws ValidationException if the session has no stopped recording, or the user may not record
	 */
	@Override
	public void prepare(ControlPanelExtension bean, WebContext webContext) {
		bean.setTabIndex(null);
		if (stoppedRecording() == null) {
			throw new ValidationException(new Message(Util.nullSafeI18n("admin.controlPanel.sailRecording.notStopped")));
		}
	}

	/**
	 * Returns the script, named for when the recording started.
	 */
	@Override
	public Download download(ControlPanelExtension bean, WebContext webContext) {
		SailRecording recording = stoppedRecording();
		String xml = (recording == null) ? null : recording.getXml();
		if ((recording == null) || (xml == null)) {
			throw new ValidationException(new Message(Util.nullSafeI18n("admin.controlPanel.sailRecording.notStopped")));
		}
		return new Download(recording.fileName(), xml, MimeType.xml);
	}

	private static @Nullable SailRecording stoppedRecording() {
		if (! SailRecording.isAllowed(CORE.getUser())) {
			return null;
		}
		SailRecording result = SailRecording.get(ControlPanelExtension.session());
		return ((result == null) || (result.getXml() == null)) ? null : result;
	}
}
