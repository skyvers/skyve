package modules.admin.ControlPanel.actions;

import org.skyve.CORE;
import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.MessageSeverity;
import org.skyve.domain.messages.ValidationException;
import org.skyve.impl.sail.recording.SailRecording;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.util.Util;
import org.skyve.web.WebContext;

import modules.admin.ControlPanel.ControlPanelExtension;

/**
 * Stops this session's SAIL recording and builds its script, which {@link DownloadSAILRecording} then downloads.
 * The browser recorder sends its last steps before this action reaches the server.
 */
public class StopSAILRecording implements ServerSideAction<ControlPanelExtension> {
	/**
	 * Stops the recording.
	 *
	 * <p>Side effects: the session stops recording; its pages no longer load the recorder.
	 *
	 * @param bean	the Control Panel
	 * @param webContext	the web context
	 * @return the Control Panel
	 * @throws ValidationException if the session has no recording, or the user may not record
	 */
	@Override
	public ServerSideActionResult<ControlPanelExtension> execute(ControlPanelExtension bean, WebContext webContext) {
		bean.setTabIndex(null);
		SailRecording recording = SailRecording.isAllowed(CORE.getUser()) ? SailRecording.get(ControlPanelExtension.session()) : null;
		if (recording == null) {
			throw new ValidationException(new Message(Util.nullSafeI18n("admin.controlPanel.sailRecording.none")));
		}
		recording.stop();
		webContext.growl(MessageSeverity.info, Util.nullSafeI18n("admin.controlPanel.sailRecording.stopped", String.valueOf(recording.getStepCount())));
		return new ServerSideActionResult<>(bean);
	}
}
