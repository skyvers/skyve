package modules.admin.ControlPanel.actions;

import org.primefaces.PrimeFaces;
import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.MessageSeverity;
import org.skyve.domain.messages.ValidationException;
import org.skyve.impl.sail.recording.SailRecording;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.RequestUxUiSelection;
import org.skyve.impl.web.UserAgent;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.metadata.user.User;
import org.skyve.util.Util;
import org.skyve.web.WebContext;

import jakarta.faces.context.FacesContext;
import jakarta.servlet.http.HttpServletRequest;
import modules.admin.ControlPanel.ControlPanelExtension;

/**
 * Starts recording this session as a SAIL script, replacing any earlier recording of the session.
 *
 * <p>In SmartClient, the Control Panel's view then loads the recorder into the page without a reload; in PrimeFaces,
 * which loads the recorder with each page, the page is loaded again. Every page of the session loads it until
 * recording stops; see docs/sail.md, "Recording". In production (no environment identifier) the user is
 * warned that the script will hold the data they see and type.
 */
public class StartSAILRecording implements ServerSideAction<ControlPanelExtension> {
	/**
	 * Reloads a PrimeFaces page, marking the load as one the recorder does not record (see recorder-pf.js).
	 */
	static final String RELOAD_SCRIPT = "try{sessionStorage.setItem('skyve.sail.recorder.navigating','reload');}catch(e){}location.reload();";

	/**
	 * Starts the recording.
	 *
	 * <p>Side effects: sets the session's recording attribute.
	 *
	 * @param bean	the Control Panel
	 * @param webContext	the web context
	 * @return the Control Panel
	 * @throws ValidationException if the configuration or the user's roles do not allow recording
	 */
	@Override
	public ServerSideActionResult<ControlPanelExtension> execute(ControlPanelExtension bean, WebContext webContext) {
		bean.setTabIndex(null);
		User user = CORE.getUser();
		if (! SailRecording.isAllowed(user)) {
			throw new ValidationException(new Message(Util.nullSafeI18n("admin.controlPanel.sailRecording.notAllowed")));
		}
		HttpServletRequest request = EXT.getHttpServletRequest();
		RequestUxUiSelection selection = UserAgent.getSelection(request);
		SailRecording.start(request.getSession(), user, selection.getUxUi().getName(), selection.getUserAgentType());
		// A PrimeFaces page loads the recorder only with the page: load this one again, without recording the load
		if (FacesContext.getCurrentInstance() != null) {
			PrimeFaces.current().executeScript(RELOAD_SCRIPT);
		}
		if ((UtilImpl.ENVIRONMENT_IDENTIFIER == null) && (webContext != null)) {
			webContext.message(MessageSeverity.warn, Util.nullSafeI18n("admin.controlPanel.sailRecording.production"));
		}
		return new ServerSideActionResult<>(bean);
	}
}
