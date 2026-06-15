package org.skyve.impl.web.faces;

import java.util.List;
import java.util.Map;
import java.util.UUID;

import org.skyve.domain.messages.MessageSeverity;
import org.skyve.impl.web.ViewWebContext;
import org.skyve.util.Util;

import jakarta.faces.application.FacesMessage;
import jakarta.faces.application.FacesMessage.Severity;
import jakarta.faces.context.FacesContext;
import jakarta.servlet.http.HttpServletRequest;

/**
 * Represents request-scoped context state used by Skyve web processing.
 */
public class FacesWebContext extends ViewWebContext {
	private static final long serialVersionUID = -1539528185277420146L;

	/**
	 * Creates a faces-backed web context bound to the current HTTP request.
	 */
	public FacesWebContext() {
		super(UUID.randomUUID().toString(),
				(HttpServletRequest) FacesContext.getCurrentInstance().getExternalContext().getRequest());
	}

	/**
	 * Adds a growl-level message to the current faces context.
	 *
	 * @param severity the message severity
	 * @param message the message text key or literal
	 */
	@Override
	public void growl(MessageSeverity severity, String message) {
		message(severity, message);
	}

	/**
	 * Adds a standard faces message translated from Skyve severity.
	 *
	 * @param severity the message severity
	 * @param message the message text key or literal
	 */
	@Override
	public void message(MessageSeverity severity, String message) {
		String i18n = Util.nullSafeI18n(message);
		FacesContext.getCurrentInstance().addMessage(null, new FacesMessage(determineFacesSeverity(severity), i18n, i18n));
	}
	
	/**
	 * Maps a Skyve message severity to the equivalent JSF {@link FacesMessage} severity.
	 *
	 * @param severity the Skyve message severity to map
	 * @return the corresponding JSF message severity
	 */
	private static Severity determineFacesSeverity(MessageSeverity severity) {
		Severity result = null;
		switch (severity) {
			case info:
				result = FacesMessage.SEVERITY_INFO;
				break;
			case warn:
				result = FacesMessage.SEVERITY_WARN;
				break;
			case error:
				result = FacesMessage.SEVERITY_ERROR;
				break;
			case fatal:
				result = FacesMessage.SEVERITY_FATAL;
				break;
			default:
		}
		return result;
	}
	
	/**
	 * Returns no explicit growl cache because faces messages are pushed directly to the context.
	 *
	 * @return always {@code null}
	 */
	@Override
	public List<Map<String, String>> getGrowls() {
		return null;
	}
	
	/**
	 * Returns no explicit message cache because faces messages are pushed directly to the context.
	 *
	 * @return always {@code null}
	 */
	@Override
	public List<Map<String, String>> getMessages() {
		return null;
	}
}
