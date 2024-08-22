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

public class FacesWebContext extends ViewWebContext {
	private static final long serialVersionUID = -1539528185277420146L;

	public FacesWebContext() {
		super(UUID.randomUUID().toString(),
				(HttpServletRequest) FacesContext.getCurrentInstance().getExternalContext().getRequest());
	}

	@Override
	public void growl(MessageSeverity severity, String message) {
		message(severity, message);
	}

	@Override
	public void message(MessageSeverity severity, String message) {
		String i18n = Util.i18n(message);
		FacesContext.getCurrentInstance().addMessage(null, new FacesMessage(determineFacesSeverity(severity), i18n, i18n));
	}
	
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
	
	@Override
	public List<Map<String, String>> getGrowls() {
		return null;
	}
	
	@Override
	public List<Map<String, String>> getMessages() {
		return null;
	}
}
