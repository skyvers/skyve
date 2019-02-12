package org.skyve.impl.web.faces;

import java.util.List;
import java.util.Map;
import java.util.UUID;

import javax.faces.application.FacesMessage;
import javax.faces.application.FacesMessage.Severity;
import javax.faces.context.FacesContext;

import org.skyve.domain.messages.MessageSeverity;
import org.skyve.impl.web.AbstractWebContext;

public class FacesWebContext extends AbstractWebContext {
	private static final long serialVersionUID = -1539528185277420146L;

	public FacesWebContext() {
		super(UUID.randomUUID().toString(),
				FacesContext.getCurrentInstance().getExternalContext().getRequest(),
				FacesContext.getCurrentInstance().getExternalContext().getResponse());
	}

	@Override
	public void growl(MessageSeverity severity, String message) {
		message(severity, message);
	}

	@Override
	public void message(MessageSeverity severity, String message) {
		FacesContext.getCurrentInstance().addMessage(null, new FacesMessage(determineFacesSeverity(severity),
																				message,
																				message));
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
