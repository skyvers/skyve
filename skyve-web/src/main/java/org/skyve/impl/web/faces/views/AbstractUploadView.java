package org.skyve.impl.web.faces.views;

import java.util.regex.Pattern;

import org.primefaces.model.file.UploadedFile;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.AbstractWebContext;
import org.skyve.metadata.view.TextOutput.Sanitisation;
import org.skyve.util.OWASP;

import jakarta.faces.annotation.ManagedProperty;
import jakarta.faces.application.FacesMessage;
import jakarta.faces.context.FacesContext;
import jakarta.inject.Inject;

public abstract class AbstractUploadView extends LocalisableView {
	private static final long serialVersionUID = 8618349823087627588L;

	private static long MB_IN_BYTES = 1024 * 1024;

	@Inject
	@ManagedProperty(value = "#{param." + AbstractWebContext.CONTEXT_NAME + "}")
	private String context;

	@Inject
	@ManagedProperty(value = "#{param." + AbstractWebContext.BINDING_NAME + "}")
	private String binding;

	private String whitelistRegex;
	private long maximumSizeInBytes;

	protected AbstractUploadView(String whitelistRegex, int maximumSizeMB) {
		this.whitelistRegex = whitelistRegex;
		maximumSizeInBytes = maximumSizeMB * MB_IN_BYTES;
	}

	// Call this from extending views
	protected void postConstruct() {
		context = OWASP.sanitise(Sanitisation.text, UtilImpl.processStringValue(context));
		binding = OWASP.sanitise(Sanitisation.text, UtilImpl.processStringValue(binding));
	}
	
	public String getContext() {
		return context;
	}

	public String getBinding() {
		return binding;
	}

	public String getWhitelistRegex() {
		return whitelistRegex;
	}

	public long getMaximumSizeInBytes() {
		return maximumSizeInBytes;
	}
	
	protected boolean validFile(UploadedFile file, FacesContext fc) {
		long size = file.getSize();
		if (size > maximumSizeInBytes) {
			UtilImpl.LOGGER.warning("FileUpload - File size of " + size + " > maximumSizeInBytes of " + maximumSizeInBytes);
			FacesMessage msg = new FacesMessage("Failure", "File is too large");
			fc.addMessage(null, msg);
			return false;
		}

		String name = file.getFileName();
		// NB PatternSyntaxException is caught in SkyveContextListener at startup
		if (name != null) {
			if (((whitelistRegex != null) && 
					// Check whitelist regex if defined
					(! Pattern.compile(whitelistRegex, Pattern.CASE_INSENSITIVE | Pattern.MULTILINE).matcher(name).matches())) ||
					// Always disallow files starting with .
					name.matches("^((.*\\/)+\\..*|\\..*|(.*\\\\)+\\..*)$")) {
				UtilImpl.LOGGER.warning("FileUpload - Filename " + name + " does not match " + whitelistRegex);
				FacesMessage msg = new FacesMessage("Failure", "Filename " + name + " is not allowed");
				fc.addMessage(null, msg);
				return false;
			}
		}
		
		return true;
	}
}

